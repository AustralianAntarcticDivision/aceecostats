library(raadtools)
library(roc)
files <- chla_johnsonfiles(product = "MODISA")
files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))

library(raster)
library(aceecostats)
library(tibble)
library(dplyr)
library(feather)
library(sf)
library(purrr)
outf <- "/mnt/acebulk"
db <- dplyr::src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
## put a tidy end to the series
maxdate <- ISOdatetime(2016, 12, 31, 23, 59, 59, tz = "GMT")
aes_zone_data <- aes_zone@data[, c("ID", "SectorName", "Zone")]
# ## here Continent just means High Latitude
## we trick it here so the ID overlay gets bundled together below
aes_zone_data$Zone[aes_zone_data$Zone == "Continent"] <- "High-Latitude"

ucell <- tbl(db, "modis_bins") %>% collect(n = Inf)

## main loop over all season-years
usegs <- distinct(files, season_segs)
sparkline_list <- vector("list", nrow(usegs))

## loop over season-years
for (i in seq_along(sparkline_list)) {
  #asub <- which(segs == unique(segs)[i])
  afiles <-  usegs[i, ] %>% inner_join(files)
  #a_obj <- setZ(readAll(subset(obj, asub)), getZ(obj)[asub])
  
  meanval <- lapply(afiles$fullname, readRDS) %>% bind_rows() %>% 
    group_by(bin_num) %>% 
    summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa))
  ## aggregate min/max for 90 days per cell
  #  meanval_map <- mean(a_obj, na.rm = TRUE)
  start_date <- min(afiles$date)
  ## mean of the min/max by region
  sparkys <-  meanval %>% 
    inner_join(ucell %>% inner_join(aes_zone_data), c("bin_num" = "cell_")) %>% 
    group_by(SectorName, Zone) %>% 
    # summarize(meanmin = mean(minval), meanmax = mean(maxval)) %>% 
    summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa)) %>% 
    mutate(season_year = start_date) %>% ungroup()
  sparkline_list[[i]] <- sparkys
  print(i)
}  

sp_line <- bind_rows(sparkline_list) %>% mutate(season = aes_season(season_year))
#db$con %>% db_drop_table(table='chl_sparkline_tab')
dplyr::copy_to(db, sp_line, "chl_sparkline_tab", temporary = FALSE)
#saveRDS(sp_line, file.path(outf, "sparky_line.rds"))
