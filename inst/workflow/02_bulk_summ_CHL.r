decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2017), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}

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


obj <- brick(file.path(outf, sprintf("%s.grd", "chl")))
ras <- raster(obj)
gridarea <- area(ras) * if (isLonLat(ras)) 1 else (1/1e6)

## unique integer from 0 to ~nrow(sf)/90 for each three month period
segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))


## unique grid map cell number
ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))

## classify cell index by polygon
ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
                 aes_zone)$ID

## main loop over all season-years
sparkline_list <- vector("list", length(unique(segs)))
## loop over season-years
for (i in seq_along(sparkline_list)) {
  asub <- which(segs == unique(segs)[i])
  a_obj <- setZ(readAll(subset(obj, asub)), getZ(obj)[asub])
  
 
  
  ## aggregate min/max for 90 days per cell
 # minval_map <- min(a_obj, na.rm = TRUE)
 #  maxval_map <- max(a_obj, na.rm = TRUE)
  meanval_map <- mean(a_obj, na.rm = TRUE)
  start_date <- as.POSIXct(getZ(a_obj)[1], tz = "GMT")
  rm(a_obj)
  gc()
  ## mean of the min/max by region
  sparkys <-  tabit(meanval_map) %>% 
   # inner_join(tabit(minval_map) %>% rename(minval = val)) %>% 
    inner_join(ucell %>% inner_join(aes_zone_data)) %>% 
    group_by(SectorName, Zone) %>% 
   # summarize(meanmin = mean(minval), meanmax = mean(maxval)) %>% 
    summarize(mean = mean(val)) %>% 
    mutate(season_year = start_date)
  sparkline_list[[i]] <- sparkys
  print(i)
}  

sp_line <- bind_rows(sparkline_list) %>% mutate(season = aes_season(season_year))
#db$con %>% db_drop_table(table='chl_sparkline_tab')
dplyr::copy_to(db, sp_line, "chl_sparkline_tab", temporary = FALSE)
#saveRDS(sp_line, file.path(outf, "sparky_line.rds"))

## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(segs)))

decade_labels <- decade_maker(as.Date(getZ(obj)))
season_labels <- aes_season(as.Date(getZ(obj)))
interval_labels <- segs
udecades <- levels(decade_labels)[3:4]
useasons <- c("Summer", "Winter")
## this loop 10 summers in a decade

alldays <- tibble(date = as.POSIXct(getZ(obj), tz = "GMT"), decade = decade_maker(date), season = aes_season(date), 
                  season_year = segs)


big_tab <- vector("list")
icount <- 0
idecade <- 1
iseason <- 1


for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(useasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
    #decade_years <- this_decade_days %>% split(.$season_year)
    #list_of_seasons <- vector("list", length(decade_years))
    a_obj <- readAll(subset(obj, which(segs %in% this_decade_days$season_year)))
    meanval_map <- mean(a_obj, na.rm = TRUE)
    tab <- tabit(meanval_map) 
    tab <- tab %>% mutate(date = this_decade_days$date[1]) 
    tab$count <- values(calc(!is.na(a_obj), sum, na.rm = TRUE))[tab$cell_]
    
    ## big tab (need labels back on decade)
    tab <- tab %>%  mutate(decade = decade_maker(date), season = useasons[iseason])
    
    icount <- icount + 1
    big_tab[[icount]] <- tab
    rm(tab, a_obj)
    gc()
  }
}

big_tab <- bind_rows(big_tab)
big_tab$area <- raster::extract(gridarea, big_tab$cell_)

big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)

#db$con %>% db_drop_table(table='chl_density_tab')

copy_to(db, big_tab, "chl_density_tab", temporary = FALSE)
