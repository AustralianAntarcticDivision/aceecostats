decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2017), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}
#aes_decades <- seq(as.POSIXct("1977-01-01"), length = 5, by = "10 years")
#devtools::use_data(aes_decades, overwrite = TRUE)

library(raster)
library(aceecostats)
library(tibble)
library(dplyr)
library(feather)
library(sf)
outf <- "/mnt/acebulk"

## put a tidy end to the series
maxdate <- ISOdatetime(2016, 12, 31, 23, 59, 59, tz = "GMT")
aes_zone_data <- aes_zone@data[, c("ID", "SectorName", "Zone")]
# ## here Continent just means High Latitude
## we trick it here so the ID overlay gets bundled together below
aes_zone_data$Zone[aes_zone_data$Zone == "Continent"] <- "High-Latitude"


vars <- c("sst", "chl")
for (ivar in seq_along(vars)) {

  obj <- brick(file.path(outf, sprintf("%s.grd", vars[ivar])))
  ras <- raster(obj)
#  pp <- spex::qm_rasterToPolygons(ras)
#  pp <- st_transform(st_set_crs(pp, st_crs(projection(ras))), st_crs(proj4string(aes_zone)))
#  gridarea <- setValues(ras, st_area(pp))
  gridarea <- area(ras)
  ## this is now a known area from a non-equal grid
  
  ## unique integer from 0 to ~nrow(sf)/90 for each three month period
  segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))
  
  cell_tab <- vector("list", length(unique(segs)))
  dates <- as.POSIXct(getZ(obj))
  
  for (i in seq_along(cell_tab)) {
    asub <- which(segs == unique(segs)[i])
    a_obj <- setZ(readAll(subset(obj, asub)), dates[asub])
    tab <- tabit(min(a_obj, na.rm = TRUE)) 
    tab <- tab %>% rename(min = val) %>% mutate(date = dates[asub[1]]) 
    #%>% 
    #  filter(min > 0)
    tab$max<- values(max(a_obj, na.rm = TRUE))[tab$cell_]
    tab$mean <- values(mean(a_obj, na.rm = TRUE))[tab$cell_]
    tab$count <- values(calc(a_obj > 0, sum, na.rm = TRUE))[tab$cell_]
    cell_tab[[i]] <- tab
    print(i)
    rm(tab, a_obj)
    gc()
  }
  
  cell_tab <- bind_rows(cell_tab) %>% 
    mutate(decade = decade_maker(date)) %>% 
    filter(date <  maxdate) %>% 
    filter(!is.na(decade))
  
  ucell <- distinct(cell_tab, cell_) %>% mutate(area = extract(gridarea, cell_))
  ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
                   aes_zone)$ID
  
  
  ## summ_tab is the mean values over time
  summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone_data)) %>% 
    mutate(Season = aes_season(date)) %>% 
    group_by(Season, Zone, decade, SectorName,  date) %>%
    summarize(min = mean(min), max = mean(max), count = mean(count)) %>% 
    ungroup()
  
  ## raw_tab is all the cell values for density plots
  raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone_data)) %>% 
    mutate(Season = aes_season(date))
  
  write_feather(cell_tab,  file.path(outf, sprintf("%s_cell_tab.feather", vars[ivar])))
  rm(cell_tab)
  # writeRaster(ras,        file.path(outf, sprintf("%s_raster.grd", vars[ivar])))
  write_feather(summ_tab, file.path(outf, sprintf("%s_summ_tab.feather", vars[ivar])))
  rm(summ_tab)
  write_feather(raw_tab,  file.path(outf, sprintf("%s_raw_tab.feather", vars[ivar])))
  rm(raw_tab)
}
