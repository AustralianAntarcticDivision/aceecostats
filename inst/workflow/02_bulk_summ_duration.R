
## SEA ICE DURATION

library(dplyr)
# RUNME
dp <- "/home/acebulk"
db <- dplyr::src_sqlite(file.path(dp, "data/habitat_assessment.sqlite3"))

library(raster)
library(tibble)
library(dplyr)

library(aceecostats)

## see data-raw
gridarea <- readRDS(file.path(dp,"data/nsidc_south_area.rds"))/1e6
## put a tidy end to the series
maxdate <- ISOdatetime(2017, 9, 1, 0, 0, 0, tz = "GMT")
## load previously calculated sea ice season metrics (seaiceson_southern_2016.Rmd)
library(raster)
adv <- brick(file.path(dp, "seaiceseason/south_advance.grd"))
ret <- brick(file.path(dp, "seaiceseason/south_retreat.grd"))
duration <- ret - adv
## if retreat is equal to one, it didn't retreat
duration[ret == 1] <- 365
obj <- setZ(duration, ISOdatetime(seq(1979, length = nlayers(adv)), 2, 15, 0, 0, 0, tz = "GMT"))
rm(ret, adv)

ras <- raster(obj)

listtab <- vector("list", nlayers(obj))
dates <- as.POSIXct(getZ(obj))
for (i in seq_along(listtab)) {
  asub <- i # which(segs == unique(segs)[i])
  a_obj <- subset(obj, asub)
  
  tab <- tabit(a_obj) %>% rename(dur = val) %>% mutate(date = dates[asub[1]])  
  listtab[[i]] <- tab
  print(i)
}


cell_tab <- bind_rows(listtab) %>% 
  mutate(decade = decade_maker(date)) %>% 
  filter(date <  maxdate) %>% 
  filter(!is.na(decade))

ucell <- distinct(cell_tab, cell_) %>% mutate(area = raster::extract(gridarea[[1]], cell_))
ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
                    aes_zone)$ID

## summ_tab is the mean values over time
summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone@data[, c("ID", "SectorName", "Zone")])) %>% 
  group_by(Zone, decade, SectorName,  date) %>%
  summarize(dur = mean(dur)) %>% 
  ungroup()


summ_tab_nozone <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone@data[, c("ID", "SectorName", "Zone")])) %>% 
  group_by(decade, SectorName,  date) %>%
  summarize(dur = mean(dur)) %>% 
  ungroup()

## raw_tab is all the cell values for density plots
raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone@data[, c("ID", "SectorName", "Zone")])) 

raw_tab <- raw_tab %>% mutate(season = aes_season(date))
#db$con %>% db_drop_table(table='ice_density_tab')
#db$con %>% db_drop_table(table='ice_sparkline_tab')
#db$con %>% db_drop_table(table='ice_sparkline_tab_nozone')

copy_to(db, raw_tab, "ice_density_tab", temporary = FALSE)
copy_to(db, summ_tab, "ice_sparkline_tab", temporary = FALSE)
copy_to(db, summ_tab_nozone, "ice_sparkline_tab_nozone", temporary = FALSE)


