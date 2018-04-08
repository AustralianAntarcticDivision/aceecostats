## remap SST and ice to derived 25km chl grid
library(raster)
prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chlgrid <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                           crs = "+init=epsg:4326"), 
                                                    prjj), 25000), 
                  res = 25000, crs = prjj)


dp <- "/home/acebulk/data"

library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)

#db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))
sst <- raster(file.path(dp, "sst.grd"))

dens <- tbl(db, "sst_density_tab") %>% 
  dplyr::select(decade, season, min, max, med, cell_) %>% collect(n = Inf)
## now split by decade and season in turn
units <- distinct(dens, decade, season)
newdens <- vector("list", nrow(units))
for (i in seq_len(nrow(units))) {
  dens0 <- dens %>% dplyr::filter(decade == units$decade[i], season == units$season[i])
  sst[] <- NA
  med <- mx <- mn <- sst
  mn[dens0$cell_] <- dens0$min
  mx[dens0$cell_] <- dens0$max
  med[dens0$cell_] <- dens0$med
  mn <- projectRaster(mn, chlgrid)
  mx <- projectRaster(mx, chlgrid)
  med <- projectRaster(med, chlgrid)
  newdens[[i]] <- tibble::tibble(decade = units$decade[i], cell_ = seq_len(ncell(mn)), 
                                 min = values(mn), max = values(mx), med = values(med), 
                                 season = units$season[i], area = 625) %>% dplyr::filter(!is.na(min))
  print(i)
}

## now put the regions back on



newdens <- bind_rows(newdens)
chlmap <- tbl(db, "chl_25k_tab") %>% dplyr::distinct(cell25, SectorName, Zone) %>% 
  dplyr::collect(n = Inf)

newdens <- dplyr::inner_join(newdens, chlmap, c("cell_" = "cell25")) %>% 
  dplyr::filter(!is.na(SectorName), !is.na(Zone))

#copy_to(db, newdens, "sst_25k_tab", temporary = FALSE, 
#        indexes = list("cell_", "decade", "season"))
newdens[c("x", "y")] <- xyFromCell(chlgrid, newdens$cell_)
ggplot(newdens, aes(x, y, fill = min)) + facet_wrap(~decade+season) + geom_raster()



## remap SST and ice to derived 25km chl grid
library(raster)
prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chlgrid <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                           crs = "+init=epsg:4326"), 
                                                    prjj), 25000), 
                  res = 25000, crs = prjj)


dp <- "/home/acebulk/data"

library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)

#db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))
ice <- raster(file.path(dp, "ice.grd"))

dens <- tbl(db, "ice_days_density_tab") %>% 
  dplyr::select(decade, days, cell_) %>% collect(n = Inf)
## now split by decade and season in turn
units <- distinct(dens, decade)
newdens <- vector("list", nrow(units))
for (i in seq_len(nrow(units))) {
  dens0 <- dens %>% dplyr::filter(decade == units$decade[i])
  ice[] <- NA
  days <- ice
  days[dens0$cell_] <- dens0$days
  days <- projectRaster(days, chlgrid, method = "ngb")
  newdens[[i]] <- tibble::tibble(decade = units$decade[i], cell_ = seq_len(ncell(days)), 
                                 days = values(days), 
                                 area = 625) %>% dplyr::filter(!is.na(days))
  print(i)
}

## now put the regions back on



newdens <- bind_rows(newdens)
chlmap <- tbl(db, "chl_25k_tab") %>% dplyr::distinct(cell25, SectorName, Zone) %>% 
  dplyr::collect(n = Inf)

newdens <- dplyr::inner_join(newdens, chlmap, c("cell_" = "cell25")) %>% 
  dplyr::filter(!is.na(SectorName), !is.na(Zone))

#copy_to(db, newdens, "ice_days_25k_tab", temporary = FALSE, 
#        indexes = list("cell_", "decade"))
newdens[c("x", "y")] <- xyFromCell(chlgrid, newdens$cell_)
ggplot(newdens, aes(x, y, fill = days)) + facet_wrap(~decade) + geom_raster()









## just cheat
##'gdalwarp /home/acebulk/data/sst.grd -ts "+proj=laea +lat_0=-90 +datum=WGS84" -te -6400000 -6400000 6400000 6075000 -ts 512 499 sst.tif'

# gdalwarp \
# NETCDF:"/rdsi/PUBLIC/raad/data/www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/198201/avhrr-only-v2.19820101.nc":sst \
# -t_srs "+proj=laea +lat_0=-90 +datum=WGS84" \
# -te -6400000 -6400000 6400000 6075000 \
# -r nearest \
# -ts 512 499 psst.tif
# 
# 'gdalwarp /home/acebulk/data/ice.grd -ts "+proj=laea +lat_0=-90 +datum=WGS84" -te -6400000 -6400000 6400000 6075000 -ts 512 499 sst.tif'
# 


