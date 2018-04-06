library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)

library(tidyr)
library(DT)
#db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

dens <- tbl(db, "sst_density_tab") %>% 
  dplyr::filter(Zone == "High-Latitude")
d <- dens %>% dplyr::collect(n = Inf)
#d <- as.data.frame(roc::bin2lonlat(dens$bin_num, 4320))

sstgrid <- raster(extent(-180, 180, -80, -30), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", res = 0.25)
## get depth
topo <- readAll(raadtools::readtopo("ibcso"))
uxy <- distinct(d, cell_) %>% dplyr::mutate(x = xFromCell(sstgrid, cell_), y = yFromCell(sstgrid, cell_))
uxy$depth <- raster::extract(topo, 
                             rgdal::project(
                               as.matrix(uxy[c("x", "y")]), 
                               projection(topo)))
d <- d %>% dplyr::inner_join(uxy[c("cell_", "depth")])
op <- options(warn = -1)
ggplot(d %>% dplyr::filter(depth >= -500),  aes(x = max, group = decade, weight = 25, colour = decade)) + 
  geom_density() + facet_wrap(SectorName ~ season) 
