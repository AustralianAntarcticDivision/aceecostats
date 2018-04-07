## remap SST and ice to derived 25km chl grid
library(raster)
prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chlgrid <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                           crs = "+init=epsg:4326"), 
                                                    prjj), 25000), 
                  res = 25000, crs = prjj)


dp <- "/home/acebulk/data"

sst <- brick(file.path(dp, sprintf("%s.grd", "sst")))
projection(sst) <- "+proj=longlat +datum=WGS84"

l <- vector("list", nlayers(sst))
for (i in seq_along(l)) {
  l[[i]] <- projectRaster(sst[[i]], chlgrid)
}

sst25 <- writeRaster(brick(l), filename = file.path(dp, "sst_25k.grd"))

ice <- brick(file.path(dp, sprintf("%s.grd", "ice")))


l <- vector("list", nlayers(ice))
for (i in seq_along(l)) {
  l[[i]] <- projectRaster(ice[[i]], chlgrid)
}

writeRaster(brick(l), filename = file.path(dp, "ice_25k.grd"))



