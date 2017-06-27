
## make a grid

## use the ice grid

library(raster)
library(aceecostats)
#grid <- raster(extent(-3950000, 3950000, -3950000, 4350000), nrow =  332, ncol = 316, 
#               crs = "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

library(dplyr)
grid <- extent(aes_zone) %>% tabularaster::bufext(25000) %>% raster(res = 25000, crs = projection(aes_zone))
library(feather)
library(sf)
data("wrld_simpl", package= "maptools")
m <- st_geometry(st_transform(st_as_sf(wrld_simpl), st_crs(projection(grid))))
outf <- "/mnt/acebulk"

vars <- c("sst", "chl")[1]
for (ivar in seq_along(vars)) {
  
  obj <- brick(file.path(outf, sprintf("%s.grd", vars[ivar])))
  
  #obj <- projectRaster(obj, grid, method = "bilinear")
  obj <- setZ(obj, getZ(brick(file.path(outf, sprintf("%s.grd", vars[ivar])))))
  
  segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))
  
  cell_tab <- vector("list", length(unique(segs)))
  dates <- as.POSIXct(getZ(obj))
  
  for (i in seq_along(cell_tab)) {
    asub <- which(segs == unique(segs)[i])
    a_obj <- setZ(readAll(subset(obj, asub)), dates[asub])
    
    print(min(dates[asub]))
    gridmax <- projectRaster(max(a_obj, na.rm = TRUE), grid)
    gridmean <- projectRaster(mean(a_obj, na.rm = TRUE), grid)
    gridcount <- projectRaster(calc(a_obj > 0, sum, na.rm = TRUE), grid, method = "ngb")
 
   plot(as(extent(gridmax), "SpatialPolygons"), col = "black", asp = 1); plot(gridcount, col = viridis::viridis(100), add = TRUE)
    print(i)
    rm(tab, a_obj)
    gc()
  }
}
