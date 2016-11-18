## create a raster of the area of the pixels
library(raadtools)
p <- rasterToPolygons(r, n = 16)
pa <- spTransform(p, "+proj=laea +lat_0=-90")
library(rgeos);pa$a <- gArea(pa, byid = TRUE)
nsidc_south_area <- setValues(r, pa$a)
##devtools::use_data(nsidc_south_area)
