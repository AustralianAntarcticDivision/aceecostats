default_grid <- function() {
  prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
  raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                  crs = "+init=epsg:4326"), 
                                           prjj), 25000), 
         res = 25000, crs = prjj)
  
  
}
aes_region_simple$ID <- 1:nrow(aes_region_simple)
region <- fasterize::fasterize(sf::st_as_sf(aes_region_simple), default_grid(), field = "ID")
region <- tabularaster::as_tibble(region, value = FALSE) %>% 
  mutate(Zone = aes_region_simple$Zone[cellindex], 
         SectorName = aes_region_simple$SectorName[cellindex])
usethis::use_data(region)
