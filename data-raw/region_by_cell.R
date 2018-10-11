default_grid <- function() {
  prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
  raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                  crs = "+init=epsg:4326"), 
                                           prjj), 25000), 
         res = 25000, crs = prjj)
  
  
}
aes_zone$ID <- 1:nrow(aes_zone)
region <- fasterize::fasterize(sf::st_as_sf(aes_zone), default_grid(), field = "ID")
region <- tabularaster::as_tibble(region, value = TRUE) %>% 
  mutate(Zone = aes_zone$Zone[cellvalue], 
         SectorName = aes_zone$SectorName[cellvalue]) %>% 
  dplyr::select(-cellvalue)

usethis::use_data(region)
