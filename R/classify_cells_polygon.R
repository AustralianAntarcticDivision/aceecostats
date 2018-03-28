#' Classify raster cells by polygons
#' 
#' from polygons, copy the first column of data for every cell
#' the cells and grid MUST match, but the polygons can be in any projection
#' Make sure the polygons have the first column as the classifier you want
#' @param cells raster cell index
#' @param grid raster for the cells
#' @param polygons SpatialPolygonsDataFrame
#' @export
#' @examples 
#' library(aceecostats)
#'  library(raster)
#'  library(dplyr)
#'  library(ggplot2)
#'  
#'  dp <- "/home/acebulk/data"
#'  db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))
#'  
#'  dens <- tbl(db, "ice_days_density_tab") %>% dplyr::filter(Zone == "Continent")
#'  d <- dens %>% dplyr::collect(n = Inf)
#'  icegrid <- raster(extent(-3950000, 3950000, -3950000, 4350000), 
#'                    crs = "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs", 
#'                    res = 25000)
#'  
#'  ccamlr <- readRDS("/home/shared/data/assessment/sectors/ccamlr_statareas.rds")
#'  ccamlr48 <- subset(ccamlr, grepl("48", name))
#'  d$ccamlr <- classify_cells_by_polygon(d$cell_, icegrid, ccamlr48[, "name"])
#'  
#'  
#'  d %>% dplyr::filter(!is.na(ccamlr)) %>%  mutate(x = xFromCell(icegrid, cell_), y = yFromCell(icegrid, cell_)) %>% 
#'    ggplot(aes(x, y, fill = ccamlr)) + geom_raster() + ggplot2::coord_equal()
classify_cells_by_polygon <- function(cells, grid, polygons) {
  p_polygons <- spTransform(polygons, projection(grid))
  p_polygons$idx <- seq_len(nrow(p_polygons))
  
  poly_grid <- fasterize::fasterize(sf::st_as_sf(p_polygons), grid, field = "idx")
                                    
  p_polygons[[1]][raster::extract(poly_grid, cells)]
}