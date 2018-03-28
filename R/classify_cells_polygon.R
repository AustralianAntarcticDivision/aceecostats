#' Classify raster cells by polygons
#' 
#' from polygons, copy the first column of data for every cell
#' the cells and grid MUST match, but the polygons can be in any projection
#' Make sure the polygons have the first column as the classifier you want
#' @param cells raster cell index
#' @param grid raster for the cells
#' @param polygons SpatialPolygonsDataFrame
#' @export
classify_cells_by_polygon <- function(cells, grid, polygons) {
  p_polygons <- spTransform(polygons, projection(grid))
  p_polygons$idx <- seq_len(nrow(p_polygons))
  
  poly_grid <- fasterize::fasterize(sf::st_as_sf(p_polygons), grid, field = "idx")
                                    
  p_polygons[[1]][raster::extract(poly_grid, cells)]
}