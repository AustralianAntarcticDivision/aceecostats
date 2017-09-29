library(raadtools)
library(tabularaster)
library(dplyr)
## the inner continental boundary
innercont <- raadtools:::keepOnlyMostComplexLine(rasterToContour(!is.na(readice_monthly())))
polyinner <- rgeos::gPolygonize(innercont)

## all monthly files (since 1978)
mfiles <- raadfiles::nsidc_south_monthly_files() %>% dplyr::mutate(irow = row_number())
ice <- readAll(readice_monthly(mfiles$date, inputfiles = mfiles, setNA = FALSE))

## a background grid
# grid <- graticule::graticule(lons = seq(-180, 180, by = 5), 
#                              lats = c(-80, -40), 
#                              ylim = c(-80, -40), tiles = TRUE, 
#                              nverts = 36, proj = projection(ice))

dofun <- function(i) {
  print(i)
  ## read the data, contour, and get the outer biggest polyline
  cont15 <- try(raadtools:::keepOnlyMostComplexLine(rasterToContour(ice[[i]] > 15, level = 1)))
  poly <- rgeos::gPolygonize(cont15)
  if (is.null(poly)) {
    ## catch exceptions where the ice extent exceeds the map
    ## preventing contours from closing polylines ...
    cont15 <- raadtools:::keepOnlyMostComplexLine(rasterToContour(extend(ice[[i]], extent(ice[[i]]) + 250000, value = 0),                             level = 15))
   poly <- rgeos::gPolygonize(cont15)
  }
  ## erase the inner boundary from this outer 15% hull
  aa <- sf::st_as_sf(raster::erase(poly, polyinner))
  aa[["irow"]] <- i  ## record the row number
  aa
  
}

## apply the extraction to each month
l <- purrr::map(mfiles$irow, dofun)

## bundle the individual months together, calculate area
library(sf)
x <- do.call(rbind, l)
x$area <- sf::st_area(sf::st_transform(x$geometry, "+proj=laea +lat_0=-90 +datum=WGS84"))
x$area <- as.numeric(x$area)

## drop the geometry (we have area)
## and clean up with date, area, month, year
result <- x %>%  st_set_geometry(NULL) %>%  
  inner_join(mfiles %>% dplyr::select(date, irow), "irow") %>% 
  dplyr::select(-irow)
result$month <- as.integer(format(result$date, "%m"))
result$year <- as.integer(format(result$date, "%Y"))
result$date <- NULL
## write out area of ice extent by month
write.csv(result, "monthly_ice_extent.csv", row.names = FALSE)


## trim to July August
monthly_extent <- read.csv("monthly_ice_extent.csv")
library(dplyr)  

winter_extent <- monthly_extent %>% dplyr::filter(month %in% c(7, 8)) %>% 
  group_by(year) %>% 
  summarize(area = sum(area), count = n())

write.csv(winter_extent, file = "winter_extent.csv", row.names = FALSE)



