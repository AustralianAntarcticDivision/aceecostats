library(aceecostats)
library(geojson)

x <- aes_zone_ll


r0 <- col2rgb(x$colour, alpha = TRUE)
x$fill <- rgb(r0[1, ], r0[2, ], r0[3, ], max = 255)
x$`fill-opacity` <- r0[4, ]/255
x$colour <- NULL
geojson::geo_write(as.geojson(x), "data-raw/aes_region_ll.geojson")


x <- aes_zone


r0 <- col2rgb(x$colour, alpha = TRUE)
x$fill <- rgb(r0[1, ], r0[2, ], r0[3, ], max = 255)
x$`fill-opacity` <- r0[4, ]/255
x$colour <- NULL
geojson::geo_write(as.geojson(x), "data-raw/aes_region.geojson")
