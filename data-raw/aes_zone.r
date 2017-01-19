# aes_zone

library(aceecostats)
data(aes_region_simple)

aes_zone <- aes_region_simple
aes_zone$Zone <- unname(c(Temperate = "Mid-Latitude", Polar = "High-Latitude")[aes_region_simple$Zone])
aes_zone$Zone[aes_region_simple$Shelf == "Shelf"] <- "Continent"
aes_zone$Shelf <- NULL


library(dplyr)
## gets a bit shaky, this might get reordered somewhere
d <- aes_region@data %>% group_by(SectorName, Zone, Shelf) %>% summarize(area_km2 = sum(area_km2))
aes_region_simple <- SpatialPolygonsDataFrame(p1, as.data.frame(d), match.ID = FALSE)

p2 <- rgeos::gUnionCascaded(aes_region_ll, paste(aes_region_ll$SectorName, aes_region_ll$Zone, aes_region_ll$Shelf))
library(dplyr)
d1 <- aes_region_ll@data %>% group_by(SectorName, Zone, Shelf) %>% summarize(area_km2 = sum(area_km2))
aes_zone_ll <- SpatialPolygonsDataFrame(p2, as.data.frame(d1), match.ID = FALSE)
aes_zone_ll$Zone <- unname(c(Temperate = "Mid-Latitude", Polar = "High-Latitude")[aes_zone_ll$Zone])
aes_zone_ll$Zone[aes_zone_ll$Shelf == "Shelf"] <- "Continent"
aes_zone_ll$Shelf <- NULL
aes_zone_ll$colour <- aes_zone$colour

devtools::use_data(aes_zone)
devtools::use_data(aes_zone_ll)
