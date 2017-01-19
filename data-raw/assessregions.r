aes_region <- readRDS("data-raw/HabitatAssessment_regions.rds")
aes_region_ll <- readRDS("data-raw/HabitatAssessment_regionsLL.rds")
mkchar <- function(x) if (is.factor(x)) levels(x)[x] else x



aes_region <- rmapshaper::ms_simplify(aes_region)
aes_region_ll <- rmapshaper::ms_simplify(aes_region_ll)
aes_region@data[] <- lapply(aes_region@data, mkchar)
aes_region_ll@data[] <- lapply(aes_region_ll@data, mkchar)

aes_region_ll$rmapshaperid <- aes_region$rmapshaperid <- NULL

aes_region$ID <- 1:nrow(aes_region)
aes_region_ll$ID <- 1:nrow(aes_region_ll)

aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")
aes_region_ll$Shelf <-  ifelse(aes_region_ll$BathyClass == "Continent", "Shelf", "Ocean")


## change colour transparency
##trans <- c(BanksPlateaus = "FF", Continent = "88", Deep = "44")
hab.cols <- character(nrow(aes_region))
for (i in seq_along(hab.cols)) hab.cols[i] <- scales::alpha(aceecostats:::aes_zone_hue()[aes_region$SectorName][i],
                                                            aceecostats:::aes_zone_alpha()[ifelse(aes_region$Shelf == "Shelf", "Shelf", aes_region$Zone)][i]
)


aes_region_ll$colour <- aes_region$colour <- hab.cols

library(rgeos)
p1 <- gUnionCascaded(aes_region, paste(aes_region$SectorName, aes_region$Zone, aes_region$Shelf))

library(dplyr)
## gets a bit shaky, this might get reordered somewhere
d <- aes_region@data %>% group_by(SectorName, Zone, Shelf) %>% summarize(area_km2 = sum(area_km2))
aes_region_simple <- SpatialPolygonsDataFrame(p1, as.data.frame(d), match.ID = FALSE)

aes_region_ll$area_km2 <- aes_region$area_km2 <- rgeos::gArea(aes_region, byid = TRUE)/1e6
aes_region_simple$area_km2 <- rgeos::gArea(aes_region_simple, byid = TRUE)/1e6

hab.cols2 <- character(nrow(aes_region_simple))
for (i in seq_along(hab.cols2)) hab.cols2[i] <- scales::alpha(aceecostats:::aes_zone_hue()[aes_region_simple$SectorName][i],
                                                            aceecostats:::aes_zone_alpha()[ifelse(aes_region_simple$Shelf == "Shelf", "Shelf",
                                                                                                  aes_region_simple$Zone)][i])

aes_region_simple$colour <- hab.cols2
plot(aes_region_ll, col = aes_region_ll$colour)
plot(aes_region, col = aes_region$colour)
plot(aes_region_simple, col = aes_region_simple$colour)


devtools::use_data(aes_region, compress = "xz")
devtools::use_data(aes_region_ll, compress = "xz")

devtools::use_data(aes_region_simple, compress = "xz")
## CHECK!
#plot(aes_region_simple, col = aes_region_simple$colour)
#text(coordinates(aes_region_simple), lab = sprintf("%s \n %s \n %s", aes_region_simple$SectorName, aes_region_simple$Zone, aes_region_simple$Shelf))



