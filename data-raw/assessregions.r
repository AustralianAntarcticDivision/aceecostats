aes_region <- readRDS("data-raw/HabitatAssessment_regions.rds")
aes_region_ll <- readRDS("data-raw/HabitatAssessment_regionsLL.rds")

aes_region <- rmapshaper::ms_simplify(aes_region)
aes_region_ll <- rmapshaper::ms_simplify(aes_region_ll)
aes_region_ll$rmapshaperid <- aes_region$rmapshaperid <- NULL
## change colour transparency
trans <- c(BanksPlateaus = "FF", Continent = "88", Deep = "44")
hab.cols <- aes_zone_cols()

aes_region_ll$colour <- aes_region$colour <- sprintf("%s%s", hab.cols$col[match(aes_region$SectorName, hab.cols$name)], trans[aes_region$BathyClass])
aes_region_ll$area_km2 <- aes_region$area_km2 <- rgeos::gArea(aes_region, byid = TRUE)/1e6

aes_region$ID <- 1:nrow(aes_region)
aes_region_ll$ID <- 1:nrow(aes_region_ll)

devtools::use_data(aes_region, compress = "xz")
devtools::use_data(aes_region_ll, compress = "xz")


library(aceecostats)
library(dplyr)
library(rgeos)
library(sp)

aes_region$index <- seq(nrow(aes_region))
aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")


p1 <- gUnionCascaded(aes_region, paste(aes_region$SectorName, aes_region$Zone, aes_region$Shelf))

library(dplyr)
## gets a bit shaky, this might get reordered somewhere
d <- aes_region@data %>% group_by(SectorName, Zone, Shelf) %>% summarize(area_km2 = sum(area_km2))

aes_region_simple <- SpatialPolygonsDataFrame(p1, as.data.frame(d), match.ID = FALSE)

sector_colour <- function(secname) {
  setNames(c("#7CAE00", "#00BFC4","#C77CFF", "#F8766D"), 
           c("Atlantic","Indian", "EastPacific","WestPacific"))[secname]
  
}

aes_region_simple$colour <- sector_colour(aes_region_simple$SectorName)
devtools::use_data(aes_region_simple, compress = "xz")
## CHECK!  
#plot(aes_region_simple, col = aes_region_simple$colour)
#text(coordinates(aes_region_simple), lab = sprintf("%s \n %s \n %s", aes_region_simple$SectorName, aes_region_simple$Zone, aes_region_simple$Shelf))



