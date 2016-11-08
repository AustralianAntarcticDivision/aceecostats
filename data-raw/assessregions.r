aes_region <- readRDS("data-raw/HabitatAssessment_regions.rds")
aes_region_ll <- readRDS("data-raw/HabitatAssessment_regionsLL.rds")

## change colour transparency
trans <- c(BanksPlateaus = "FF", Continent = "88", Deep = "44")
hab.cols <- aes_zone_cols()

aes_region$colour <- sprintf("%s%s", hab.cols$col[match(aes_region$SectorName, hab.cols$name)], trans[aes_region$BathyClass])
aes_region$area_km2 <- rgeos::gArea(aes_region, byid = TRUE)/1e6
aes_region <- aes_region[order(aes_region$area_km2), ]


devtools::use_data(aes_region, compress = "xz")
devtools::use_data(aes_region_ll, compress = "xz")

