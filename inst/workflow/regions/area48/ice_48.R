## preparation
library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)


#db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

dens <- tbl(db, "ice_days_density_tab") %>% dplyr::filter(Zone == "Continent")
d <- dens %>% dplyr::collect(n = Inf)
icegrid <- raster(extent(-3950000, 3950000, -3950000, 4350000), 
                  crs = "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs", 
                  res = 25000)


ccamlr <- readRDS("/home/shared/data/assessment/sectors/ccamlr_statareas.rds")
ccamlr48 <- subset(ccamlr, grepl("48", name))
d$ccamlr <- classify_cells_by_polygon(d$cell_, icegrid, ccamlr48[, "name"])
op <- options(warn = -1)
ggplot(d  %>% 
         dplyr::filter(days < 365 ), aes(x = days, group = decade, weight = area, colour = decade)) + geom_density() + facet_wrap(SectorName ~ season)
ggsave("inst/workflow/regions/area48/ice_48.png")
par(op)

