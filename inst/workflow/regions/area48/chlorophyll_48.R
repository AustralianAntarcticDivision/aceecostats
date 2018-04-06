
## preparation
library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)


#db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

dens <- tbl(db, "chl_25k_tab")  %>% 
  dplyr::filter(SectorName != "Indian") %>% 
  dplyr::filter(SectorName != "WestPacific")

d <- dens %>% dplyr::collect(n = Inf)

prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chlgrid <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                         crs = "+init=epsg:4326"), 
                                                  prjj), 25000), 
                res = 25000, crs = prjj)

ccamlr <- readRDS("/home/shared/data/assessment/sectors/ccamlr_statareas.rds")
ccamlr48 <- subset(ccamlr, grepl("48", name))
rm(ccamlr)
## use dopey indexing because assuming first column
d$ccamlr <- classify_cells_by_polygon(d$cell25, chlgrid, ccamlr48[, "name"])

d <- d %>% dplyr::filter(!is.na(ccamlr))

op <- options(warn = -1)
## 625 is area of 25km*25km pixels
ggplot(d %>% dplyr::filter(SectorName == "Atlantic"),  aes(x = chla_johnson, group = decade, weight = 625, colour = decade)) + 
  geom_density() + facet_wrap(SectorName ~ season) + scale_x_log10()
ggsave("inst/workflow/regions/area48/chl_48_Atlantic.png")
ggplot(d %>% dplyr::filter(SectorName == "EastPacific"),  aes(x = chla_johnson, group = decade, weight = 625, colour = decade)) + 
  geom_density() + facet_wrap(SectorName ~ season) + scale_x_log10()
ggsave("inst/workflow/regions/area48/chl_48_EastPacific.png")

options(op)

