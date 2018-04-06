library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)

#db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

dens <- tbl(db, "sst_density_tab") %>% 
  dplyr::filter(SectorName != "Indian") %>% 
  dplyr::filter(SectorName != "WestPacific")

d <- dens %>% dplyr::collect(n = Inf)

sstgrid <- raster(extent(-180, 180, -80, -30), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", res = 0.25)


ccamlr <- readRDS("/home/shared/data/assessment/sectors/ccamlr_statareas.rds")
ccamlr48 <- subset(ccamlr, grepl("48", name))
rm(ccamlr)
## use dopey indexing because assuming first column
d$ccamlr <- classify_cells_by_polygon(d$cell_, sstgrid, ccamlr48[, "name"])

d <- d %>% dplyr::filter(!is.na(ccamlr))
op <- options(warn = -1)
ggplot(d %>% dplyr::filter(SectorName ==  "Atlantic"),  
       aes(x = max, group = decade, weight = area)) + 
  geom_density() + facet_wrap(Zone ~ season)

options(op)

