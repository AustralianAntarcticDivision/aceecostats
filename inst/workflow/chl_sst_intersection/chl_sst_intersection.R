# p <- "/home/acebulk/data"
# db <- src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))
# epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
# 
# ice_tab <- tbl(db, "ice_days_density_tab") %>%
#   collect(n = Inf) %>% mutate(date = date + epoch) %>%
#   filter(days >= 1, days <= 366)
# 
# chl_tab <-  tbl(db, "chl_25k_tab")  %>%  
#   mutate(area = (4600 * 4600)/1e6) %>%
#   collect(n = Inf) %>%
#   transform(season=factor(season, levels=c("Summer","Autumn","Winter","Spring"))) %>%
#   transform(SectorName=factor(SectorName, levels=c("Atlantic", "Indian","WestPacific","EastPacific"))) %>% tbl_df()
# 
# sst_tab <- tbl(db, "sst_density_tab") %>%  collect(n = Inf) %>% tbl_df()



library(dplyr)
library(raster)
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

## apply filter on chla-a 0.5 or higher
chl_tab <-  tbl(db, "chl_25k_tab")  %>%  
  dplyr::filter(chla_johnson >= 0.5) %>% 
  dplyr::select(chla_johnson, cell25, decade, season, SectorName, Zone) %>% 
   collect(n = Inf) 

prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chlgrid <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                         crs = "+init=epsg:4326"), 
                                                  prjj), 25000), 
                res = 25000, crs = prjj)

## apply filter on 2deg C or lower
sst_tab <- tbl(db, "sst_density_tab") %>% 
  dplyr::filter(med <= 2) %>% 
  dplyr::transmute(sst = med, cell_, decade, season, area, SectorName, Zone) %>% 
  collect(n = Inf)

sstgrid <- raster(brick(file.path(dp, sprintf("%s.grd", "sst"))))
projection(sstgrid) <- "+init=epsg:4326"
sstarea <- area(sstgrid) 

## map the sst cell to its chl-cell

sst_tab[c("cell_chl")] <- cellFromXY(chlgrid, rgdal::project(xyFromCell(sstgrid, sst_tab$cell_), projection(chlgrid)))

sst_tab_0 <- dplyr::filter(sst_tab, sst_tab$cell_chl %in% chl_tab$cell25)
chl_tab_0 <- dplyr::filter(chl_tab, chl_tab$cell25 %in% sst_tab$cell_chl)

## depth
topo <- raadtools::readtopo("gebco_14", xylim = extent(-180, 180, -90, 0))
sst_tab_0$depth <- raster::extract(topo, xyFromCell(sstgrid, sst_tab_0$cell_))
chl_tab_0$depth <- raster::extract(topo, rgdal::project(xyFromCell(chlgrid, chl_tab_0$cell25), 
                                                        projection(chlgrid), inv = TRUE))


##"/home/shared/data/assessment/sst_chl_intersection/"
saveRDS(sst_tab_0, "sst_intersectCHL.rds")
saveRDS(chl_tab_0, "chl_intersectSST.rds")


