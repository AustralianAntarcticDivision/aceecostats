## create the bin tables from L3 ocean colour, so we have them on hand
## classified by cell_, area (km2), ID, SectorName and Zone

library(sp)
library(aceecostats)
library(roc)
library(dplyr)
## RUNME
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))


init <- initbin(NUMROWS = 4320)
## counts up from the south
maxbin <- init$totbin/2
## unique grid map cell number
#ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))

ucell <- tibble(cell_ = seq_len(maxbin), area = 4600)
xy <- sp::SpatialPoints(do.call(cbind, bin2lonlat(ucell$cell_, 4320)), proj4string = CRS("+init=epsg:4326"))
ucell$ID <- over(spTransform(xy, raster::projection(aes_zone)), 
                 aes_zone)$ID

ucell <- 
  ucell %>% filter(!is.na(ID)) %>% inner_join(aes_zone@data %>% select(-area_km2, -colour))
#db$con %>% db_drop_table(table='modis_bins') 
dplyr::copy_to( db, ucell, "modis_bins", temporary = FALSE, 
                index= list( "SectorName", "Zone"))


library(roc)
init <- initbin(NUMROWS = 2160)
## counts up from the south
maxbin <- init$totbin/2

ucell <- tibble(cell_ = seq_len(maxbin), area = 9200)
xy <- sp::SpatialPoints(do.call(cbind, bin2lonlat(ucell$cell_, 2160)), 
                        proj4string = CRS("+init=epsg:4326"))
ucell$ID <- over(spTransform(xy, raster::projection(aes_zone)), 
                 aes_zone)$ID

ucell <- 
  ucell %>% filter(!is.na(ID)) %>% 
  inner_join(aes_zone@data %>% select(-area_km2, -colour))
#db$con %>% db_drop_table(table='seawifs_bins') 
dplyr::copy_to( db, ucell, "seawifs_bins", temporary = FALSE, 
                index= list( "SectorName", "Zone"))
