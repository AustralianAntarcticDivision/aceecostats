library(sp)
library(aceecostats)
library(roc)
init <- initbin(NUMROWS = 4320)
## counts up from the south
maxbin <- init$totbin/2
## unique grid map cell number
#ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))

ucell <- tibble(cell_ = seq_len(maxbin), area = 4600)
xy <- sp::SpatialPoints(do.call(cbind, bin2lonlat(ucell$cell_, 4320)), proj4string = CRS("+init=epsg:4326"))
ucell$ID <- over(spTransform(xy, raster::projection(aes_zone)), 
                 aes_zone)$ID

ucell <- ucell %>% filter(!is.na(ID))
db <- dplyr::src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")

dplyr::copy_to( db, ucell, "modis_bins", temporary = FALSE)
