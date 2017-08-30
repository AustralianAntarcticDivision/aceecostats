## check area details
library(dplyr)
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))
library(aceecostats)
aes_region_simple$total_area_km2 <- rgeos::gArea(aes_region_simple, byid = TRUE) /1e6

## these tables huge, for computation
chl_tab <- tbl(db, "chl_johnson_tab") %>% collect()
sst_tab <-     tbl(db, "sst_density_tab") %>% collect()
ice_tab <- tbl(db, "ice_days_density_tab")  %>% collect()

save(aes_region_simple, chl_tab, sst_tab, ice_tab, file = file.path(dp, "aes_area_summary_tables.Rdata"), compress = FALSE)
