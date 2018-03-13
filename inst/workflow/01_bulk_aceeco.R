## required extension packages
## tibble and dplyr for data frame manipulation
library(tibble)
library(dplyr)

## custom functions for this study
library(aceecostats)
## custom data read functions
library(raadtools)

## RUNME
## specify a working folder where all cached file outputs will go
## (plots all go into the working folder)
dp <- "/home/acebulk/data"

aes_icefiles <- icefiles() %>% as_tibble()
aes_sstfiles <- sstfiles() %>% as_tibble()
devtools::use_data(aes_icefiles, aes_sstfiles, overwrite = TRUE)



## build bulk caches from the remote sensing file collections
## each .grd file output is every time step  for the study area available
## takes about 600 seconds
ice <- build_bulk_file(aes_icefiles, file.path(dp, "ice.grd"), read_i_ice, layer_prefix = "ice")


library(dplyr)
library(tidync)
tnc <- tidync(aes_sstfiles$fullname[1]) 
hf <- tnc %>% 
  hyper_filter(lat  = between(lat, -80, -30))
update_source <- function(x, source) {
  src <- attr(x, "source")
  src$source <- source
  attr(x, "source") <- src
  x
}
read_i_sst_tidync <- function(i, files) {
  arr <- update_source(hf, files$fullname[i]) %>% 
    hyper_slice(select_var = c("sst", "ice"))
  sst <- arr$sst
  sst[!is.na(arr$ice)] <- NA_real_
  sst2 <- rbind(sst[721:1440, ], sst[1:720, ])
  setExtent(raster(t(sst2[, ncol(sst2):1])), extent(-180, 180, -80, -30))
}


## takes about 4856 seconds
sst <- build_bulk_file(aes_sstfiles, file.path(dp, "sst.grd"), read_i_sst_tidync, layer_prefix = "sst")

