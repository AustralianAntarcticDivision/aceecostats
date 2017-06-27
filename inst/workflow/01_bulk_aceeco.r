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

## takes about 4856 seconds
sst <- build_bulk_file(aes_sstfiles, file.path(dp, "sst.grd"), read_i_sst, layer_prefix = "sst")

