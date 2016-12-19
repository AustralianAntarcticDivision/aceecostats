## required extension packages
## tibble and dplyr for data frame manipulation
library(tibble)
library(dplyr)

## custom functions for this study
library(aceecostats)
## custom data read functions
library(raadtools)

## specify a working folder where all cached file outputs will go
## (plots all go into the working folder)
outf <- "/mnt/acebulk"

## build up file collection specially for chlorophyll-a
## SeaWiFS, then MODISA, then VIIRS
sw <- ocfiles(time.resolution = "monthly", product = "SeaWiFS", varname = "CHL", type = "L3m", ext = "nc")
md <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m", ext = "nc")
oc <- bind_rows(sw %>% filter(date < min(md$date)), md) %>%   as_tibble()
vr <- ocfiles(time.resolution = "monthly", product = "VIIRS", varname = "SNPP_CHL", type = "L3m", ext = "nc")

## this is all available days with VIIRS chosent over MODISA chosen over SeaWiFS
aes_chlfiles <- bind_rows(oc %>% filter(date < min(vr$date)), vr) %>% as_tibble()
aes_icefiles <- icefiles() %>% as_tibble()
aes_sstfiles <- sstfiles() %>% as_tibble()
devtools::use_data(aes_chlfiles, aes_icefiles, aes_sstfiles)

## build bulk caches from the remote sensing file collections
## each .grd file output is every time step  for the study area available
ice <- build_bulk_file(aes_icefiles, file.path(outf, "ice.grd"), read_i_ice, layer_prefix = "ice")
sst <- build_bulk_file(aes_sstfiles, file.path(outf, "sst.grd"), read_i_sst, layer_prefix = "sst")
chl <- build_bulk_file(aes_chlfiles, file.path(outf, "chl.grd"), read_i_chl, layer_prefix = "month_chl")


## load previously calculated sea ice season metrics (seaiceson_southern_2016.Rmd)
library(raster)
outf <- "/mnt/acebulk"
ret <- readRDS(file.path(outf, "south_retreat.rds"))

adv <- readRDS(file.path(outf,"south_advance.rds") )
duration <- ret - adv
## if retreat is equal to one, it didn't retreat
duration[ret == 1] <- 365
obj <- setZ(duration, ISOdatetime(1979:2015, 2, 15, 0, 0, 0, tz = "GMT"))


