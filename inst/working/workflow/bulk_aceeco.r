## required extension packages
## tibble and dplyr for data frame manipulation
library(tibble)
library(dplyr)

## custom functions for this study
library(aceecostats)
## custom data read functions
library(raadtools)



#' Functions to read individual maps. 
#' 
#' These are worker functions to read a specific file from the remote sensing
#' data collection. 
#' 
#' These functions encapsulate specific optimizations for extracting 
#' time series maps for the study.  The `i-th` index is one of the rows of the `files`` data frame. 
#' 
#' The specific data sets in use are: 
#' 
#' `read_i_ice` reads the NSIDC SMMR-SSM/I Nasateam daily sea ice concentration,
#'  Passive-microwave estimates of sea ice concentration at 25km spatial resolution. 
#'  Daily and monthly resolution, available from 1-Oct-1978 to present.
#' http://nsidc.org/data/nsidc-0051.html
#' 
#' `read_i_sst` reads the NOAA Optimum Interpolation 1/4 Degree Daily Sea Surface 
#' Temperature Analysis, Sea surface temperature at 0.25 degree daily resolution, 
#' from 1-Sep-1981 to present. 
#' Richard W. Reynolds, Viva F. Banzon, and NOAA CDR Program (2008): NOAA Optimum 
#' Interpolation 1/4 Degree Daily Sea Surface Temperature (OISST) Analysis, Version 2. 
#' [southern hemisphere, 1981-2016]. NOAA National Climatic Data Center. doi:10.7289/V5SQ8XB5 [November, 2016]
#' http://www.ngdc.noaa.gov/docucomp/page?xml=NOAA/NESDIS/NCDC/Geoportal/iso/xml/C00844.xml&view=getDataView&header=none
#' 
#' `read_i_u`, `read_i_v` and `read_i_mag` read 0.25 degree Ssalto/Duacs gridded absolute geostrophic 
#' velocities, Absolute geostrophic velocities computed from gridded sea surface heights 
#' above geoid from satellite altimetry  (delayed and near-real-time products). 
#' http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/madt.html. 
#' The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support 
#' from Cnes (http://www.aviso.altimetry.fr/duacs/
#' 
#' `read_i_chl` read from each of Oceandata VIIRS, MODISA and SeaWiFS Level-3 mapped 32-day 9km chlorophyll-a, 
#' rolling 32-day composite remote-sensing chlorophyll-a from the VIIRS, MODISA and SeaWiFS satellites at 
#' 9km spatial resolution". SeaWiFS is 1997-2002, MODISA is 2002-2012, and VIIRS is 2012-2016. 
#' http://oceancolor.gsfc.nasa.gov
#' 
#' Underlying each function is the infrastructure provided by an installation of `raadtools` and `raadsync`: 
#' https://github.com/AustralianAntarcticDivision/raadtools
#' https://github.com/AustralianAntarcticDataCentre/raadsync
#' 
#' @param i the index of the file
#' @param files the table of file names and dates
#'
#' @return [raster::RasterLayer()]
#' @export
#' @rdname read-maps
read_i_ice <- function(i, files) {
  raadtools::readice(files$date[i], inputfiles = files)
}
#' @rdname read-maps
read_i_sst <- function(i, files) {
  ex <- extent(0, 360, -80, -30)
  dat <- crop(raster(files$fullname[i], varname = "sst"), ex, snap = "out")
  mask <- crop(raster(files$fullname[i], varname = "ice"), ex, snap = "out")
  dat[!is.na(mask)] <- NA_real_
  raadtools:::.rotate(dat)
}
#' @rdname read-maps
read_i_u <- function(i, files) {
  ex <- extent(0, 360, -80, -30)
  dat <- crop(raster(files$fullname[i], varname = "u"), ex, snap = "out")
  raadtools:::.rotate(dat)
}
#' @rdname read-maps
read_i_v <- function(i, files) {
  ex <- extent(0, 360, -80, -30)
  dat <- crop(raster(files$fullname[i], varname = "v"), ex, snap = "out")
  raadtools:::.rotate(dat)
}
#' @rdname read-maps
read_i_mag <- function(i, files) {
  
  ex <- extent(0, 360, -80, -30)
  vlen <- function(x, y) sqrt(x*x + y*y)
  op <- options(warn = -1)
  udat <- crop(raster(files$fullname[i], varname = "u"), ex, snap = "out")
  vdat <- crop(raster(files$fullname[i], varname = "u"), ex, snap = "out")
  options(op)
  raadtools:::.rotate(vlen(udat, vdat))
}
#' @rdname read-maps
read_i_chl <- function(i, files) {
  ex <- extent(-180, 180, -80, -30)
  crop(raster(files$fullname[i], varname = "chlor_a"), ex)
}


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
oc <- bind_rows(oc %>% filter(date < min(vr$date)), vr)

## build bulk caches from the remote sensing file collections
## each .grd file output is every time step  for the study area available
ice <- build_bulk_file(icefiles(), file.path(outf, "ice.grd"), read_i_ice, layer_prefix = "ice")
sst <- build_bulk_file(sstfiles(), file.path(outf, "sst.grd"), read_i_sst, layer_prefix = "sst")
chl <- build_bulk_file(oc, file.path(outf, "chl.grd"), read_i_chl, layer_prefix = "month_chl")


## load previously calculated sea ice season metrics (seaiceson_southern_2016.Rmd)
library(raster)
outf <- "/mnt/acebulk"
ret <- readRDS(file.path(outf, "south_retreat.rds"))

adv <- readRDS(file.path(outf,"south_advance.rds") )
duration <- ret - adv
## if retreat is equal to one, it didn't retreat
duration[ret == 1] <- 365
obj <- setZ(duration, ISOdatetime(1979:2015, 2, 15, 0, 0, 0, tz = "GMT"))


