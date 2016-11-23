library(tibble)


library(raadtools)

read_i_ice <- function(i, files) {
  raadtools::readice(files$date[i], inputfiles = files)
}
read_i_sst <- function(i, files) {
  ex <- extent(0, 360, -80, -30)
  dat <- crop(raster(files$fullname[i], varname = "sst"), ex, snap = "out")
  mask <- crop(raster(files$fullname[i], varname = "ice"), ex, snap = "out")
  dat[!is.na(mask)] <- NA_real_
  raadtools:::.rotate(dat)
}

read_i_u <- function(i, files) {
  ex <- extent(0, 360, -80, -30)
  dat <- crop(raster(files$fullname[i], varname = "u"), ex, snap = "out")
  raadtools:::.rotate(dat)
}
read_i_v <- function(i, files) {
  ex <- extent(0, 360, -80, -30)
  dat <- crop(raster(files$fullname[i], varname = "v"), ex, snap = "out")
  raadtools:::.rotate(dat)
}

read_i_mag <- function(i, files) {
  
  ex <- extent(0, 360, -80, -30)
  vlen <- function(x, y) sqrt(x*x + y*y)
  op <- options(warn = -1)
  udat <- crop(raster(files$fullname[i], varname = "u"), ex, snap = "out")
  vdat <- crop(raster(files$fullname[i], varname = "u"), ex, snap = "out")
  options(op)
  raadtools:::.rotate(vlen(udat, vdat))
}

read_i_chl <- function(i, files) {
  ex <- extent(-180, 180, -80, -30)
  crop(raster(files$fullname[i], varname = "chlor_a"), ex)
}
library(aceecostats)
library(raadtools)
outf <- "/mnt/acebulk"
ice <- build_bulk_file(icefiles(), file.path(outf, "ice.grd"), read_i_ice, layer_prefix = "ice")
sst <- build_bulk_file(sstfiles(), file.path(outf, "sst.grd"), read_i_sst, layer_prefix = "sst")
mag <- build_bulk_file(currentsfiles(), file.path(outf, "mag.grd"), read_i_mag, layer_prefix = "mag")

outf <- "/home/shared/data/assessment/acebulk/summaries"
library(dplyr)
library(tibble)
sw <- ocfiles(time.resolution = "monthly", product = "SeaWiFS", varname = "CHL", type = "L3m", ext = "nc")
md <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m", ext = "nc")
oc <- bind_rows(sw %>% filter(date < min(md$date)), md) %>%   as_tibble()
vr <- ocfiles(time.resolution = "monthly", product = "VIIRS", varname = "SNPP_CHL", type = "L3m", ext = "nc")
oc <- bind_rows(oc %>% filter(date < min(vr$date)), vr)

system.time({
chl <- build_bulk_file(oc, file.path(outf, "chl.grd"), read_i_chl, layer_prefix = "month_chl")
})


## run each summ script, and then
## scp -r summaries/ mdsumner@144.6.224.186:/home/shared/data/assessment/acebulk/
  