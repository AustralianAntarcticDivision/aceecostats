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

library(aceecostats)
library(raadtools)
outf <- "/mnt/acebulk"
ice <- build_bulk_file(icefiles(), file.path(outf, "ice.grd"), read_i_ice, layer_prefix = "ice")
sst <- build_bulk_file(sstfiles(), file.path(outf, "sst.grd"), read_i_sst, layer_prefix = "sst")
mag <- build_bulk_file(currentsfiles(), file.path(outf, "mag.grd"), read_i_mag, layer_prefix = "mag")


## run each summ script, and then
## scp -r summaries/ mdsumner@144.6.224.186:/home/shared/data/assessment/acebulk/
  