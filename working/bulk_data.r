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
files <- icefiles()[1:10, ]
ice <- build_bulk_file(files, tf, read_i_ice)
files <- tail(sstfiles(), 15)
sst <- build_bulk_file(files, "sst.grd", read_i_sst)
files <- currentsfiles()
#u <- build_bulk_file(files, "u.grd", read_i_u)
#v <- build_bulk_file(files, "v.grd", read_i_v, layer_prefix = "v")
mag <- build_bulk_file(files, "bsq/mag_south.grd", read_i_mag, layer_prefix = "mag")

bsq <- brick(tf)
## convert to BIL if needed
#system.time({bil <- writeRaster(bsq, filename = "bil.grd", bandorder = "BIL", datatype = "FLT4S")})

