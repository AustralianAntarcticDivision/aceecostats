## required extension packages
## tibble and dplyr for data frame manipulation
library(tibble)
library(dplyr)

## custom functions for this study
library(aceecostats)
## custom data read functions
library(raadtools)



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


## specify a working folder
outf <- "/mnt/acebulk/retention"
#dir.create(outf)
library(raadtools)
uvel <- build_bulk_file(currentsfiles(), file.path(outf, "u.grd"), read_i_u, layer_prefix = "uvel")


