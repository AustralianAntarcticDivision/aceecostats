
#' Decade classification
#' 
#' 
#' This changes over time, but currently it's clean on 10 year boundaries (1977-2017). 
#' @param x date-time vector
#'
#' @return classified factor vector
#' @export
#'
decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2017), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}

#' @importFrom raster readIniFile
#' @importFrom tibble as_tibble
read_ini <- function(inifile) {
  lapply(raster::readIniFile(inifile, aslist = TRUE), as_tibble)
}

write_ini <- function(x, inifile) {
  text <- character()
  names_x <- names(x)
  for (i in seq_along(x)) {
    text <- c(text, sprintf("[%s]", names_x[i]))
    for (j in seq(ncol(x[[i]]))) {
      text <- c(text, name_equals(names(x[[i]])[j], x[[i]][[j]]))
    }
  }
  writeLines(text, inifile)
}

name_equals <- function(x, y) {
  sprintf("%s=%s", x, y)
}

raad_file_ini <- function(files, template, prefix = "lyr") {
  inifile <- dummy_ini_path(template)
  ini <- read_ini(inifile)

  ini$data$minvalue <- paste(rep(ini$data$minvalue, nrow(files)), collapse = ":")
  ini$data$maxvalue <- paste(rep(ini$data$maxvalue, nrow(files)), collapse = ":")
  ini$description$layername <- paste(sprintf("%s_%s", prefix,  format(files$date, "%Y-%m-%d")), collapse = ":")
  ini$description$zvalues <- sprintf("time:%s", paste(format(files$date, "%Y-%m-%d"), collapse = ":"))
  ini$data$nbands <- nrow(files)
  ini$data$bandorder <- "BSQ"
  ini
}
#' @importFrom raster writeRaster
dummy_ini_path <- function(grid) {
  temp <- sprintf("%s.grd", tempfile())
  writeRaster(grid, temp, overwrite = TRUE, datatype = "FLT4S", bandorder = "BSQ")
  temp
}




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
#' @export
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



#' File-backed raster file.
#'
#' Build a file-backed raster from a collection of file names and dates.
#'
#'
#' The files collection must have columns `fullname` with the path to the file, and `date` with a valid date or date-time.
#' The function `read_i_raster` takes an integer and the files collection and returns the raster read from the i-th row.
#' The date is set on the object after this function returns, to ensure it matches the database of files, and also so that the raster function can be used as a fallback.
#' @param files data frame with `fullname` and `date`
#' @param filename database of files, see Details
#' @param read_i_raster function to read the i-th file
#' @param layer_prefix optional prefix to give each layer, pre-pended to the date
#'
#' @return RasterBrick
#' @export
#' @importFrom raster brick setZ values
build_bulk_file <- function(files, filename, read_i_raster = NULL, layer_prefix = "lyr") {
  if (is.null(read_i_raster)) read_i_raster <- raster::raster
  ti <- gsub("d$", "i", filename)
  bincon <- file(ti, open = "wb")
  for (i in seq(nrow(files))) {
    data0 <- setZ(read_i_raster(i, files), files$date[i])
    writeBin(as.vector(values(data0)), bincon,  size = 4)
    if (i == 1) {
      ini <- raad_file_ini(files, data0, prefix = layer_prefix)
      write_ini(ini, filename)
    }
  }
  close(bincon)
  brick(filename)
}
