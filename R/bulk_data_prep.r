#' @importFrom raster readIniFile
#' @importFrom tibble as_tibble
read_ini <- function(inifile) {
  lapply(raster:::readIniFile(inifile, aslist = TRUE), as_tibble)
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


#' File-backed raster file.
#'
#' Build a file-backed raster from a collection of file names and dates.
#'
#'
#' The files collection must have columns \code{fullname} with the path to the file, and \code{date} with a valid date or date-time.
#' The function \code{read_i_raster} takes an integer and the files collection and returns the raster read from the i-th row.
#' The date is set on the object after this function returns, to ensure it matches the database of files, and also so that the raster function can be used as a fallback.
#' @param files data frame with `fullname` and `date`
#' @param filename database of files, see Details
#' @param read_i_raster function to read the i-th file
#' @param layer_prefix optional prefix to give each layer, pre-pended to the date
#'
#' @return RasterBrick
#' @export
#'
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
