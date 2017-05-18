#' function to ensure we have clean boundaries
#'
#' @export
snaptodoy <- function(date, mon = 2, mday = 15, start = TRUE) {
  ldate <- as.POSIXlt(date)
  doy <- as.integer(format(ISOdatetime(ldate$year + 1900, mon, mday, 0, 0, 0, tz = "UTC"), "%j"))
  test <- ldate$yday > doy 
  if (start) {
    if (test) {ldate$mday <- mday; ldate$mon <- mon - 1; ldate$year <- ldate$year + 1}
    if (!test) {ldate$mday <- mday; ldate$mon <- mon - 1}
  } else {
    if (test) {ldate$mday <- mday; ldate$mon <- mon - 1}
    if (!test) {ldate$mday <- mday; ldate$mon <- mon + 1; ldate$year <- ldate$year - 1}
    
  }
  as.POSIXct(ldate)
}

#'  Read ice cache
#'  
#'  function to read a series of time slices, and interpolate a full sequence of days
#'  
#'  this version of readIceBrick replaces it to get it from ice.grd
#' @param dates dates to return
#' @param filename bulk file source
#'
#' @export
#' @name readIceCache
readIceCache <- function(dates, filename) {
  #x <- suppressWarnings(readice(dates, hemisphere = hemisphere,  setNA = FALSE))
  x <- brick(filename)
  bulkdates <- as.Date(getZ(x))
  
  d_dates <- as.Date(dates)
  idx <- which(bulkdates >= min(d_dates) & bulkdates <= max(d_dates))
  x <- readAll(raster::subset(x, idx))
  t0 <- bulkdates[idx]
  x <- setZ(x, bulkdates[idx])
  
  if (nlayers(x) == length(dates)) return(x)
  
  xmat <- values(x)
  xmat[xmat > 100] <- 0
  xmat[is.na(xmat)] <- 0
  dimnames(xmat) <- list(NULL, NULL)
  
  ## rebuild the entire data set with linear interpolation
  xmat2 <- matrix(0, nrow(xmat), length(dates))

  for (i in seq_len(nrow(xmat2))) {
    xmat2[i,] <- approxfun(t0, xmat[i,], rule = 2)(dates)
  }
  x <- setValues(x, xmat2)
  ##names(x) <- sprintf("layer%0.3i", seq(nlayers(x)))
  names(x) <- format(dates, "%Y_%m_%d")
  setZ(x, dates)
  
}


#'  function to read a series of time slices, and interpolate a full sequence optionally
#'  
readIceBrick <- function(dates, hemisphere = "south", interpolate = TRUE) {
  x <- suppressWarnings(readice(dates, hemisphere = hemisphere,  setNA = FALSE))
  ## we need to interpolate (or insert monthly clim)
  if (!interpolate) return(x)
  if (nlayers(x) == length(dates)) return(x)
  
  xmat <- values(x)
  xmat[xmat > 100] <- 0
  dimnames(xmat) <- list(NULL, NULL)
  
  ## rebuild the entire data set with linear interpolation
  xmat2 <- matrix(0, nrow(xmat), length(dates))
  t0 <- timedateFrom(getZ(x))
  for (i in seq_len(nrow(xmat2))) {
    xmat2[i,] <- approxfun(t0, xmat[i,], rule = 2)(dates)
  }
  x <- setValues(x, xmat2)
  ##names(x) <- sprintf("layer%0.3i", seq(nlayers(x)))
  names(x) <- format(dates, "%Y_%m_%d")
  setZ(x, dates)
  
}


#' actually calculate the ice season
#' @export
calc_ice_season <- function(yfile, threshval = 15) {
  hemi <- substr(basename(yfile[1]), 1, 5)
  threshold.value <- threshval
  ndays <- 5
  
  len <- 15
  
  ## north_ice_1979_02_15_1980_02_14.grd"
  ice <- brick(yfile)
  year_n <- nlayers(ice)
  template <- ice[[1]] * 0
  icemat <- values(ice)
  icemat[icemat > 100] <- 0
  
  ## here we need to get the next day for the interpolation . . .
  ##icemat[is.na(icemat)] <- 0
  adv <- numeric(nrow(icemat))
  ret <- numeric(nrow(icemat))
  threshold <- icemat >= threshold.value
  
  
  rsum <- .rowSums(threshold, nrow(threshold), ncol(threshold))
  ## all values less
  alllt <- rsum == 0
  ## all values greater than threshold
  allgt <- rsum == ncol(threshold)
  ## values missing
  ##miss <- icemat[,1] > 100
  ## all the rest
  visit <- which(!alllt & !allgt)
  for (ii in seq_along(visit)) {
    rl <- rle(threshold[visit[ii], ])
    
    ##    annual day of advance is the time when the ice
    ##    concentration in a given pixel first exceeds 15% (taken to
    ##    approximate the ice edge) for at least 5 days
    for (ir in seq_along(rl$lengths)) {
      if (rl$values[ir] & rl$lengths[ir] >= ndays) {
        adv[visit[ii]] <- if (ir == 1) 1L else sum(head(rl$lengths, ir - 1))
        break;
      }
    }
    if (adv[visit[ii]] == 0) adv[visit[ii]] <- NA
    
    ##while day of retreat
    ## ## is the time when concentration remains below 15% until the end
    ## ## of the given sea ice year
    revlengths <- rev(rl$lengths)
    revvals <- rev(rl$values)
    for (ri in seq_along(revlengths)) {
      if (revvals[ri]) {
        ret[visit[ii]] <- if (ri == 1) length(year_n) else sum(revlengths[ri:length(revlengths)])
        break;
      }
    }
    if (ret[visit[ii]] == 0) ret[visit[ii]] <- NA
  }
  adv[alllt] <- NA
  ## adv[miss] <- NA
  adv[allgt] <- 1
  ret[alllt] <- NA
  ret[allgt] <- length(year_n)
  
  list(adv = setValues(template, adv), ret = setValues(template, ret))
  
}