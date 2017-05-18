# First we pre-cache a single file of all daily data for each year after doing
# some sanity checks. This allows ease of code development and debugging.  -->


library(raadtools)
library(dplyr)
sfiles <- icefiles(hemisphere = "south")
#sfiles %>% summarize(count = n(), datemin = min(date), datemax = max(date))
# 
# # ISSUES
# Data are missing from 1987-12-04 to early Jan 1988, Massom et al. use "the climatology" but it seems pertinent to just assume we cannot calculate day of retreat for that season since the mean value can be wildly different from a given year. 
# 
# Data are two-daily from 1978 to mid 80s, we just interpolate to fill the days. 


dp <- "/mnt/acebulk/seaiceseason"
bulkfile <- "/mnt/acebulk/data/ice.grd"


# Collate a single file for each year, in a local folder. Years start at
# February 15 and end on February 14 in the southern hemisphere, and start
# August 15 and end August 14 in the northern hemisphere.

hmon <- 2
hmday <- 15
startdate <- snaptodoy(min(sfiles$date), mon = hmon, mday = hmday, start = TRUE)
endate <- snaptodoy(max(sfiles$date), mon = hmon, mday = hmday -1, start = FALSE)
hemi <- "south"
## start dates, including the end of the last year
bdates <- seq(startdate, endate, by = "1 year")

for (iyear in seq_along(head(bdates, - 1))) {
  #yearn <- (ydates %>% filter(yearN == levels(yearN)[iyear]))$date
  yearn <- seq(bdates[iyear], bdates[iyear + 1], by = "1 day")
  tfilename <- file.path(dp, "yearfiles", 
                         sprintf("%s_ice_%s_%s.grd", hemi, format(min(yearn), "%Y_%m_%d"), 
                                 format(max(yearn), "%Y_%m_%d")))
  print(iyear)
  if (!file.exists(tfilename)) {
    ice <- readIceCache(yearn, bulkfile)
    writeRaster(ice, tfilename, overwrite = TRUE)
  }
}


# 
# # Calculating ice season
# 
# Ice season is defined by a calculated day of *advance* and *retreat* for a pixel corresponding to the development of sea 
# ice cover and its subsequent melting. 
# 
# > The annual day of advance is the time when the ice concentration in a given pixel 
# > first exceeds 15% (taken to < approximate the ice edge) for at least 5 days 
# > while day of retreat is the time when concentration remains below 15% until the end 
# > of the given sea ice year. Massom et al (2013)
# 
# To calculate this we arrange an entire year of data in a single 3-dimensional array of X * Y * Time pixels. Where there is
# a break in the Time dimension each pixel's sequence in time is linearly interpolated to each calendar day. This occurs in the 
# first 12???? years of ice data where there is only one data set every two days, and once in ????1998 where there is 1.5 month gap. 
# (Note that our method diverges from that of Massom et al. who use the monthly climatology in this case). 
# 
# 
# 
# The method proceeds to threshold the ice values at 15% or greater concentration, then processing the sequence base on the rules in Massom et al. (2103). In short: 
# 
# * advance is the first day at or above the threshold after which the subsequent four days are also at or above the threshold
# * retreat is the day after which the threshold is not reached again. 


library(raadtools)
files <- list.files(file.path(dp, "yearfiles"), full.names = TRUE, pattern = "^south.*grd$")

advance <- vector("list", length(files))
retreat <- vector("list", length(files))
daycount <- vector("list", length(files))

for (i in seq_along(files)) {
  ## obj contains 2 rasters "adv" and "ret"
  obj <- calc_ice_season(files[i])
  advance[[i]] <- obj$adv
  retreat[[i]] <- obj$ret
  daycount[[i]] <- calc(readAll(brick(files[i])) > 15, mean, na.rm = TRUE) * 365
  
}


advance <- brick(stack(advance), filename = file.path(dp, "south_advance.grd"))
retreat <- brick(stack(retreat), filename = file.path(dp, "south_retreat.grd"))
daycount <- brick(stack(daycount), filename = file.path(dp, "south_daycount.grd"))
  
