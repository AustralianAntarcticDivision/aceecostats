library(raster)
outf <- "/mnt/acebulk"
obj <- brick(file.path(outf, "mag.grd"))

season <-
  function(x) {
    x <- (as.POSIXlt(x)$mon) + 1L
    c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")[x]
  }

## unique integer from 0 to ~nrow(sf)/90 for each three month period
segs <- cumsum(c(0, abs(diff(unclass(factor(season(getZ(obj))))))))

library(tibble)
library(dplyr)
tabit <- function(x) {
  tibble(val = values(x), cell_ = seq(ncell(x))) %>% filter(!is.na(val))
}
listtab <- vector("list", length(unique(segs)))
dates <- as.POSIXct(getZ(obj))
for (i in seq_along(listtab)) {
  asub <- which(segs == unique(segs)[i])
  ##sst <- readsst(sf$date[asub], time.resolution = "daily", filename = tfile, overwrite = TRUE)
  a_obj <- readAll(subset(obj, asub))
  tab <- tabit(min(a_obj)) %>% rename(min_mag = val) %>% mutate(date = dates[asub[1]])
  tab$max_mag <- values(max(a_obj))[tab$cell_]
  tab$mean_mag <- values(mean(a_obj))[tab$cell_]
  listtab[[i]] <- tab
  print(i)
}

obj_tab <- bind_rows(listtab)
library(feather)
write_feather(obj_tab, "summaries/mag_tab.feather")
writeRaster(raster(obj), "summaries/mag_raster.grd")
