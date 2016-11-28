library(raster)
outf <- "/mnt/acebulk"
sst <- brick(file.path(outf, "sst.grd"))

season <-
  function(x) {
    x <- (as.POSIXlt(x)$mon) + 1L
    c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")[x]
  }

## unique integer from 0 to ~nrow(sf)/90 for each three month period
segs <- cumsum(c(0, abs(diff(unclass(factor(season(getZ(sst))))))))

library(tibble)
library(dplyr)
tabit <- function(x) {
  tibble(val = values(x), cell_ = seq(ncell(x))) %>% filter(!is.na(val))
}
listtab <- vector("list", length(unique(segs)))
dates <- as.POSIXct(getZ(sst), tz = "GMT")
for (i in seq_along(listtab)) {
  asub <- which(segs == unique(segs)[i])
  ##sst <- readsst(sf$date[asub], time.resolution = "daily", filename = tfile, overwrite = TRUE)
  a_sst <- readAll(subset(sst, asub))
  tab <- tabit(min(a_sst)) %>% rename(min_sst = val) %>% mutate(date = dates[asub[1]])
  tab$max_sst <- values(max(a_sst))[tab$cell_]
  tab$mean_sst <- values(mean(a_sst))[tab$cell_]
  listtab[[i]] <- tab
  print(i)
}

sst_tab <- bind_rows(listtab)
library(feather)
write_feather(sst_tab, "summaries/sst_tab.feather")
writeRaster(raster(obj), "summaries/sst_raster.grd")
