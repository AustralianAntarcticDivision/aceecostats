library(raster)
outf <- "/home/shared/data/assessment/acebulk/summaries"
chl <- brick(file.path(outf, "chl.grd"))

season <-
  function(x) {
    x <- (as.POSIXlt(x)$mon) + 1L
    c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")[x]
  }

## unique integer from 0 to ~nrow(sf)/90 for each three month period
segs <- cumsum(c(0, abs(diff(unclass(factor(season(getZ(chl))))))))

library(tibble)
library(dplyr)
tabit <- function(x) {
  tibble(val = values(x), cell_ = seq(ncell(x))) %>% filter(!is.na(val))
}
listtab <- vector("list", length(unique(segs)))
dates <- as.POSIXct(getZ(chl), tz = "GMT")
for (i in seq_along(listtab)) {
  asub <- which(segs == unique(segs)[i])
  ##sst <- readsst(sf$date[asub], time.resolution = "daily", filename = tfile, overwrite = TRUE)
  a_chl <- readAll(subset(chl, asub))
  tab <- tabit(min(a_chl)) %>% rename(min_chl = val) %>% mutate(date = dates[asub[1]])
  tab$max_chl <- values(max(a_chl))[tab$cell_]
  tab$mean_chl <- values(mean(a_chl))[tab$cell_]
  listtab[[i]] <- tab
  print(i)
}

chl_tab <- bind_rows(listtab)
library(feather)
write_feather(chl_tab, file.path(outf, "chl_tab.feather"))
writeRaster(raster(chl), file.path(outf, "chl_raster.grd"))
