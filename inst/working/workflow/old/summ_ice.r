library(raster)
outf <- "/mnt/acebulk"
obj <- brick(file.path(outf, "ice.grd"))

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

  a_obj <- readAll(subset(obj, asub))
  tab <- tabit(min(a_obj)) %>% rename(min = val) %>% mutate(date = dates[asub[1]]) %>% 
    filter(min > 0)
  tab$max<- values(max(a_obj))[tab$cell_]
  tab$mean <- values(mean(a_obj))[tab$cell_]
  listtab[[i]] <- tab
  print(i)
}

obj_tab <- bind_rows(listtab)
library(feather)
write_feather(obj_tab, "summaries/ice_tab.feather")
writeRaster(raster(obj), "summaries/ice_raster.grd")
