
tabit <- function(x) {
  tibble(val = values(x), cell_ = seq(ncell(x))) %>% filter(!is.na(val))
}

decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1981, 1990, 1999, 2008, 2016), lab = c("1981-1990", "1990-1999","1999-2008", "2008-2016"))
}

library(raster)
library(tibble)
library(dplyr)
outf <- "/mnt/acebulk"
library(aceecostats)
library(feather)
aes_region$index <- seq(nrow(aes_region))
aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")

## put a tidy end to the series
maxdate <- ISOdatetime(2016, 9, 1, 0, 0, 0, tz = "GMT")


vars <- c("ice", "sst", "chl")
for (ivar in seq_along(vars)) {
  
  obj <- brick(file.path(outf, sprintf("%s.grd", vars[ivar])))
  ras <- raster(obj)
  ## unique integer from 0 to ~nrow(sf)/90 for each three month period
  segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))
  
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

  ## now process the summaries down
  
  cell_tab <- bind_rows(listtab) %>% 
    mutate(decade = decade_maker(date)) %>% 
    filter(date <  maxdate) %>% 
    filter(!is.na(decade))
  
  ucell <- distinct(cell_tab, cell_)
  ucell$index <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_region)), 
                      aes_region)$index
  
  ## summ_tab is the mean values over time
  summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
    mutate(Season = aes_season(date)) %>% 
    group_by(Season, Zone, decade, SectorName,  date) %>%
    summarize(min = mean(min), max = mean(max)) %>% 
    ungroup()
  
  ## raw_tab is all the cell values for density plots
  raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
    mutate(Season = aes_season(date))
  
  write_feather(obj_tab, sprintf("summaries/%s_cell_tab.feather", vars[ivar]))
  writeRaster(ras, sprintf("summaries/%s_raster.grd", vars[ivar]))
  write_feather(summ_tab, sprintf("summaries/%s_summ_tab.feather", vars[ivar]))
  write_feather(raw_tab, sprintf("summaries/%s_raw_tab.feather", vars[ivar]))
}

