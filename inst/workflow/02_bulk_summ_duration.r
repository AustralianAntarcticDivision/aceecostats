outf <- "/mnt/acebulk"

## SEA ICE DURATION

library(raster)
library(tibble)
library(dplyr)

library(aceecostats)

gridarea <- readRDS(file.path(outf,"nsidc_south_area.rds"))/1e6
## put a tidy end to the series
maxdate <- ISOdatetime(2016, 9, 1, 0, 0, 0, tz = "GMT")
## load previously calculated sea ice season metrics (seaiceson_southern_2016.Rmd)
library(raster)
outf <- "/mnt/acebulk"
ret <- readRDS(file.path(outf, "south_retreat.rds"))

adv <- readRDS(file.path(outf,"south_advance.rds") )
duration <- ret - adv
## if retreat is equal to one, it didn't retreat
duration[ret == 1] <- 365
obj <- setZ(duration, ISOdatetime(1979:2015, 2, 15, 0, 0, 0, tz = "GMT"))
rm(ret, adv)

# 
# vars <- "seaice_duration_raster" 
# obj <- brick(file.path(outf, sprintf("%s.grd", vars)))
# obj <- setZ(obj, ISOdatetime(1979:2015, 2, 15, 0, 0, 0, tz = "GMT"))

ras <- raster(obj)

## unique integer from 0 to ~nrow(sf)/90 for each three month period
#segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))

listtab <- vector("list", nlayers(obj))
dates <- as.POSIXct(getZ(obj))
for (i in seq_along(listtab)) {
  asub <- i # which(segs == unique(segs)[i])
  a_obj <- subset(obj, asub)
  
  tab <- tabit(a_obj) %>% rename(dur = val) %>% mutate(date = dates[asub[1]])  
  #filter(dur > 0)
  tab$dur[tab$dur < 30] <- 365
  # tab$max<- values(max(a_obj))[tab$cell_]
  #  tab$mean <- values(mean(a_obj))[tab$cell_]
  listtab[[i]] <- tab
  print(i)
}

## now process the summaries down
decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2017), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}
cell_tab <- bind_rows(listtab) %>% 
  mutate(decade = decade_maker(date)) %>% 
  filter(date <  maxdate) %>% 
  filter(!is.na(decade))

ucell <- distinct(cell_tab, cell_) %>% mutate(area = raster::extract(gridarea[[1]], cell_))
ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
                    aes_zone)$ID

## summ_tab is the mean values over time
summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone@data[, c("ID", "SectorName", "Zone")])) %>% 
  #mutate(Season = aes_season(date)) %>% 
  group_by(Zone, decade, SectorName,  date) %>%
  summarize(min = min(dur), max = max(dur), mean = mean(dur)) %>% 
  ungroup()

#cell_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) 

## raw_tab is all the cell values for density plots
raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_zone@data[, c("ID", "SectorName", "Zone")])) 


write_feather(cell_tab,  file.path(outf, "seaice_duration_cell_tab.feather"))
writeRaster(ras,        file.path(outf, "seaice_duration_raster.grd"))
write_feather(summ_tab, file.path(outf, "seaice_duration_summ_tab.feather"))
write_feather(raw_tab,  file.path(outf, "seaice_duration_raw_tab.feather"))


