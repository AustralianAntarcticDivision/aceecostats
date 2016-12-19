

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

  gridarea <- area(ras)/1e6
  ## unique integer from 0 to ~nrow(sf)/90 for each three month period
  segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))
  
  cell_tab <- vector("list", length(unique(segs)))
  dates <- as.POSIXct(getZ(obj))

  for (i in seq_along(cell_tab)) {
    asub <- which(segs == unique(segs)[i])
    a_obj <- setZ(readAll(subset(obj, asub)), dates[asub])
    tab <- tabit(min(a_obj, na.rm = TRUE)) %>% rename(min = val) %>% mutate(date = dates[asub[1]]) 
    #%>% 
    #  filter(min > 0)
    tab$max<- values(max(a_obj, na.rm = TRUE))[tab$cell_]
    tab$mean <- values(mean(a_obj, na.rm = TRUE))[tab$cell_]
    tab$count <- values(calc(a_obj > 0, sum, na.rm = TRUE))[tab$cell_]
    cell_tab[[i]] <- tab
    print(i)
  }

  ## now process the summaries down
  
  cell_tab <- bind_rows(cell_tab) %>% 
    mutate(decade = decade_maker(date)) %>% 
    filter(date <  maxdate) %>% 
    filter(!is.na(decade))
  
  ucell <- distinct(cell_tab, cell_) %>% mutate(area = extract(gridarea, cell_))
  ucell$index <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_region)), 
                      aes_region)$index
  
  ## summ_tab is the mean values over time
  summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
    mutate(Season = aes_season(date)) %>% 
    group_by(Season, Zone, decade, SectorName,  date) %>%
    summarize(min = mean(min), max = mean(max), count = mean(count)) %>% 
    ungroup()
  
  ## raw_tab is all the cell values for density plots
  raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
    mutate(Season = aes_season(date))
  
  write_feather(cell_tab,  file.path(outf, sprintf("%s_cell_tab.feather", vars[ivar])))
  rm(cell_tab)
 # writeRaster(ras,        file.path(outf, sprintf("%s_raster.grd", vars[ivar])))
  write_feather(summ_tab, file.path(outf, sprintf("%s_summ_tab.feather", vars[ivar])))
  rm(summ_tab)
  write_feather(raw_tab,  file.path(outf, sprintf("%s_raw_tab.feather", vars[ivar])))
  rm(raw_tab)
}





## SEA ICE DURATION

library(raster)
library(tibble)
library(dplyr)

library(aceecostats)
library(feather)
aes_region$index <- seq(nrow(aes_region))
aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")
gridarea <- readRDS(file.path(outf,"nsidc_south_area.rds"))/1e6
## put a tidy end to the series
maxdate <- ISOdatetime(2016, 9, 1, 0, 0, 0, tz = "GMT")

vars <- "seaice_duration" 
ivar <- 1
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

cell_tab <- bind_rows(listtab) %>% 
  mutate(decade = decade_maker(date)) %>% 
  filter(date <  maxdate) %>% 
  filter(!is.na(decade))

ucell <- distinct(cell_tab, cell_) %>% mutate(area = extract(gridarea, cell_))
ucell$index <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_region)), 
                    aes_region)$index

## summ_tab is the mean values over time
summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
  #mutate(Season = aes_season(date)) %>% 
  group_by(Shelf, Zone, decade, SectorName,  date) %>%
  summarize(min = min(dur), max = max(dur), mean = mean(dur)) %>% 
  ungroup()

#cell_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) 

## raw_tab is all the cell values for density plots
raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) 
#%>%     mutate(Season = aes_season(date))

write_feather(cell_tab,  file.path(outf, "seaice_duration_cell_tab.feather"))
writeRaster(ras,        file.path(outf, "seaice_duration_raster.grd"))
write_feather(summ_tab, file.path(outf, "seaice_duration_summ_tab.feather"))
write_feather(raw_tab,  file.path(outf, "seaice_duration_raw_tab.feather"))


