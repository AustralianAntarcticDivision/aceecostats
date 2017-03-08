decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2017), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}
#aes_decades <- seq(as.POSIXct("1977-01-01"), length = 5, by = "10 years")
#devtools::use_data(aes_decades, overwrite = TRUE)

library(raster)
library(aceecostats)
library(tibble)
library(dplyr)
library(feather)
library(sf)
outf <- "/mnt/acebulk"

## put a tidy end to the series
maxdate <- ISOdatetime(2016, 12, 31, 23, 59, 59, tz = "GMT")
aes_zone_data <- aes_zone@data[, c("ID", "SectorName", "Zone")]
# ## here Continent just means High Latitude
## we trick it here so the ID overlay gets bundled together below
aes_zone_data$Zone[aes_zone_data$Zone == "Continent"] <- "High-Latitude"


obj <- brick(file.path(outf, sprintf("%s.grd", "sst")))
ras <- raster(obj)
gridarea <- area(ras) * if (isLonLat(ras)) 1 else (1/1e6)

## unique integer from 0 to ~nrow(sf)/90 for each three month period
segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))

## crux of density calcs, a decade and a season
#asub <- aes_season(getZ(obj)) == "Summer" & as.integer(format(as.Date(getZ(obj)), "%Y")) < 1987
#a_obj <- readAll(subset(obj, which(asub)))

## unique grid map cell number
ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))

## classify cell index by polygon
ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
                 aes_zone)$ID

## main loop over all season-years
sparkline_list <- vector("list", length(unique(segs)))
## loop over season-years
for (i in seq_along(sparkline_list)) {
  asub <- which(segs == unique(segs)[i])
  a_obj <- setZ(readAll(subset(obj, asub)), dates[asub])
  
  par(mfrow = c(2, 1), mar = rep(0, 4))
  cols <- viridis::viridis(100)
  
  
  ## aggregate min/max for 90 days per cell
  minval_map <- min(a_obj, na.rm = TRUE)
  maxval_map <- max(a_obj, na.rm = TRUE)
  
  ## mean of the min/max by region
  sparkys <-  tabit(maxval_map) %>% rename(maxval = val)%>% 
    inner_join(tabit(minval_map) %>% rename(minval = val)) %>% 
    inner_join(ucell %>% inner_join(aes_zone_data)) %>% 
    group_by(SectorName, Zone) %>% 
    summarize(meanmin = mean(minval), meanmax = mean(maxval)) %>% 
    mutate(season_year = dates[asub[1]])
  sparkline_list[[i]] <- sparkys
  print(i)
}  

saveRDS(sp_line, "sparky_line.rds")
sp_line <- readRDS("sparky_line.rds")
library(ggplot2)
ggplot(gather(sp_line, measure, sst, -SectorName, -Zone, -season_year) %>% 
         filter(Zone == "High-Latitude", aes_season(season_year) == "Winter"), aes(x = season_year, y = sst, group = measure, colour = measure)) + geom_line() + facet_wrap(~SectorName+ Zone)

## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(segs)))
segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))

decade_labels <- decade_maker(as.Date(getZ(obj)))
season_labels <- aes_season(as.Date(getZ(obj)))
interval_labels <- segs
udecades <- levels(decade_labels)
useasons <- c("Summer", "Winter")
## this loop 10 summers in a decade

alldays <- tibble(date = as.Date(getZ(obj)), decade = decade_maker(date), season = aes_season(date), 
                  season_year = wrong_segs)


big_tab <- vector("list")
icount <- 0
library(purrr)
 for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(seasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
    decade_years <- this_decade_days %>% split(.$season_year)
    list_of_seasons <- vector("list", length(decade_years))
    
    for (iyear in seq_along(list_of_seasons)) {
      yeardays <- decade_years[[iyear]]
      a_obj <- readAll(subset(obj, which(wrong_segs %in% yeardays$season_year)))
      
      ## aggregate min/max for 90 days per cell
      minval_map <- min(a_obj, na.rm = TRUE)
      maxval_map <- max(a_obj, na.rm = TRUE)
      #  skip_on(c("autumn", "spring")) {
      
      ## all of the min/max grid values by region
      tab <- tabit(minval_map) 
      tab <- tab %>% rename(min = val) %>% mutate(date = dates[asub[1]]) 
      tab$max<- values(maxval_map)[tab$cell_]
      tab$count <- values(calc(!is.na(a_obj), sum, na.rm = TRUE))[tab$cell_]
      list_of_seasons[[iyear]] <- tab
      print(iyear)
    }
    
    ## big tab (need labels back on decade)
    tab <- bind_rows(list_of_seasons) %>% mutate(decade = decade_maker(date))
    summ <- tab %>% group_by(decade, cell_) %>% 
      summarize(min = mean(min), max  = mean(max), count = sum(count)) %>% 
      mutate(season = useasons[iseason])
    icount <- icount + 1
   big_tab[[icount]] <- summ
  rm(tab, a_obj, list_of_seasons)
  gc()
  }
 }
big_tab <- bind_rows(big_tab)
big_tab$area <- extract(gridarea, big_tab$cell_)

big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)
saveRDS(big_tab, "decadal_tabs.rds", compress = "xz")

file.copy(c("decadal_tabs.rds", "sparky_line.rds"), file.path("/tmp", c("decadal_tabs.rds", "sparky_line.rds")))


