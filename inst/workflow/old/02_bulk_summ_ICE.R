
#aes_decades <- seq(as.POSIXct("1977-01-01"), length = 5, by = "10 years")
#devtools::use_data(aes_decades, overwrite = TRUE)

library(raster)
library(aceecostats)
library(tibble)
library(dplyr)
library(sf)
outf <- "/home/copy_aes/data"
#db <- dplyr::src_sqlite(file.path(outf, "habitat_assessment_output.sqlite3"), create = TRUE)
db <- dplyr::src_sqlite(file.path(outf, "habitat_assessment_output.sqlite3"))
## put a tidy end to the series
maxdate <- ISOdatetime(2016, 12, 31, 23, 59, 59, tz = "GMT")
aes_zone_data <- aes_zone@data[, c("ID", "SectorName", "Zone")]
# ## here Continent just means High Latitude
## we trick it here so the ID overlay gets bundled together below
aes_zone_data$Zone[aes_zone_data$Zone == "Continent"] <- "High-Latitude"


obj <- brick(file.path(outf, sprintf("%s.grd", "ice")))
dates <- as.Date(getZ(obj))
ras <- raster(obj)
gridarea <- area(ras) * if (isLonLat(ras)) 1 else (1/1e6)

## unique integer from 0 to ~nrow(sf)/90 for each three month period
segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))


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
  val_map <- calc(a_obj > 15, mean, na.rm = TRUE)
  sparkys <-  tabit(val_map) %>% #rename(maxval = val)%>% 
    #inner_join(tabit(minval_map) %>% rename(minval = val)) %>% 
    #inner_join(tabit(meanval_map) %>% rename(meanval = val)) %>% 
    inner_join(ucell %>% inner_join(aes_zone_data)) %>% 
    group_by(SectorName, Zone) %>% 
    summarize(val = mean(val)) %>% 
    #summarize(meanmin = mean(minval), meanmax = mean(maxval)) %>% 
    mutate(season_year = dates[asub[1]])
  
  sparkline_list[[i]] <- sparkys
  print(i)
}  

sp_line <- bind_rows(sparkline_list)  %>% mutate(season = aes_season(season_year))
#db$con %>% db_drop_table(table='ice_minmaxmean_sparkline_tab')
#dplyr::copy_to(db, sp_line, "ice_minmaxmean_sparkline_tab", temporary = FALSE)
#saveRDS(sp_line, file.path(outf, "sparky_line.rds"))
library(ggplot2)
ggplot(sp_line %>% filter(Zone == "High-Latitude", season %in% c("Summer", "Winter")), 
       aes(x  = season_year,  y = val, colour = season)) +
  geom_line() + facet_wrap(~SectorName)

## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(segs)))
segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))

decade_labels <- decade_maker(as.Date(getZ(obj)))
season_labels <- aes_season(as.Date(getZ(obj)))
interval_labels <- segs
udecades <- levels(decade_labels)
useasons <- c("Summer", "Autumn", "Winter", "Spring")
## this loop 10 summers in a decade

alldays <- tibble(date = as.Date(getZ(obj)), decade = decade_maker(date), season = aes_season(date), 
                  season_year = segs)


big_tab <- vector("list")
icount <- 0
idecade <- 1
iseason <- 1
iyear <- 1
library(purrr)
 for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(useasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
    decade_years <- this_decade_days %>% split(.$season_year)
    list_of_seasons <- vector("list", length(decade_years))
    
    for (iyear in seq_along(list_of_seasons)) {
      yeardays <- decade_years[[iyear]]
      a_obj <- readAll(subset(obj, which(segs %in% yeardays$season_year)))
      
      ## aggregate min/max for 90 days per cell
      minval_map <- calc(a_obj, min, na.rm = TRUE)
      maxval_map <- calc(a_obj, max, na.rm = TRUE)
      meanval_map <- calc(a_obj, mean, na.rm = TRUE)
      
      ## all of the min/max grid values by region
      tab <- tabit(minval_map) 
      tab <- tab %>% rename(minal = val) %>% mutate(date = yeardays$date[1]) 
      tab$max <- values(maxval_map)[tab$cell_]
      tab$mean <- values(meanval_map)[tab$cell_]

      list_of_seasons[[iyear]] <- tab
      print(iyear)
    }
    
    ## big tab (need labels back on decade)
    tab <- bind_rows(list_of_seasons) %>% mutate(decade = decade_maker(date))
    summ <- tab %>% group_by(decade, cell_) %>% 
      summarize(minval = mean(min), maxval = mean(max), meanval = mean(mean)) %>% 
      mutate(season = useasons[iseason])
    icount <- icount + 1
   big_tab[[icount]] <- summ
  rm(tab, a_obj, list_of_seasons)
  gc()
  }
 }
big_tab <- bind_rows(big_tab)
big_tab$area <- raster::extract(gridarea, big_tab$cell_)

big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)
#db$con %>% db_drop_table(table='ice_minmaxmean_density_tab')
copy_to(db, big_tab, "ice_minmaxmean_density_tab", temporary = FALSE)


