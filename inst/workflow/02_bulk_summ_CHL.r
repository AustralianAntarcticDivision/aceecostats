library(raadtools)
library(roc)
files <- chla_johnsonfiles(product = "MODISA")
label <- "modisa"
# # ## initialize the bin logic for MODISA
init <- initbin(NUMROWS = 4320)


library(raster)
library(aceecostats)
library(tibble)
library(dplyr)
library(feather)
library(sf)
library(purrr)
outf <- "/mnt/acebulk"
db <- dplyr::src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
## put a tidy end to the series
maxdate <- ISOdatetime(2016, 12, 31, 23, 59, 59, tz = "GMT")
aes_zone_data <- aes_zone@data[, c("ID", "SectorName", "Zone")]
# ## here Continent just means High Latitude
## we trick it here so the ID overlay gets bundled together below
aes_zone_data$Zone[aes_zone_data$Zone == "Continent"] <- "High-Latitude"


## unique integer from 0 to ~nrow(sf)/90 for each three month period
#segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))

files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))

init <- initbin(NUMROWS = 4320)
## counts up from the south
maxbin <- init$totbin/2
## unique grid map cell number
#ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))

ucell <- tibble(cell_ = seq_len(maxbin), area = 4)
## classify cell index by polygon
# ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
#                  aes_zone)$ID
xy <- sp::SpatialPoints(do.call(cbind, bin2lonlat(ucell$cell_, 4320)), proj4string = CRS("+init=epsg:4326"))
ucell$ID <- over(spTransform(xy, projection(aes_zone)), 
                 aes_zone)$ID

ucell <- ucell %>% filter(!is.na(ID))
## main loop over all season-years
usegs <- distinct(files, season_segs)
sparkline_list <- vector("list", nrow(usegs))

## loop over season-years
for (i in seq_along(sparkline_list)) {
  #asub <- which(segs == unique(segs)[i])
  afiles <-  usegs[i, ] %>% inner_join(files)
  #a_obj <- setZ(readAll(subset(obj, asub)), getZ(obj)[asub])
  
  meanval <- lapply(afiles$fullname, readRDS) %>% bind_rows() %>% 
    group_by(bin_num) %>% 
    summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa))
  ## aggregate min/max for 90 days per cell
  #  meanval_map <- mean(a_obj, na.rm = TRUE)
  start_date <- min(afiles$date)
  ## mean of the min/max by region
  sparkys <-  meanval %>% 
    inner_join(ucell %>% inner_join(aes_zone_data), c("bin_num" = "cell_")) %>% 
    group_by(SectorName, Zone) %>% 
    # summarize(meanmin = mean(minval), meanmax = mean(maxval)) %>% 
    summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa)) %>% 
    mutate(season_year = start_date) %>% ungroup()
  sparkline_list[[i]] <- sparkys
  print(i)
}  

sp_line <- bind_rows(sparkline_list) %>% mutate(season = aes_season(season_year))
#db$con %>% db_drop_table(table='chl_sparkline_tab')
dplyr::copy_to(db, sp_line, "chl_sparkline_tab", temporary = FALSE)
#saveRDS(sp_line, file.path(outf, "sparky_line.rds"))

## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(segs)))

decade_labels <- decade_maker(as.Date(getZ(obj)))
season_labels <- aes_season(as.Date(getZ(obj)))
interval_labels <- segs
udecades <- levels(decade_labels)[3:4]
useasons <- c("Summer", "Winter")
## this loop 10 summers in a decade

alldays <- tibble(date = as.POSIXct(getZ(obj), tz = "GMT"), decade = decade_maker(date), season = aes_season(date), 
                  season_year = segs)


big_tab <- vector("list")
icount <- 0
idecade <- 1
iseason <- 1


for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(useasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
    #decade_years <- this_decade_days %>% split(.$season_year)
    #list_of_seasons <- vector("list", length(decade_years))
    a_obj <- readAll(subset(obj, which(segs %in% this_decade_days$season_year)))
    meanval_map <- mean(a_obj, na.rm = TRUE)
    tab <- tabit(meanval_map) 
    tab <- tab %>% mutate(date = this_decade_days$date[1]) 
    tab$count <- values(calc(!is.na(a_obj), sum, na.rm = TRUE))[tab$cell_]
    
    ## big tab (need labels back on decade)
    tab <- tab %>%  mutate(decade = decade_maker(date), season = useasons[iseason])
    
    icount <- icount + 1
    big_tab[[icount]] <- tab
    rm(tab, a_obj)
    gc()
  }
}

big_tab <- bind_rows(big_tab)
big_tab$area <- raster::extract(gridarea, big_tab$cell_)

big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)

#db$con %>% db_drop_table(table='chl_density_tab')

copy_to(db, big_tab, "chl_density_tab", temporary = FALSE)




















#  library(raadtools)
#  library(roc)
#  files <- chla_johnsonfiles(product = "MODISA")
#   label <- "modisa"
# # # ## initialize the bin logic for MODISA
#  init <- initbin(NUMROWS = 4320)
# 
# 
# library(raster)
# library(aceecostats)
# library(tibble)
# library(dplyr)
# library(feather)
# library(sf)
# library(purrr)
# outf <- "/mnt/acebulk"
# db <- dplyr::src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
# ## put a tidy end to the series
# maxdate <- ISOdatetime(2016, 12, 31, 23, 59, 59, tz = "GMT")
# aes_zone_data <- aes_zone@data[, c("ID", "SectorName", "Zone")]
# # ## here Continent just means High Latitude
# ## we trick it here so the ID overlay gets bundled together below
# aes_zone_data$Zone[aes_zone_data$Zone == "Continent"] <- "High-Latitude"
# 
# 
# ## unique integer from 0 to ~nrow(sf)/90 for each three month period
# #segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))
# 
# files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))
# 
# init <- initbin(NUMROWS = 4320)
# ## counts up from the south
# maxbin <- init$totbin/2
# ## unique grid map cell number
# #ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))
# 
# ucell <- tibble(cell_ = seq_len(maxbin), area = 4)
# ## classify cell index by polygon
# # ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
# #                  aes_zone)$ID
# xy <- sp::SpatialPoints(do.call(cbind, bin2lonlat(ucell$cell_, 4320)), proj4string = CRS("+init=epsg:4326"))
# ucell$ID <- over(spTransform(xy, projection(aes_zone)), 
#                  aes_zone)$ID
# 
# ucell <- ucell %>% filter(!is.na(ID))
# ## main loop over all season-years
# usegs <- distinct(files, season_segs)
# sparkline_list <- vector("list", nrow(usegs))
# 
# ## loop over season-years
# for (i in seq_along(sparkline_list)) {
#   #asub <- which(segs == unique(segs)[i])
#   afiles <-  usegs[i, ] %>% inner_join(files)
#   #a_obj <- setZ(readAll(subset(obj, asub)), getZ(obj)[asub])
#   
#   meanval <- lapply(afiles$fullname, readRDS) %>% bind_rows() %>% 
#     group_by(bin_num) %>% 
#     summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa))
#   ## aggregate min/max for 90 days per cell
# #  meanval_map <- mean(a_obj, na.rm = TRUE)
#   start_date <- as.POSIXct(getZ(a_obj)[1], tz = "GMT")
#   ## mean of the min/max by region
#   sparkys <-  tabit(meanval_map) %>% 
#    # inner_join(tabit(minval_map) %>% rename(minval = val)) %>% 
#     inner_join(ucell %>% inner_join(aes_zone_data)) %>% 
#     group_by(SectorName, Zone) %>% 
#    # summarize(meanmin = mean(minval), meanmax = mean(maxval)) %>% 
#     summarize(mean = mean(val)) %>% 
#     mutate(season_year = start_date)
#   sparkline_list[[i]] <- sparkys
#   print(i)
# }  
# 
# sp_line <- bind_rows(sparkline_list) %>% mutate(season = aes_season(season_year))
# #db$con %>% db_drop_table(table='chl_sparkline_tab')
# dplyr::copy_to(db, sp_line, "chl_sparkline_tab", temporary = FALSE)
# #saveRDS(sp_line, file.path(outf, "sparky_line.rds"))
# 
# ## season_year needs a formalization above (using date)
# ## collection list to store summary tables per season-i
# ## trim this to only summer and winter
# time_tab <- vector("list", length(unique(segs)))
# 
# decade_labels <- decade_maker(as.Date(getZ(obj)))
# season_labels <- aes_season(as.Date(getZ(obj)))
# interval_labels <- segs
# udecades <- levels(decade_labels)[3:4]
# useasons <- c("Summer", "Winter")
# ## this loop 10 summers in a decade
# 
# alldays <- tibble(date = as.POSIXct(getZ(obj), tz = "GMT"), decade = decade_maker(date), season = aes_season(date), 
#                   season_year = segs)
# 
# 
# big_tab <- vector("list")
# icount <- 0
# idecade <- 1
# iseason <- 1
# 
# 
# for (idecade in seq_along(udecades)) {
#   for (iseason in seq_along(useasons)) {
#     ## identify every day uniquely badged by season_year labels
#     this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
#     #decade_years <- this_decade_days %>% split(.$season_year)
#     #list_of_seasons <- vector("list", length(decade_years))
#     a_obj <- readAll(subset(obj, which(segs %in% this_decade_days$season_year)))
#     meanval_map <- mean(a_obj, na.rm = TRUE)
#     tab <- tabit(meanval_map) 
#     tab <- tab %>% mutate(date = this_decade_days$date[1]) 
#     tab$count <- values(calc(!is.na(a_obj), sum, na.rm = TRUE))[tab$cell_]
#     
#     ## big tab (need labels back on decade)
#     tab <- tab %>%  mutate(decade = decade_maker(date), season = useasons[iseason])
#     
#     icount <- icount + 1
#     big_tab[[icount]] <- tab
#     rm(tab, a_obj)
#     gc()
#   }
# }
# 
# big_tab <- bind_rows(big_tab)
# big_tab$area <- raster::extract(gridarea, big_tab$cell_)
# 
# big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)
# 
# #db$con %>% db_drop_table(table='chl_density_tab')
# 
# copy_to(db, big_tab, "chl_density_tab", temporary = FALSE)
