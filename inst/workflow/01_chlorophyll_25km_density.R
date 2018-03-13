library(raadtools)
library(roc)
library(aceecostats)

library(dplyr)
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

files <- oc_sochla_files(product = "MODISA")

files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))

## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(files$season_segs)))


decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2018), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}


## this loop 10 summers in a decade
alldays <- tibble(date = files$date, decade = decade_maker(date), season = aes_season(date), 
                  season_year = files$season_segs)

icount <- 0
udecades <- unique(levels(alldays$decade)[alldays$decade])
useasons <- c("Spring", "Summer", "Autumn", "Winter")
prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chl25 <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                  crs = "+init=epsg:4326"), 
                                   prjj), 25000), 
                res = 25000, crs = prjj)
#devtools::install_github("hypertidy/gridcol")
#library(gridcols)
two5_cell <- function(bin) {
  cellFromXY(chl25, 
                   rgdal::project(as.matrix(tibble::as_tibble(bin2lonlat(bin, 4320))),  projection(chl25)))
}
for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(useasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
    asubs <- which(files$season_segs %in% this_decade_days$season_year)
    a_dat <- purrr::map_df(files$fullname[asubs], readRDS) %>% 
      mutate(cell25 = two5_cell(bin_num)) %>% 
      group_by(cell25)  %>% 
      
  #    group_by(bin_num)  %>% 
      summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa)) %>% 
      mutate(date = this_decade_days$date[1]) %>% 
      mutate(decade = aceecostats:::decade_maker(date), season = useasons[iseason])
    
    icount <- icount + 1
    a_dat[c("x", "y")] <- as.data.frame(xyFromCell(chl25, a_dat$cell25))
    ll <- rgdal::project(as.matrix(a_dat[, c("x", "y")]), 
                         projection(chl25), inv = TRUE)
    library(sf)
    aes_zone_ll_buff <- as(st_buffer(st_as_sf(aes_zone_ll), 0.5), "Spatial")
    ll[is.na(ll)] <- 0
    pp <- over(SpatialPoints(ll, proj4string = CRS(projection(aes_zone_ll))), aes_zone_ll_buff)
    a_dat$SectorName <- pp$SectorName
    a_dat$Zone <- pp$Zone
    a_dat <- a_dat %>% dplyr::filter(!is.na(SectorName))
    a_dat <- a_dat %>% filter(!is.na(cell25))
    
    
    if (icount == 1) {
      copy_to(db, a_dat, "chl_25k_tab", temporary = FALSE, indexes = list("cell25", "decade", "season"))
    } else {
      db_insert_into( con = db$con, table = "chl_25k_tab", values = a_dat)
    }
    print(icount)
    rm(a_dat)
    gc()
  }
}


# over_mdb <- function(x, s) {
#   ind <- as(x, "")
# }

## modify the raw tab and write a copy for the density work
#a <- tbl(db, "chl_25k_tab")   %>%  collect(n = Inf)
# ll <- rgdal::project(as.matrix(a[, c("x", "y")]), projection(chl25), inv = TRUE)
# library(sf)
# aes_zone_ll_buff <- as(st_buffer(st_as_sf(aes_zone_ll), 0.5), "Spatial")
# pp <- over(SpatialPoints(ll, proj4string = CRS(projection(aes_zone_ll))), aes_zone_ll_buff)
# a$SectorName <- pp$SectorName
# a$Zone <- pp$Zone
# a <- a %>% dplyr::filter(!is.na(SectorName))
# a <- a %>% filter(!is.na(cell25))
#a$x <- x_coord(gridcol(a$cell25, chl25))
#a$y <- y_coord(gridcol(a$cell25, chl25))
#db$con %>% db_drop_table(table='chl_25k_tab')
#copy_to(db, a, "chl_25k_tab", temporary = FALSE, indexes = list("cell25", "decade", "season"))
