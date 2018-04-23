library(raster)
library(aceecostats)
library(dplyr)
library(raadtools)
files <- oc_sochla_files(product = "MODISA")

prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
chlgrid <- raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                           crs = "+init=epsg:4326"), 
                                                    prjj), 25000), 
                  res = 25000, crs = prjj)


default_grid <- function() {
  prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
  raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                             crs = "+init=epsg:4326"), 
                                                      prjj), 25000), 
                    res = 25000, crs = prjj)
  
  
}
chlfiles <- files %>% dplyr::filter(format(date, "%m") %in% c("12", "01", "02")) %>%  mutate(month = as.Date(format(date, "%Y-%m-15")))

two5_cell <- function(bin) {
  cellFromXY(chlgrid, 
             rgdal::project(as.matrix(tibble::as_tibble(croc::bin2lonlat(bin, 4320))),  projection(chlgrid)))
}

northmostcell <- croc::lonlat2bin(-180, 0, 4320)

read_summarize <- function(fullname) {
  fullname %>% purrr::map_df(~readRDS(.x)) %>% dplyr::filter(bin_num < northmostcell) %>%  mutate(cell25 = two5_cell(bin_num)) %>% 
    group_by(cell25)  %>% 
    summarize(chla_johnson = mean(chla_johnson)) 
  
}

umonth <- unique(chlfiles$month)

filelist <- split(chlfiles, chlfiles$month)[as.character(umonth)]
library(future.apply)
plan(multiprocess)
l <- future_lapply(filelist, function(xa) read_summarize(xa$fullname))
d <- dplyr::bind_rows(l, .id = "monthid")

d$month <- as.Date(d$monthid)
d$monthid <- NULL




sstfiles <- raadfiles::oisst_monthly_files() %>% 
  dplyr::filter(date > as.POSIXct("2002-07-03"), format(date, "%m") %in% c("12", "01", "02")) 
sstfiles$month <- as.Date(format(sstfiles$date, "%Y-%m-15"))

l <- vector("list", nrow(sstfiles)) 
umonthsst <- unique(sstfiles$month)
for (i in seq_along(l)) {
  sst <- raster::raster(sstfiles$fullname[1], band = sstfiles$band[i], varname = "sst")
  psst <- projectRaster(sst, defaultgrid())
  ##ice <- readice_monthly(format(sstfiles$date[i], "%Y-%m-15"))
  l [[i]] <- psst
}

dsst <- dplyr::bind_rows(lapply(l, tabularaster::as_tibble), .id = "monthid")
dsst$month <- umonthsst[as.integer(dsst$monthid)]
dsst <- transmute(dsst, sst = cellvalue, cell25 = cellindex, month = month)

#d <- tbl(db, "chl_sst_25k_monthly") %>% dplyr::select(cell25, chla_johnson, month) %>% collect(Inf)
#d$month <- as.Date("1970-01-01") + d$month


d2 <- left_join(dsst, d)


dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))
db$con %>% db_drop_table(table='chl_sst_25k_monthly')
copy_to(db, d2, "chl_sst_25k_monthly", temporary = FALSE, indexes = list("cell25", "month"))

sst_from_db <- function(db, amonth) {
 # amonth <- as.Date(format(amonth, "%Y-%m-15"))
 tab <-  tbl(db, "chl_sst_25k_monthly") %>% 
    dplyr::select(cell25, sst, month) %>% 
    dplyr::filter(month == amonth) %>% dplyr::collect(Inf)
  
  g <- setValues(default_grid(), NA)
  g[tab$cell25] <- tab$sst
  g
  
}


chl_from_db <- function(db, amonth) {
 # amonth <- as.Date(format(amonth, "%Y-%m-15"))
  tab <-  tbl(db, "chl_sst_25k_monthly") %>% 
    dplyr::select(cell25, chla_johnson, month) %>% 
    dplyr::filter(month == amonth) %>% dplyr::collect(Inf)
  
  g <- setValues(default_grid(), NA)
  g[tab$cell25] <- tab$chla_johnson
  g
  
}


chl <- chl_from_db(db, as.integer(as.Date("2002-12-15")))
sst <- sst_from_db(db,  as.integer(as.Date("2002-12-15")))
