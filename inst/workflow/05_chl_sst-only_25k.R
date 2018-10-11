library(raster)
library(aceecostats)
library(dplyr)
library(raadtools)
## CHLA-RJ and OISST (daily)
chlfiles <- oc_sochla_files(product = "MODISA")  %>% 
  #dplyr::filter(format(date, "%m") %in% c("12", "01", "02")) %>%  
  mutate(month = as.Date(format(date, "%Y-%m-15")))

sstfiles <- raadfiles::oisst_daily_files() %>% 
  dplyr::filter(between(date, min(chlfiles$date), max(chlfiles$date))) %>%  #, format(date, "%m") %in% c("12", "01", "02"))  %>% 
  mutate(month = as.Date(format(date, "%Y-%m-15")))


## KD490 and PAR (monthly)
kdfiles <- raadtools::ocfiles(time.resolution = "monthly", product = "MODISA", varname = "KD490", type = "L3m")
parfiles <- raadtools::ocfiles(time.resolution = "monthly", product = "MODISA", varname = "PAR", type = "L3m")


default_grid <- function() {
  prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
  raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                             crs = "+init=epsg:4326"), 
                                                      prjj), 25000), 
                    res = 25000, crs = prjj)
  
  
}

two5_cell <- function(bin) {
  cellFromXY(default_grid(), 
             rgdal::project(as.matrix(tibble::as_tibble(croc::bin2lonlat(bin, 4320))),  
                            projection(default_grid())))
}


read_summarize_chl <- function(fullname) {
  x <- fullname %>% purrr::map_df(~readRDS(.x)) %>%  
    mutate(cell25 = two5_cell(bin_num)) %>% 
    group_by(cell25)  %>% 
    summarize(chla_johnson = mean(chla_johnson, na.rm = TRUE), 
              chla_nasa = mean(chla_nasa, na.rm = TRUE)) 
  x <- dplyr::filter(x, !is.na(cell25))
  out1 <- setValues(default_grid(), NA)
  out1[x$cell25] <- x$chla_johnson
  out2 <- setValues(default_grid(), NA)
  out2[x$cell25] <- x$chla_nasa
  
  brick(out1, out2)
  
}


read_sst <- function(file) {
  sst <- raster::raster(file, varname = "sst") 
  ice <- raster::raster(file, varname = "ice")
  sst <- crop(sst, extent(0, 360, -90, 0))
  ice <- crop(ice, extent(0, 360, -90, 0))
  sst[!is.na(raster::values(ice))] <- NA
  sst
}
read_summarize_sst_med <- function(fullname) {
  sst <- calc(raster::brick(purrr::map(fullname, read_sst)), median, na.rm = TRUE)
  projectRaster(raster::rotate(sst), default_grid())
}
read_summarize_sst_min <- function(fullname) {
  sst <- calc(raster::brick(purrr::map(fullname, read_sst)), min, na.rm = TRUE)
  projectRaster(raster::rotate(sst), default_grid())
}
read_summarize_sst_max <- function(fullname) {
  sst <- calc(raster::brick(purrr::map(fullname, read_sst)), max, na.rm = TRUE)
  projectRaster(raster::rotate(sst), default_grid())
}
missings <- function(sst, chl) {
  which(is.na(raster::values(chl)) & !is.na(raster::values(sst)))
}


umonth <- unique(chlfiles$month)

chlfilelist <- split(chlfiles, chlfiles$month)[as.character(umonth)]
sstfilelist <- split(sstfiles, sstfiles$month)[as.character(umonth)]
parfilelist <- parfiles$fullname[findInterval(as.POSIXct(umonth), parfiles$date)]
kdfilelist <-  kdfiles$fullname[findInterval(as.POSIXct(umonth), kdfiles$date)]
## figure out append code for db to build table from scratch
library(nabor)
l <- vector("list", length(umonth))
for (i in seq_along(umonth)) {
  #chl <- read_summarize_chl(chlfilelist[[i]]$fullname)
  #chl_johnson <- chl[[1]]
  #chl_nasa <- chl[[2]]
  sst_med <- read_summarize_sst_med(sstfilelist[[i]]$fullname)
  sst_min <- read_summarize_sst_min(sstfilelist[[i]]$fullname)
  sst_max <- read_summarize_sst_max(sstfilelist[[i]]$fullname)
  
  # par <- projectRaster(raster(parfilelist[i]),
  #                      default_grid())
  # 
  # vv <- missings(par, chl_johnson)
  # xy <- tabularaster::as_tibble(chl_johnson, xy = TRUE) %>% dplyr::filter(!is.na(cellvalue))
  # knn <- WKNNF(as.matrix(xy[c("x", "y")]))
  # kq <- knn$query(xyFromCell(chl_johnson, vv), k = 1, eps = 0, radius = 0)
  # chl_johnson[vv] <- xy$cellvalue[kq$nn.idx]
  # 
  # 
  # kd <- projectRaster(raster(kdfilelist[i]), 
  #                     default_grid())
  # 
  # vv <- missings(par, kd)
  # xy <- tabularaster::as_tibble(kd, xy = TRUE) %>% 
  #   dplyr::filter(!is.na(cellvalue))
  # library(nabor)
  # knn <- WKNNF(as.matrix(xy[c("x", "y")]))
  # kq <- knn$query(xyFromCell(kd, vv), k = 1, eps = 0, radius = 0)
  # kd[vv] <- xy$cellvalue[kq$nn.idx]
  # 
  
  l[[i]] <- tibble(sst_med = raster::values(sst_med),
                   sst_min = raster::values(sst_min), 
                   sst_max = raster::values(sst_max),
                   #chla_johnson = raster::values(chl_johnson), 
                  # chla_nasa = raster::values(chl_nasa),
                   #kd490 = raster::values(kd), 
                   #par = raster::values(par), 
                   cell = seq_len(ncell(sst_med)))  
  print(i)
}


## START HERE
#d$prod <- croc::prod_BeFa(d$chla_nasa, d$par, d$sst, d$daylength)

d <- dplyr::bind_rows(l, .id = "monthid")
d$date <- umonth[as.integer(d$monthid)]
#ll <- rgdal::project(xyFromCell(default_grid(), d$cell), projection(default_grid()), inv = TRUE)
#d$daylength <- croc:::daylength(ll[,2], as.integer(format(d$date, "%j")))
#d$monthid <- NULL
## post-hoc updates
d$date <- as.integer(d$date)
d$year <- as.integer(format(as.Date("1970-01-01") + d$date, "%Y"))
d$mon<- as.integer(format(as.Date("1970-01-01") + d$date, "%m"))
bad <- is.na(d$sst_med) #& is.na(d$chla_johnson) & is.na(d$kd490) & is.na(d$par)
d <- d[!bad, ]

saveRDS(d, "/home/acebulk/data/sst_min-max-med_25k.rds")

## we need to join these ponies
sstd <- readRDS("/home/acebulk/data/sst_min-max-med_25k.rds")
chlprev <- readRDS("/home/acebulk/data/chl_sst_25k_tab.rds")

library(dplyr)
dd <- chlprev %>% mutate(date_int = as.integer(as.Date(date))) %>% 
  inner_join(sstd, c("date_int" = "date", "cell" = "cell"))
dd$date_int <- NULL
dd <- dd %>% dplyr::filter(!is.na(chla_johnson))

saveRDS(dd, "/home/acebulk/data/chl_sst-min-med-max_chl_25k.rds")

# 
# 
# library(dplyr)
# library(raster)
# sst_from_db <- function(db, amonth) {
#  # amonth <- as.Date(format(amonth, "%Y-%m-15"))
#  tab <-  tbl(db, "chl_sst_25k_monthly") %>% 
#     dplyr::select(cell, sst, date) %>% 
#     dplyr::filter(date == amonth) %>% dplyr::collect(Inf)
#   
#   g <- setValues(default_grid(), NA)
#   g[tab$cell] <- tab$sst
#   g
#   
# }
# 
# 
# 
# 
# chl_from_db <- function(db, amonth) {
#  # amonth <- as.Date(format(amonth, "%Y-%m-15"))
#   tab <-  tbl(db, "chl_sst_25k_monthly") %>% 
#     dplyr::select(cell, chla, date) %>% 
#     dplyr::filter(date == amonth) %>% dplyr::collect(Inf)
#   
#   g <- setValues(default_grid(), NA)
#   g[tab$cell] <- tab$chla
#   g
#   
# }
# 
# 
# chl <- chl_from_db(db, as.integer(as.Date("2002-12-15")))
# sst <- sst_from_db(db,  as.integer(as.Date("2002-12-15")))
# 
# 
# 
# 
# 
# 
# 
