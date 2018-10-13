library(raster)
library(aceecostats)
library(dplyr)
library(raadtools)
## CHLA-RJ and OISST (daily)
#chlfiles <- oc_sochla_files(product = "MODISA")  %>% 
#dplyr::filter(format(date, "%m") %in% c("12", "01", "02")) %>%  
#  mutate(month = as.Date(format(date, "%Y-%m-15")))

sstfiles <- raadfiles::oisst_daily_files() %>% 
  #  dplyr::filter(between(date, min(chlfiles$date), max(chlfiles$date))) %>%  #, format(date, "%m") %in% c("12", "01", "02"))  %>% 
  mutate(month = as.Date(format(date, "%Y-%m-15")))


## KD490 and PAR (monthly)
#kdfiles <- raadtools::ocfiles(time.resolution = "monthly", product = "MODISA", varname = "KD490", type = "L3m")
#parfiles <- raadtools::ocfiles(time.resolution = "monthly", product = "MODISA", varname = "PAR", type = "L3m")


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


umonth <- unique(sstfiles$month)

sstfilelist <- split(sstfiles, sstfiles$month)[as.character(umonth)]

lfun <- function(filenames) {
  fnames <- filenames$fullname
  sst_med <- read_summarize_sst_med(fnames)
  sst_min <- read_summarize_sst_min(fnames)
  sst_max <- read_summarize_sst_max(fnames)
 # if (runif(1) > 0.9) print(sample(letters, 1))
  x <- tibble(sst_med = raster::values(sst_med),
         sst_min = raster::values(sst_min), 
         sst_max = raster::values(sst_max),
         cell = seq_len(ncell(sst_med)))  
  fpath <- file.path("/home/mdsumner/Git/aceecostats/outdir", gsub("nc$", "rds", basename(fnames[1])))

  saveRDS(x, fpath)
  fpath
}

library(future.apply)
plan(multiprocess)

l <- future_lapply(sstfilelist, lfun)


fs <- list.files("outdir", pattern = "rds$", full.names = TRUE)
dt <- as.Date(strptime(basename(fs), "avhrr-only-v2.%Y%m%d"))
d <- vector("list", length(fs))
library(dplyr)
for (i in seq_along(fs)) {
  d[[i]] <- readRDS(fs[i]) %>% mutate(date = dt[i])
  print(i)
}
d <- dplyr::bind_rows(d)
bad <- is.na(d$sst_med) #& is.na(d$chla_johnson) & is.na(d$kd490) & is.na(d$par)
d <- d[!bad, ]

saveRDS(d, "/home/acebulk/data/sst_ALL_min-max-med_25k.rds", compress = FALSE)


## START HERE
library(dplyr)
library(aceecostats)
d  <- readRDS("/home/acebulk/data/sst_ALL_min-max-med_25k.rds") %>% 
  mutate(decade = decade_maker(date), 
         season = aes_season(date), 
         year = as.integer(format(date, "%Y")), 
         mon = as.integer(format(date, "%m"))) %>% 
  inner_join(region, c("cell" = "cellindex")) %>% 
  dplyr::filter(!is.na(Zone), !is.na(SectorName))


saveRDS(d, "/home/acebulk/data/sst_ALL_min-max-med_region_25k.rds", compress = FALSE)
