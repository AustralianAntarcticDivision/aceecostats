

library(raadtools)
library(roc)
library(tibble)
library(dplyr)
## data frame of all L3 RRS files for MODISA
modisfiles <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname, lab = "modisa")
seawifsfiles <- ocfiles(time.resolution = "daily", product = "SeaWiFS", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname, lab = "seawifs")
viirsfiles <- ocfiles(time.resolution = "daily", product = "VIIRS", varname = "SNPP_RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname, lab = "viirs")

allfiles <- bind_rows(modisfiles, seawifsfiles, viirsfiles)



## hacked function to read just the RRS we need
read_L3_file <- function(file, compound_vars = NULL) {
  info <- rhdf5::h5ls(file)
  tab <- table(info$dim); wm <- which.max(tab); test <- names(tab[wm])
  ## get all vars, or just the ones the users wants
  if (is.null(compound_vars))  {
    compound <- info$name[info$dim == test]
  } else {
    compound <- compound_vars
  }
  compoundpath <- file.path("/level-3_binned_data", compound)
  d <- tibble::as_tibble(rhdf5::h5read(file, name = file.path("/level-3_binned_data", "BinList")))

  l <- lapply(compoundpath, function(aname) tibble::as_tibble(rhdf5::h5read(file, name = aname))[, "sum"])
  ## not very generalized but ok for L3 RRS
  
  d2 <- dplyr::bind_cols(lapply(seq_along(compound), function(i) setNames(l[[i]], sprintf("%s_%s", compound[i], names(l[[i]])))))
  dplyr::bind_cols(d, d2)
}

sub_l3_seawifs <- function(file) {
  read_L3_file(file, compound_vars = c("Rrs_443", "Rrs_490", "Rrs_555", "Rrs_510")) %>%
    filter(bin_num <= maxbin) %>% 
    mutate(chla_johnson = chla(., sensor = "SeaWiFS", algo = "johnson")) %>% 
    mutate(chla_nasa = chla(., sensor = "SeaWiFS", algo = "oceancolor")) %>% 
    dplyr::select(bin_num, chla_johnson, chla_nasa) %>% filter(chla_johnson > 0)
}

sub_l3_modis <- function(file) {
  read_L3_file(file, compound_vars = c("Rrs_443", "Rrs_488", "Rrs_555", "Rrs_547")) %>% 
    filter(bin_num <= maxbin) %>% 
    mutate(chla_johnson = chla(., sensor = "MODISA", algo = "johnson")) %>% 
    mutate(chla_nasa = chla(., sensor = "MODISA", algo = "oceancolor")) %>% 
    dplyr::select(bin_num, chla_johnson, chla_nasa) %>% filter(chla_johnson > 0)
}

read_l3_fileset <- function(files) {
  bind_rows(lapply(files, sub_l3_modis))
}

## experimenting with different speedups
# db <- src_sqlite("/mnt/acebulk/chlorophyll_assessment.sqlite3")
# files <- modisfiles
# label <- "modisa"
# ## initialize the bin logic for MODISA
# init <- initbin(NUMROWS = 4320)
# ## counts up from the south
# maxbin <- init$totbin/2
# files$season <- aceecostats::aes_season(files$date)
# segs <- unclass(factor(cumsum(c(0, abs(diff(unclass(factor(files$season))))))))
# usegs <- unique(segs)
# 
# for (i in seq_along(usegs)) {
#   asub <- segs == usegs[i]
#   system.time({
#   d <- read_l3_fileset(files$fullname[asub])
#   })
#   system.time({
#     d2 <- d90 %>% partition(bin_num) %>% mutate(n = n()) %>% 
#       summarize(chla_nasa = mean(chla_nasa), chla_johnson = mean(chla_johnson),
#                 n = sum(n)) %>% collect()
#   })
#   system.time({
#    d3 <- d90 %>% group_by(bin_num) %>% mutate(n = n()) %>% 
#     summarize(chla_nasa = mean(chla_nasa), chla_johnson = mean(chla_johnson),
#               n = sum(n))
#   })
#   
#   d$date <- as.Date(files$date[asub][1])
#   print(d$date[1])
#   
#   if (i == 1) {
#     copy_to(db, d, label, indexes = list("bin_num", "date"), temporary = FALSE)
#   } else {
#     db_insert_into( con = db$con, table = label, values = d)
#   }
# }



if (FALSE) {
  # files <- seawifsfiles
  # label <- "seawifs"
  # init <- initbin(NUMROWS = 2160)
  # 
  files <- modisfiles
  label <- "modisa"
  ## initialize the bin logic for MODISA
  init <- initbin(NUMROWS = 4320)
  ## counts up from the south
  maxbin <- init$totbin/2
  files$season <- aceecostats::aes_season(files$date)
  segs <- unclass(factor(cumsum(c(0, abs(diff(unclass(factor(files$season))))))))
  usegs <- unique(segs)
  
  for (i in seq_along(usegs)) {
    asub <- segs == usegs[i]
    d <- read_l3_fileset(files$fullname[asub][1:2])
    d <- d %>% group_by(bin_num) %>% mutate(n = n()) %>% 
      summarize(chla_nasa = mean(chla_nasa), chla_johnson = mean(chla_johnson),
                n = sum(n))
    d$date <- as.Date(files$date[asub][1])
    print(d$date[1])
    
    if (i == 1) {
      copy_to(db, d, label, indexes = list("bin_num", "date"), temporary = FALSE)
    } else {
      db_insert_into( con = db$con, table = label, values = d)
    }
  }
  
  
}