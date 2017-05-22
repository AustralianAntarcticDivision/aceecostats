
## now relying on scripts here MDS 2017-05-21
##"data_local/acecrc.org.au/ocean_colour/"





# library(dplyr)
# db <- src_sqlite("/mnt/acebulk/chlorophyll_assessment.sqlite3", create = TRUE)
# 
# library(raadtools)
# library(roc)
# files <- chla_johnsonfiles(product = "MODISA")
#  label <- "modisa"
# # ## initialize the bin logic for MODISA
# init <- initbin(NUMROWS = 4320)
# # ## counts up from the south
# 
# files$season <- aceecostats::aes_season(files$date)
# segs <- unclass(factor(cumsum(c(0, abs(diff(unclass(factor(files$season))))))))
# usegs <- unique(segs)
# # 
#  for (i in seq_along(usegs)) {
#    
#    ## this is so much faster mdsumner 2017-05-22
#    asub <- segs == usegs[i]
#    d <- bind_rows(lapply(files$fullname[asub], readRDS))
# 
# #   if (i == 1) {
# #     copy_to(db, d, label, indexes = list("bin_num", "date"), temporary = FALSE)
# #   } else {
# #     db_insert_into( con = db$con, table = label, values = d)
# #   }
# }
# 
# 
# 
# if (FALSE) {
#   # files <- seawifsfiles
#   # label <- "seawifs"
#   # init <- initbin(NUMROWS = 2160)
#   # 
#   files <- modisfiles
#   label <- "modisa"
#   ## initialize the bin logic for MODISA
#   init <- initbin(NUMROWS = 4320)
#   ## counts up from the south
#   maxbin <- init$totbin/2
#   files$season <- aceecostats::aes_season(files$date)
#   segs <- unclass(factor(cumsum(c(0, abs(diff(unclass(factor(files$season))))))))
#   usegs <- unique(segs)
#   
#   for (i in seq_along(usegs)) {
#     asub <- segs == usegs[i]
#     d <- read_l3_fileset(files$fullname[asub][1:2])
#     d <- d %>% group_by(bin_num) %>% mutate(n = n()) %>% 
#       summarize(chla_nasa = mean(chla_nasa), chla_johnson = mean(chla_johnson),
#                 n = sum(n))
#     d$date <- as.Date(files$date[asub][1])
#     print(d$date[1])
#     
#     if (i == 1) {
#       copy_to(db, d, label, indexes = list("bin_num", "date"), temporary = FALSE)
#     } else {
#       db_insert_into( con = db$con, table = label, values = d)
#     }
#   }
#   
#   
# }