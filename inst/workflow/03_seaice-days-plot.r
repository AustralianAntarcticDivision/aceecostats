decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1977, 1987, 1997, 2007, 2017), 
      lab = c("1977-1987", "1987-1998","1998-2007", "2007-2017"))
}
## preparation
library(aceecostats)
library(raster)
library(feather)
library(dplyr)
library(ggplot2)
## local path to required cache files
datapath <- "/mnt/acebulk"



library(ggplot2)
library(tidyr)
dp <- "/mnt/acebulk/seaiceseason"

##db file
library(dplyr)
db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
ice_density_tab <- tbl(db, "ice_days_density_tab") %>% collect(n = Inf) %>% mutate(date = date + epoch)  %>% 

  filter(!Zone == "Mid-Latitude") 

ice_sparkline_tab <- tbl(db, "ice_days_sparkline_tab") %>% collect(n = Inf) %>% 
  mutate(date = date + epoch) 

ice_sparkline_tab_nozone <- tbl(db, "ice_days_sparkline_tab_nozone") %>% collect(n = Inf) %>% 
  mutate(date = date + epoch)



pdf("inst/workflow/graphics/icedays_sparklines_nozone001.pdf")
 spark_data <- ice_sparkline_tab_nozone
 density_data <-  ice_density_tab %>% filter(days > 0, days < 365)
  gspark <-  ggplot(spark_data, aes(x = date, y = days)) + geom_line() + facet_wrap(~SectorName)
  gdens <- ggplot(density_data, aes(x = days, weights = area,  group = decade, colour = decade)) + 
    geom_density() + facet_wrap(~SectorName)  
  
  print(gspark + ggtitle("Combined zones"))
  print(gdens + ggtitle("Combined zones"))
  
dev.off()
