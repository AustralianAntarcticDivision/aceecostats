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

##db file
library(dplyr)
db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
ice_density_tab <- tbl(db, "ice_density_tab") %>% collect(n = Inf) %>% mutate(date = date + epoch)  %>% 
  filter(dur < 365) %>% 
  filter(!Zone == "Mid-Latitude") 

ice_sparkline_tab <- tbl(db, "ice_sparkline_tab") %>% collect(n = Inf) %>% 
  mutate(date = date + epoch) %>% 
  ## remove low-lat
  filter(!Zone == "Mid-Latitude") %>% 
  #mutate(Zone = "High-Latitude")

  ## combine zones
  group_by(Zone, SectorName, date, decade) %>% summarize(duration = mean(dur))
  #summarize(min = mean(min), max = mean(max))

ice_sparkline_tab_nozone <- tbl(db, "ice_sparkline_tab_nozone") %>% collect(n = Inf) %>% 
  mutate(date = date + epoch) %>% 
  ## remove low-lat
  #filter(!Zone == "Mid-Latitude") %>% 
  #mutate(Zone = "High-Latitude")
  
  ## combine zones
  group_by(SectorName, date, decade) %>% summarize(duration = mean(dur))
#summarize(min = mean(min), max = mean(max))



#library(tidyr)

## loop the plots
pdf("inst/workflow/graphics/iceduration_density_sparklines001-draft.pdf")
uzones <- unique(ice_density_tab$Zone)
#useasons <- c("Summer", "Winter")
for (izone in seq_along(uzones)) {
#  for (iseason in seq_along(useasons)) {
    
    # ## reshape the sparkline data to key/col on min/max
     spark_data <- ice_sparkline_tab %>%  filter(Zone == uzones[izone])
    # 
    # ## subset the density data
     density_data <- ice_density_tab %>% filter(Zone == uzones[izone]) %>% rename(duration = dur) 
    # 
    ## combine high lat and continent
    gspark <-  ggplot(spark_data, aes(x = date, y = duration)) + geom_line() + facet_wrap(~SectorName)
     
    gdens <- ggplot(density_data, aes(x = duration, weights = area,  group = decade, colour = decade)) + 
      geom_density() + facet_wrap(~SectorName)  
 
    print(gspark + ggtitle(uzones[izone]))
    print(gdens + ggtitle(uzones[izone]))

  }

dev.off()




pdf("inst/workflow/graphics/iceduration_density_sparklines_nozone001-draft.pdf")
 spark_data <- ice_sparkline_tab_nozone
 density_data <-  ice_density_tab %>%  rename(duration = dur) 
  gspark <-  ggplot(spark_data, aes(x = date, y = duration)) + geom_line() + facet_wrap(~SectorName)
  gdens <- ggplot(density_data, aes(x = duration, weights = area,  group = decade, colour = decade)) + 
    geom_density() + facet_wrap(~SectorName)  
  
  print(gspark + ggtitle("Combined zones"))
  print(gdens + ggtitle("Combined zones"))
  
dev.off()