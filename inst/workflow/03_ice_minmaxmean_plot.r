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
epoch <- as.Date(ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT"))
ice_density_tab <- tbl(db, "ice_minmaxmean_density_tab") %>% collect(n = Inf)
ice_sparkline_tab <- tbl(db, "ice_minmaxmean_sparkline_tab") %>% collect(n = Inf) %>% 
  mutate(season_year = season_year + epoch, season = aes_season(season_year))


library(tidyr)

## loop the plots
pdf("inst/workflow/graphics/ice_minmaxmean_density_sparklines000.pdf")
#uzones <- unique(ice_density_tab$Zone)
useasons <- unique(ice_density_tab$season)
izone <- 2
iseason <- 1
#for (izone in seq_along(uzones)) {
  for (iseason in seq_along(useasons)) {
    
    ## reshape the sparkline data to key/col on min/max
    spark_data <- ice_sparkline_tab %>% filter(Zone == uzones[izone], season == useasons[iseason]) %>% 
      gather(measure, ice, -season, -SectorName, -Zone, -season_year)
    
    ## subset the density data
    density_data <- ice_density_tab %>% filter(Zone == uzones[izone], season == useasons[iseason]) 
    
  ## create the three gg objects for sparkline, min-sst, max-sst
   gspark <-  ggplot(spark_data, aes(x = season_year, y = ice, group = measure, colour = measure)) + 
     geom_line() + facet_wrap(~SectorName)
   gdens <- ggplot(density_data %>% filter(meanval > 15), aes(x = meanval, weights = area,  group = decade, colour = decade)) + 
      geom_density() + facet_wrap(~SectorName)  
  
    print(gspark)
    
    print(gdens + ggtitle(sprintf("%s", useasons[iseason])))
    #print(gdens_max + ggtitle(sprintf("%s", useasons[iseason])))
    
  }
#}

dev.off()


