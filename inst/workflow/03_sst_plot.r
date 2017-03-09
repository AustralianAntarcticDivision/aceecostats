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
sst_density_tab <- tbl(db, "sst_density_tab") %>% collect(n = Inf)
sst_sparkline_tab <- tbl(db, "sst_sparkline_tab") %>% collect(n = Inf) %>% mutate(season_year = season_year + epoch)


## we can chain this all into ggplot interactively
## gather is a reshape so "measure" is key on "min" or "max", and "sst" is the value
## we keep SectorName, Zone, season_year out of the reshape, and we filter/facet on them
spark_data <- sst_sparkline_tab %>% filter(Zone == "High-Latitude", aes_season(season_year) == "Winter") %>% 
  gather(measure, sst, -SectorName, -Zone, -season_year)

density_data <- sst_density_tab %>% filter(Zone == "Mid-Latitude", season == "Summer") 

# ## the plot call is general, after we've reshaped and filterered above
# ggplot(spark_data, aes(x = season_year, y = sst, group = measure, colour = measure)) + geom_line() + facet_wrap(~SectorName+ Zone)
# ggplot(density_data, aes(x = min, weights = area,  group = decade, colour = decade)) + 
#   geom_density() + facet_wrap(~SectorName+ Zone) 




## loop the plots
pdf("inst/workflow/graphics/sst_density_sparklines001.pdf")
uzones <- unique(sst_density_tab$Zone)
useasons <- c("Summer", "Winter")
for (izone in seq_along(uzones)) {
  for (iseason in seq_along(useasons)) {
    
    ## reshape the sparkline data to key/col on min/max
    spark_data <- sst_sparkline_tab %>% filter(Zone == uzones[izone], aes_season(season_year) == useasons[iseason]) %>% 
      gather(measure, sst, -SectorName, -Zone, -season_year)
    
    ## subset the density data
    density_data <- sst_density_tab %>% filter(Zone == uzones[izone], season == useasons[iseason]) 
    
  ## create the three gg objects for sparkline, min-sst, max-sst
   gspark <-  ggplot(spark_data, aes(x = season_year, y = sst, group = measure, colour = measure)) + geom_line() + facet_wrap(~SectorName+ Zone)
   gdens_min <- ggplot(density_data, aes(x = min, weights = area,  group = decade, colour = decade)) + 
      geom_density() + facet_wrap(~SectorName+ Zone) 
   gdens_max <- ggplot(density_data, aes(x = max, weights = area,  group = decade, colour = decade)) + 
     geom_density() + facet_wrap(~SectorName+ Zone) 
   
    print(gspark)
    print(gdens_min + ggtitle(sprintf("%s", useasons[iseason])))
    print(gdens_max + ggtitle(sprintf("%s", useasons[iseason])))
    
  }
}
dev.off()
