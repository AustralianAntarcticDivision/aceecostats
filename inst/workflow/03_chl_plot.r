## use daily bin summaries DB
library(dplyr)
db <- src_sqlite("/mnt/acebulk/chlorophyll_assessment.sqlite3")
dd <- bind_rows(tbl(db, "seawifs") %>% distinct(date) %>% collect(n = Inf), 
                tbl(db, "modisa") %>% distinct(date) %>% collect(n = Inf)) %>% distinct(date) %>% arrange(date)



dd$season <- aceecostats::aes_season(as.POSIXct(dd$date + as.Date("1970-01-01"), tz = "GMT"))
dd$decade <- aceecostats:::decade_maker(as.POSIXct(dd$date + as.Date("1970-01-01"), tz = "GMT"))
dd <- copy_to(db, dd)

seas <- "Summer"
dec <- "1999-2008"
tempd <- dd %>% filter(season == seas, decade == dec) %>% 
  inner_join(tbl(db, "modisa")) %>% select(chla_nasa) %>% collect(n = Inf)




##################### original

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

## load everything from the two tables (parameterized filtering not working, otherwise we
## could filter in the DB while plotting)
chl_density_tab <- tbl(db, "chl_density_tab")  %>% 
  collect(n = Inf)  %>% mutate(date = date + epoch)  %>%    rename(chl = val)
chl_sparkline_tab <- tbl(db, "chl_sparkline_tab")  %>% 
  collect(n = Inf) %>% 
  mutate(season_year = season_year + epoch) %>% 
  rename(chl = mean)


## loop the plots
pdf("inst/workflow/graphics/chl_density_sparklines001.pdf")
uzones <- unique(chl_density_tab$Zone)
useasons <- c("Summer", "Winter")
izone <- iseason <- 1
for (izone in seq_along(uzones)) {
  for (iseason in seq_along(useasons)) {
    
    ## subset the spark data
    spark_data <- chl_sparkline_tab %>% filter(Zone == uzones[izone], season == useasons[iseason])  
    
    ## subset the density data
    density_data <- chl_density_tab %>% filter(Zone == uzones[izone], season == useasons[iseason]) 
    
  ## create the three gg objects for sparkline, min-sst, max-sst
   gspark <-  ggplot(spark_data, aes(x = season_year, y = chl)) + geom_line() + facet_wrap(~SectorName+ Zone)
   gdens <- ggplot(density_data, aes(x = chl, weights = area,  group = decade, colour = decade)) + 
      geom_density() + facet_wrap(~SectorName+ Zone) 

    ## linear for the spark line
    print(gspark + ggtitle(sprintf("%s", useasons[iseason])) ) #+ scale_y_log10())
    ## log for the density (alternatively set the xlim to much tighter)
    print(gdens + ggtitle(sprintf("%s", useasons[iseason])) + scale_x_log10() + ylab("km2"))  ##  + xlim(c(0, 1)))
    
  
  }
}
dev.off()
