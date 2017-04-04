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
pdf("inst/workflow/graphics/iceduration_density_sparklines002.pdf")
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




pdf("inst/workflow/graphics/iceduration_density_sparklines_nozone002.pdf")
 spark_data <- ice_sparkline_tab_nozone
 density_data <-  ice_density_tab %>%  rename(duration = dur) 
  gspark <-  ggplot(spark_data, aes(x = date, y = duration)) + geom_line() + facet_wrap(~SectorName)
  gdens <- ggplot(density_data, aes(x = duration, weights = area,  group = decade, colour = decade)) + 
    geom_density() + facet_wrap(~SectorName)  
  
  print(gspark + ggtitle("Combined zones"))
  print(gdens + ggtitle("Combined zones"))
  
dev.off()

library(raster)

adv <- brick(file.path(dp, "south_advance.grd"))
ret <- brick(file.path(dp, "south_retreat.grd"))
duration <- ret - adv
## if retreat is equal to or less than one, it didn't retreat
duration[ret <= 1] <- 365
obj <- setZ(duration, ISOdatetime(seq(1979, length = nlayers(adv)), 2, 15, 0, 0, 0, tz = "GMT"))
rm(ret, adv)

decade <- decade_maker(getZ(obj))
decade_maps <- vector("list", nlevels(decade))
ll_maps <- vector("list", nlevels(decade))

for (i in seq_along(decade_maps)) {
  asub <- which(decade == levels(decade)[i])
  dur <- calc(subset(duration, asub), mean, na.rm = TRUE)
  ll_maps[[i]] <- dur
  
  decade_maps[[i]] <- tabit(dur)
  
}


decade_maps <- bind_rows(decade_maps, .id = "decade") %>% 
  rename(duration = val)
decade_maps$decade <- levels(decade)[as.integer(decade_maps$decade)]
library(ggplot2)
decade_maps <- local({
  xy <- xyFromCell(duration, decade_maps$cell_)
  #xy <- rgdal::project(xy, projection(duration), inv = TRUE)
  decade_maps$x <- xy[, 1]
  decade_maps$y <- xy[, 2]
  decade_maps
})
library(ggplot2)
map <- fortify(subset(aes_zone, !Zone == "Mid-Latitude"))

ggmaps <- ggplot(decade_maps, aes(x = x, y = y, fill = duration)) + 
  geom_raster() + facet_wrap(~decade) + 
  coord_equal() + 
  scale_fill_gradientn(colours = palr::sstPal(200)[1:120]) + 
  geom_path(data = map, aes(x = long, y = lat, group = group, fill = NULL)) + 
  scale_x_continuous(labels = NULL) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pdf("inst/workflow/graphics/iceduration_maps.pdf")
print(ggmaps)
dev.off()


## now convert to different maps

llmap <- raster(extent(-180, 180, -85, -50), res = 0.1, crs = "+init=epsg:4326")
#decade <- decade_maker(getZ(obj))


ll_maps <- projectRaster(stack(ll_maps), llmap)

pdf("inst/workflow/graphics/iceduration_maps_longlat.pdf")
par(mar = rep(0, 4))

plot(setNames(ll_maps, levels(decade)),
     asp = NA, 
     col = palr::sstPal(200)[1:120], nr = 4, 
     addfun = function() plot(aes_zone_ll, add = TRUE), 
     axes = FALSE)

dev.off()
