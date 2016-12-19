


ice_duration_layout_m <- function() {
  tx <- textConnection(
    "1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4")
  as.matrix(read.csv(tx, header=F))
}



## preparation
#library(aceecostats)
#library(raster)
library(dplyr)
library(raster)
library(feather)
## local path to required cache files

outf <- "/mnt/acebulk"



## date range for the sparkline
sparkline_range <- ISOdatetime(c(1980, 2016), c(1, 11), 1, 0, 0, 0, tz = "GMT")

outpdf <- "inst/workflow/graphics/ice_assess_duration06.pdf"
ras <- raster(file.path(outf,"seaice_duration_raster.grd"))
cell_tab <- read_feather(file.path(outf,"seaice_duration_cell_tab.feather")) 
raw_tab <- read_feather(file.path(outf, "seaice_duration_raw_tab.feather"))
summ_tab <- read_feather(file.path(outf, "seaice_duration_summ_tab.feather"))
##%>%   rename(min = min_ice, max = max_ice, mean = mean_ice)
varlabel <- function(ttext) {
  bquote(.(ttext)~ "ICE SEASON (days)") 
}
min_max <- c(0, 365)





total_areas <- aes_region@data %>% group_by(SectorName, Zone) %>% summarize(area_km2 = sum(area_km2))
total_areas$area_factor <- total_areas$area_km2 / c(Atlantic = 1.3, EastPacific = 1.4, Indian = 1.6, WestPacific = 1.15)[total_areas$SectorName]

## plot specifics
lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = nlevels(raw_tab$decade) + 2))[-c(1, 2)]
den.range <- c(0, 1.5)
dplot <- TRUE
if (dplot) pdf(outpdf)

seas <- ""
zone <- "Polar"
shelf <- "Ocean"

for (shelf in c("Ocean", "Shelf")) {
  #for (seas in "Spring") {
  #  for (zone in "Polar") {
  layout(ice_duration_layout_m())
  op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.95, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
  ## DENSITY PLOTS
  for (sector in c("Atlantic",  "EastPacific", "Indian", "WestPacific")) {
    this_area <- subset(total_areas, Zone == zone & SectorName == sector)
    den.range <- c(0, this_area$area_km2/this_area$area_factor)
    titletext<- paste("Polar", shelf)
    asub <- subset(raw_tab, SectorName == sector & Shelf == shelf & Zone == zone)
    if (nrow(asub) < 10) {
      dummyplot()
      #  dummyplot()
      next;
    }
    with(asub, {
      plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
      if (sector %in% c("Atlantic", "EastPacific")) mtext("mean", side = 2)
      text(30, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
      for (k in seq_along(lcols)) {
        vals <- dur[decade == decselect(k)]
        
        if (length(vals) < 1 | all(is.na(vals))) next;
        
        dens.df <- do_density(vals)
        lines(dens.df, col=lcols[k], lwd=lwdths[k])
        print(k)
      }
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
           col = paste0(sector_colour(sector),40))
      box()
      mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
      axis(1)
    }
    )
    
  }
  
  ## SPARKLINES
  for (sector in c("Atlantic",  "EastPacific", "Indian", "WestPacific")) {
    
    asub <- subset(summ_tab, SectorName == sector & Shelf == shelf & Zone == zone)
    print(nrow(asub))
    if (nrow(asub) < 10) {
      dummyplot()
      # dummyplot()
      next; 
    } 
    
    with(asub, {
      plot(sparkline_range, range(mean), type = "n", axes = FALSE, xlab = "", ylab = "")
      segmentlines(cbind(date, mean), col = lcols[decade])
      abline(h = mean(mean))
      textheadtail(date, round(mean))
      
      
      
    }
    )
    
  }
  par(op)
  
}

if (dplot) dev.off()

