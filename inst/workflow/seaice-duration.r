ice_duration_layout_m <- function() {
  tx <- textConnection(
    "1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4")
  as.matrix(read.csv(tx, header=F))
}



## preparation
library(aceecostats)
library(raster)
library(dplyr)
library(raster)
library(feather)
## local path to required cache files

outf <- "/mnt/acebulk"

## domain and range for the sparkline (or use the range of the data)
sparkline_domain <- ISOdatetime(c(1980, 2016), c(1, 11), 1, 0, 0, 0, tz = "GMT")
#sparkline_range <- c(170, 350)

outpdf <- "inst/workflow/graphics/ice_area_distribution002.pdf"
ras <- raster(file.path(outf,"seaice_duration_raster.grd"))

## tables of cell data and summaries
raw_tab <- read_feather(file.path(outf, "seaice_duration_raw_tab.feather"))
summ_tab <- read_feather(file.path(outf, "seaice_duration_summ_tab.feather"))

#' Generate variable label
#'
#' @param ttext 
#'
#' @return
#' @export
#'
#' @examples
varlabel <- function(ttext) {
  bquote(.(ttext)~ "ICE SEASON (days)") 
}

## range of the data for density plots
min_max <- c(0, 365)


#' Worker function for histogram/density
#'
#' @param v measured values (e.g. ice season duration)
#' @param w weight values (e.g. area of this cell)
#'
#' @return
#' @export
#'
do_hist <- function(v, w = NULL) {
  the.his <- hist(v, breaks=50, plot = FALSE)
  multiplier <- (the.his$counts / the.his$density)[1]
  the.den <- density(v, from=min(v), to=max(v), weights = w)
  data.frame(x=the.den$x, y=the.den$y*multiplier)
}


## aggregate the aes regions down for ice, just Sector and Shelf here
total_areas <- aes_region@data %>% group_by(SectorName, Shelf) %>% summarize(area_km2 = sum(area_km2))
#total_areas$area_factor <- 1 / 500 #*c(Atlantic = 1.3, EastPacific = 1.4, Indian = 1.6, WestPacific = 1.15)[total_areas$SectorName]

## pre-determine the actual maximum for each related sub-plot
total_areas$den_MAX <- numeric(nrow(total_areas))
for (i in seq(nrow(total_areas))) {
  asub <- raw_tab %>% filter(SectorName == total_areas$SectorName[i], Shelf == total_areas$Shelf[i])
  total_areas$den_MAX[i] <- max(do_hist(asub$dur, asub$area)$y)
}
total_areas <- total_areas %>% group_by(SectorName) %>% mutate(den_MAX = max(den_MAX))


## plot specifics
lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = nlevels(raw_tab$decade) + 2))[-c(1, 2)]
dplot <- TRUE
if (dplot) pdf(outpdf)

seas <- ""
zone <- "Polar"
shelf <- "Ocean"
sector <- "Atlantic"
for (shelf in c("Ocean", "Shelf")) {
  layout(ice_duration_layout_m())
  op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.95, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
  ## DENSITY PLOTS
  
  for (sector in c("Atlantic",  "EastPacific", "Indian", "WestPacific")) {
    this_area <- subset(total_areas, Shelf == shelf & SectorName == sector)
    den.range <- c(0, this_area$den_MAX/10) 
    titletext<- paste("Polar", shelf)
    asub <- subset(raw_tab, SectorName == sector & Shelf == shelf & Zone == zone)
    
    plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
    if (sector %in% c("Atlantic", "EastPacific")) mtext("km^2", side = 2)
    text(30, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
    for (k in seq_along(lcols)) {
      vals_wgt <- asub %>% filter(decade == decselect(k)) %>% dplyr::select(dur, area)
      if (nrow(vals_wgt) < 1 | all(is.na(vals_wgt$dur))) next;
      dens.df <- do_hist(vals_wgt$dur, w = vals_wgt$area)
      lines(dens.df, col=lcols[k], lwd=lwdths[k])
      
    }
    
    axis(2, cex.axis = 0.5, las = 1, mgp = c(3, -1.95, 0))
    
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
         col = paste0(sector_colour(sector),40))
    box()
    mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
    if (sector %in% c("EastPacific", "WestPacific")) axis(1); 
    # })
    
  }
  
  ## SPARKLINES
  for (sector in c("Atlantic",  "EastPacific", "Indian", "WestPacific")) {
    
    asub <- dplyr::filter(summ_tab, SectorName == sector & Shelf == shelf & Zone == zone) %>% select(date, mean, decade)
    
    plot(sparkline_domain, range(asub$mean) + c(-10, 10), type = "n", axes = FALSE, xlab = "", ylab = "")
    segmentlines(cbind(asub$date, asub$mean), col = lcols[asub$decade])
    abline(h = mean(asub$mean))
    textheadtail(asub$date, round(asub$mean))
  }
  
  par(op)
  
}

if (dplot) dev.off()

