
## preparation
library(aceecostats)
library(raster)
library(feather)
library(dplyr)

## local path to required cache files
datapath <- "/mnt/acebulk"

## date range for the sparkline
sparkline_domain <- mk_sparkline_domain()


outpdf <- "inst/workflow/graphics/chl_area_distribution_001.pdf"
ras <- raster(file.path(datapath,"chl_raster.grd"))

 summ_tab <- read_feather(file.path(datapath, "chl_summ_tab.feather")) 
  raw_tab <-  read_feather(file.path(datapath, "chl_raw_tab.feather")) 


varlabel <- function(ttext) {
  bquote(.(ttext)~ "CHL-a mg m-3") 
}
seclab <- 2.1
min_max <- c(0.01, 3)
usr <- c(0.000000001, 100, -5, 6e9)
dolog <- "x"
scaleval <- if (dolog == "x")  function(x) log(x) else function(x) x
unscaleval <- if(dolog == "x") function(x) exp(x) else function(x) x



total_areas <- aes_region@data %>% 
  group_by(SectorName, Zone) %>% 
  summarize(area_km2 = sum(area_km2))

total_areas$area_max <- total_areas$area_km2
## Atlantic/Polar
total_areas$area_max[1] <- 1e8
## Atlantic/Temperate
total_areas$area_max[2] <- 8e7
## EastPacific/Polar
total_areas$area_max[3] <-  7e6
## EastPacific/Temperate
total_areas$area_max[4] <-  5e7
## Indian/Polar
total_areas$area_max[5] <- 6e7
## Indian/Temperate
total_areas$area_max[6] <- 1e8
## WestPacific/Polar
total_areas$area_max[7] <- 2.5e7
## WestPacific/Temperate
total_areas$area_max[8] <- 1.2e8


lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = length(unique(alldecades)) + 2))[-c(1, 2)]
dplot <- TRUE
seas <- "Summer"; zone <- "Polar"
op1 <- options(scipen = -1)
if (dplot) pdf(outpdf)
for (seas in c( "Summer", "Winter")) {
  for (zone in c("Polar",  "Temperate")) {
    
    layout(layout_m())
    op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.95, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
    ## DENSITY PLOTS
    for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
      this_area <- dplyr::filter(total_areas, Zone == zone & SectorName == sector)
      
      den.range <- c(0, this_area$area_max)
      titletext<- paste(seas, zone)
      asub <- dplyr::filter(raw_tab, SectorName == sector & Zone == zone & Season == seas)## %>% collect(n = Inf)
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next;
      }
      
      plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "", log = dolog)
      
      polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
      if (sector %in% c("Atlantic", "EastPacific")) mtext("min (km^2)", side = 2)
      for (k in seq_along(lcols)) {
        vals_wgt <- asub %>% filter(decade == decselect(k)) %>% dplyr::select(min, area)
        if (nrow(vals_wgt) < 1 | all(is.na(vals_wgt$min))) next;
        dens.df <- do_hist(scaleval(vals_wgt$min), w = vals_wgt$area)

        lines(unscaleval(dens.df$x), dens.df$y, col=lcols[k], lwd=lwdths[k])
        
      }
      axis(2, cex.axis = 0.5, las = 1, mgp = c(3, -1.95, 0))
      box()
      mtext(side=1, varlabel(titletext) ,outer =TRUE, line=1.5, cex=1)
      
     plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "", log = dolog)
      polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
      
      if (grepl("Pacific", sector)) axis(1)
      if (sector %in% c("Atlantic", "EastPacific")) mtext("max (km^2)", side = 2)
      text(seclab[1], den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
      
      for (k in seq_along(lcols)) {
        vals_wgt <- asub %>% filter(decade == decselect(k)) %>% dplyr::select(max, area)
        if (nrow(vals_wgt) < 1 | all(is.na(vals_wgt$max))) next;
        dens.df <- do_hist(scaleval(vals_wgt$max), w = vals_wgt$area)
        lines(unscaleval(dens.df$x), dens.df$y, col=lcols[k], lwd=lwdths[k])
      }
      axis(2, cex.axis = 0.5, las = 1, mgp = c(3, -1.95, 0))
      box()
      mtext(side=1, varlabel(titletext) ,outer =TRUE, line=1.5, cex=1)
      
      
      
      
    }
    mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
    
    ## SPARKLINES
    for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
      asub <- dplyr::filter(summ_tab, SectorName == sector & Zone == zone & Season == seas) %>% collect()
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next; 
      } 
      
      shouldersub <- dplyr::filter(summ_tab, SectorName == sector & Zone == zone & Season == c(Summer = "Autumn", Winter = "Spring")[seas])
      sparkline_range <- range(c(asub$min, shouldersub$min))
      plot(sparkline_domain, sparkline_range, type = "n", axes = FALSE, xlab = "", ylab = "")
      segmentlines(cbind(asub$date, asub$min), col = lcols[asub$decade])
      abline(h = mean(asub$min))
      textheadtail(asub$date, asub$min)
      
      ## do the shoulder season
      segmentlines(cbind(shouldersub$date, shouldersub$min), col = lcols[shouldersub$decade], lty = 2)
      
      sparkline_range <- range(c(asub$max, shouldersub$max))
      plot(sparkline_domain, sparkline_range, type = "n", axes = FALSE, xlab = "", ylab = "")
      segmentlines(cbind(asub$date, asub$max), col = lcols[asub$decade])
      abline(h = mean(asub$max))
      textheadtail(asub$date, asub$max)
      
      
      ## do the shoulder season
      segmentlines(cbind(shouldersub$date, shouldersub$max), col = lcols[shouldersub$decade], lty = 2)
      
      
    }
    par(op)
    
  }}

options(op1)
if (dplot) dev.off()


