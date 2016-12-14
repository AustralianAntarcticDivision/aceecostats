devtools::load_all()

## preparation
library(aceecostats)
library(raster)
library(feather)
library(dplyr)

## local path to required cache files
datapath <- "/mnt/acebulk"

do_chl <- TRUE
## date range for the sparkline
sparkline_range <- mk_sparkline_range()



if (do_chl) {
  outpdf <- "inst/workflow/graphics/chl_assess_10.pdf"
  dolog = "x"
  ras <- raster(file.path(datapath,"chl_raster.grd"))
  ## not used
  ## cell_tab <- read_feather(file.path(datapath, "chl_cell_tab.feather"))  
  #%>% mutate(min = log(min), max = log(max), mean = log(mean))
  summ_tab <- read_feather(file.path(datapath, "chl_summ_tab.feather")) 
  #%>%  mutate(min = log(min), max = log(max))  ## no mean
  
  raw_tab <-  read_feather(file.path(datapath, "chl_raw_tab.feather")) 
  #%>%   mutate(min = log(min), max = log(max), mean = log(mean))
  seclab <- 0.06
  varlabel <- function(ttext) {
    bquote(.(ttext)~ "CHL-a mg m-3") 
  }
  min_max <- c(0.01, 3)
  usr <- c(0.000000001, 100, -5, 100)
}


total_areas <- aes_region@data %>% group_by(SectorName, Zone) %>% summarize(area_km2 = sum(area_km2))
total_areas$area_factor <- total_areas$area_km2 / c(Atlantic = 1.3, EastPacific = 1.4, Indian = 1.6, WestPacific = 1.15)[total_areas$SectorName]

#total_areas$area_factor <- 3000000
## plot specifics
lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = nlevels(raw_tab$decade) + 2))[-c(1, 2)]
den.range <- c(0, 2)
dplot <- TRUE
scaleval <- if (dolog == "x")  function(x) log(x) else function(x) x
unscaleval <- if(dolog == "x") function(x) exp(x) else function(x) x
seas <- "Summer"; zone <- "Polar"
if (dplot) pdf(outpdf)
for (seas in c( "Summer", "Winter")) {
  for (zone in c("Polar",  "Temperate")) {
    
    #for (seas in "Spring") {
    #  for (zone in "Polar") {
    
    layout(layout_m())
    op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.95, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
    ## DENSITY PLOTS
    for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
      this_area <- subset(total_areas, Zone == zone & SectorName == sector)
      den.range <- c(0, this_area$area_km2/this_area$area_factor)
      titletext<- paste(seas, zone)
      asub <- dplyr::filter(raw_tab, SectorName == sector & Zone == zone & Season == seas)
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next;
      }
      with(asub, {
        plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "", log = dolog)
        #usr <- par("usr")
        
        polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
        #polygon(expand.grid(x = min_max, y = den.range)[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
        if (sector %in% c("Atlantic", "EastPacific")) mtext("min", side = 2)
       # text(0, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
        for (k in seq_along(lcols)) {
          vals <- min[decade == decselect(k)]
          wgt <- area[decade == decselect(k)] 
          if (length(vals) < 1 | all(is.na(vals))) next;
          
          dens.df <- do_density(scaleval(vals), w = wgt)
          lines(unscaleval(dens.df$x), dens.df$y, col=lcols[k], lwd=lwdths[k])
          print(k)
        }
        
        #usr <- par("usr")
        # if (dolog == "x") usr[c()] <- log(usr)
        
        #do_rect(usr, paste0(sector_colour(sector),40))
        
        #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
        #     col = paste0(sector_colour(sector),40))
        box()
      
        mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
        plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "max", log = dolog)
        if (grepl("Pacific", sector)) axis(1)
     
        #polygon(expand.grid(x = min_max, y = den.range)[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
        #usr <- par("usr")
        polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
        if (sector %in% c("Atlantic", "EastPacific")) mtext("max", side = 2)
        text(seclab[1], den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
        for (k in seq_along(lcols)) {
          vals <- max[decade == decselect(k)]
          if (length(vals) < 1 | all(is.na(vals))) next;
          if (length(vals) < 1) next;
          wgt <- area[decade == decselect(k)]
          dens.df <- do_density(scaleval(vals), w = wgt)
          lines(unscaleval(dens.df$x), dens.df$y, col=lcols[k], lwd=lwdths[k])
        }
       # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
      #       col = paste0(sector_colour(sector),40))
        box()
      
        
      }
      
      )
      
    }
    mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
    ## SPARKLINES
    for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
      asub <- subset(summ_tab, SectorName == sector & Zone == zone & Season == seas)
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next; 
      } 
      
      shouldersub <- subset(summ_tab, SectorName == sector & Zone == zone & Season == c(Summer = "Spring", Winter = "Autumn")[seas])
      
      
      with(asub, {
        sparkline_yrange <- range(c(min, shouldersub$min))
        plot(sparkline_range, sparkline_yrange, type = "n", axes = FALSE, xlab = "", ylab = "")
        segmentlines(cbind(date, min), col = lcols[decade])
        abline(h = mean(min))
        textheadtail(date, min)
        
        ## do the shoulder season
        segmentlines(cbind(shouldersub$date, shouldersub$min), col = lcols[decade], lty = 2)
        
        
        sparkline_yrange <- range(c(max, shouldersub$max))
        plot(sparkline_range, sparkline_yrange, type = "n", axes = FALSE, xlab = "", ylab = "")
        segmentlines(cbind(date, max), col = lcols[decade])
        abline(h = mean(max))
        textheadtail(date, max)
        
        
        ## do the shoulder season
        segmentlines(cbind(shouldersub$date, shouldersub$max), col = lcols[decade], lty = 2)
        
      }
      )
      
    }
    par(op)
    
  }}

if (dplot) dev.off()


