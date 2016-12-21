
## preparation
library(aceecostats)
library(raster)
library(feather)
library(dplyr)

## local path to required cache files
datapath <- "/mnt/acebulk"

## date range for the sparkline
sparkline_domain <- aceecostats:::mk_sparkline_range()

outpdf <- "inst/workflow/graphics/sst_area_distribution_002.pdf"

ras <- raster(file.path(datapath, "sst_raster.grd"))
## cell_tab <- read_feather(file.path(datapath, "sst_cell_tab.feather")) 
summ_tab <- read_feather(file.path(datapath, "sst_summ_tab.feather")) 
raw_tab <-  read_feather(file.path(datapath, "sst_raw_tab.feather")) 
varlabel <- function(ttext) {
  bquote(.(ttext)~ "SST" ~ (degree*C)) 
}


seclab <- 0
min_max <- c(-2, 30)
usr <- c(-5, 100, -5, 6e9)
dolog <- ""

total_areas <- aes_region@data %>% group_by(SectorName, Zone) %>% summarize(area_km2 = sum(area_km2))
#total_areas$area_factor <- total_areas$area_km2 / c(Atlantic = 1.3, EastPacific = 1.4, Indian = 1.6, WestPacific = 1.15)[total_areas$SectorName]

## pre-determine the actual maximum for each related sub-plot
total_areas$den_MAXmin <- total_areas$den_MAXmax <- numeric(nrow(total_areas))
for (i in seq(nrow(total_areas))) {
  asub <- raw_tab %>% filter(SectorName == total_areas$SectorName[i], Zone == total_areas$Zone[i])
  total_areas$min_MAX[i] <- max(do_hist(asub$min, asub$area)$y)
  total_areas$max_MAX[i] <- max(do_hist(asub$max, asub$area)$y)
}
total_areas <- total_areas %>% group_by(SectorName) %>% mutate(min_MAX = max(min_MAX), max_MAX = max(max_MAX))


lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = nlevels(raw_tab$decade) + 2))[-c(1, 2)]
den.range <- c(0, 2)
dplot <- TRUE
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
      
      den.range <- c(0, this_area$min_MAX/95)
      titletext<- paste(seas, zone)
      asub <- dplyr::filter(raw_tab, SectorName == sector & Zone == zone & Season == seas)
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next;
      }
      plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
      polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
      if (sector %in% c("Atlantic", "EastPacific")) mtext("min km^2", side = 2)
      
      for (k in seq_along(lcols)) {
        vals_wgt <- asub %>% filter(decade == decselect(k)) %>% dplyr::select(min, area)
        if (nrow(vals_wgt) < 1 | all(is.na(vals_wgt$min))) next;
        dens.df <- do_hist(vals_wgt$min, w = vals_wgt$area)
        lines(dens.df, col=lcols[k], lwd=lwdths[k])
        
      }
      axis(2, cex.axis = 0.5, las = 1, mgp = c(3, -1.95, 0))
      box()
      mtext(side=1, varlabel(titletext) ,outer =TRUE, line=1.5, cex=1)
      
      
      
      plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "max km^2", log = dolog)
      polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
      if (grepl("Pacific", sector)) axis(1)
      if (sector %in% c("Atlantic", "EastPacific")) mtext("max", side = 2)
      text(seclab[1], den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
      
      for (k in seq_along(lcols)) {
        vals_wgt <- asub %>% filter(decade == decselect(k)) %>% dplyr::select(max, area)
        if (nrow(vals_wgt) < 1 | all(is.na(vals_wgt$max))) next;
        dens.df <- do_hist(vals_wgt$max, w = vals_wgt$area)
        lines(dens.df, col=lcols[k], lwd=lwdths[k])
      }
      axis(2, cex.axis = 0.5, las = 1, mgp = c(3, -1.95, 0))
      box()
      mtext(side=1, varlabel(titletext) ,outer =TRUE, line=1.5, cex=1)
      
      
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
      
#      shouldersub <- subset(summ_tab, SectorName == sector & Zone == zone & Season == c(Summer = "Spring", Winter = "Autumn")[seas])
      shouldersub <- subset(summ_tab, SectorName == sector & Zone == zone & Season == c(Summer = "Autumn", Winter = "Spring")[seas])
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

if (dplot) dev.off()


