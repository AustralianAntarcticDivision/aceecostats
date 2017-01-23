
## preparation
library(aceecostats)
library(raster)
library(feather)
library(dplyr)

## local path to required cache files
datapath <- "/mnt/acebulk"

## date range for the sparkline
sparkline_domain <- aceecostats:::mk_sparkline_domain()

outpdf <- "inst/workflow/graphics/sst_area_distribution_002.pdf"

ras <- raster(file.path(datapath, "sst_raster.grd"))
## cell_tab <- read_feather(file.path(datapath, "sst_cell_tab.feather")) 
summ_tab <- read_feather(file.path(datapath, "sst_summ_tab.feather")) 
raw_tab <-  read_feather(file.path(datapath, "sst_raw_tab.feather")) 
varlabel <- function(ttext) {
  bquote(.(ttext)~ "SST" ~ (degree*C)) 
}


seclab <- 28
min_max <- c(-2, 30)
usr <- c(-5, 100, -5, 6e9)
dolog <- ""

total_areas <- aes_region@data %>% 
  group_by(SectorName, Zone) %>% 
  summarize(area_km2 = sum(area_km2))

total_areas$area_max <- total_areas$area_km2
## Atlantic/Polar
total_areas$area_max[1] <- 3e6
## Atlantic/Temperate
total_areas$area_max[2] <- 3e6
## EastPacific/Polar
total_areas$area_max[3] <-  1e5
## EastPacific/Temperate
total_areas$area_max[4] <-  2e6
## Indian/Polar
total_areas$area_max[5] <- 2e6
## Indian/Temperate
total_areas$area_max[6] <- 2.5e6
## WestPacific/Polar
total_areas$area_max[7] <- 5e5
## WestPacific/Temperate
total_areas$area_max[8] <- 5e6


lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = length(unique(alldecades)) + 2))[-c(1, 2)]

dplot <- TRUE
#seas <- "Summer"; zone <- "Polar"
# scale_factors <- expand.grid(Sector =c("Atlantic",  "Indian", "EastPacific", "WestPacific"), 
#                              Zone = c("Polar", "Temperate"), 
#                              scale_factor = 1, stringsAsFactors = FALSE)

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
    asub <- dplyr::filter(raw_tab, SectorName == sector & Zone == zone & Season == seas)
    if (nrow(asub) < 10) {
      dummyplot()
      dummyplot()
      next;
    }
    plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
    polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
    if (sector %in% c("Atlantic", "EastPacific")) mtext("min (km^2)", side = 2)
    
    for (k in seq_along(lcols)) {
      vals_wgt <- asub %>% filter(decade == decselect(k)) %>% dplyr::select(min, area)
      if (nrow(vals_wgt) < 1 | all(is.na(vals_wgt$min))) next;
      dens.df <- do_hist(vals_wgt$min, w = vals_wgt$area)
      lines(dens.df, col=lcols[k], lwd=lwdths[k])
      
    }
    axis(2, cex.axis = 0.5, las = 1, mgp = c(3, -1.95, 0))
    box()
    mtext(side=1, varlabel(titletext) ,outer =TRUE, line=1.5, cex=1)
    
    
    
    plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = , log = dolog)
    polygon(expand.grid(x = usr[1:2], y = usr[3:4])[c(1, 2, 4, 3), ], col = paste0(sector_colour(sector),40))
    if (grepl("Pacific", sector)) axis(1)
    if (sector %in% c("Atlantic", "EastPacific")) mtext("max (km^2)", side = 2)
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
    asub <- dplyr::filter(summ_tab, SectorName == sector & Zone == zone & Season == seas)
    if (nrow(asub) < 10) {
      dummyplot()
      dummyplot()
      next; 
    } 
    
    ## get the shoulder season
    shouldersub <- dplyr::filter(summ_tab, SectorName == sector & Zone == zone & Season == c(Summer = "Autumn", Winter = "Spring")[seas])
    sparkline_range <- range(c(asub$min, shouldersub$min))
    plot(sparkline_domain, sparkline_range, type = "n", axes = FALSE, xlab = "", ylab = "")
    segmentlines(cbind(asub$date, asub$min), col = lcols[asub$decade])
    ## do the shoulder season
    segmentlines(cbind(shouldersub$date, shouldersub$min), col = lcols[shouldersub$decade], lty = 2)
    abline(h = mean(asub$min))
    textheadtail(asub$date, asub$min)
    
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


