
layout_m <- function() {
  tx <- textConnection(
    "1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
  1,1,9,9,9,9,9,1,3,3,11,11,11,11,11,3
  1,1,9,9,9,9,9,1,3,3,11,11,11,11,11,3
  1,1,9,9,9,9,9,1,3,3,11,11,11,11,11,3
  1,1,9,9,9,9,9,1,3,3,11,11,11,11,11,3
  1,1,9,9,9,9,9,1,3,3,11,11,11,11,11,3
  1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
  2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
  2,2,10,10,10,10,10,2,4,4,12,12,12,12,12,4
  2,2,10,10,10,10,10,2,4,4,12,12,12,12,12,4
  2,2,10,10,10,10,10,2,4,4,12,12,12,12,12,4
  2,2,10,10,10,10,10,2,4,4,12,12,12,12,12,4
  2,2,10,10,10,10,10,2,4,4,12,12,12,12,12,4
  2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
  5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7
  5,5,13,13,13,13,13,5,7,7,15,15,15,15,15,7
  5,5,13,13,13,13,13,5,7,7,15,15,15,15,15,7
  5,5,13,13,13,13,13,5,7,7,15,15,15,15,15,7
  5,5,13,13,13,13,13,5,7,7,15,15,15,15,15,7
  5,5,13,13,13,13,13,5,7,7,15,15,15,15,15,7
  5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7
  5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7
  5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7
  5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7
  6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8
  6,6,14,14,14,14,14,6,8,8,16,16,16,16,16,8
  6,6,14,14,14,14,14,6,8,8,16,16,16,16,16,8
  6,6,14,14,14,14,14,6,8,8,16,16,16,16,16,8
  6,6,14,14,14,14,14,6,8,8,16,16,16,16,16,8
  6,6,14,14,14,14,14,6,8,8,16,16,16,16,16,8
  6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8
  6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8
  6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8
  6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8")
  as.matrix(read.csv(tx, header=F))
}
textheadtail <- function(x, y) {
  xx <- c(head(x, 1), tail(x, 1))
  yy <- c(head(y, 1), tail(y, 1))
  points(xx, yy, pch=19, cex=0.3, col=c("grey","black"))
  text(xx, yy, label = round(yy, 1), pos=2, xpd=NA, cex=0.65, offset=0.2)
  
}
path2seg <- function(x) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)
}
segmentlines <- function(x, col) {
  ind <- path2seg(seq(nrow(x)))
  segments(x[ind[,1], 1], x[ind[,1], 2], x[ind[,2], 1], x[ind[,2], 2], 
           col = col)
}
sector_colour <- function(secname) {
  setNames(c("#7CAE00", "#00BFC4","#C77CFF", "#F8766D"), 
           c("Atlantic","Indian", "EastPacific","WestPacific"))[secname]
  
}
sector_name <- function(secname) {
  setNames(c("Atlantic","Indian","East\nPacific","West\nPacific"), 
           c("Atlantic","Indian", "EastPacific","WestPacific"))[secname]
}
decselect <- function(n) {
  stopifnot(length(n) == 1L)
  c("1980-1992", "1991-2004","2002-2016")[n]
}
do_density <- function(v) {
  the.his <- hist(v, breaks=50, plot = FALSE)
  multiplier <- (the.his$counts / the.his$density)[1]
  the.den <- density(v, from=min(v), to=max(v))
  scl <- function(x) (x - min(x))/diff(range(x))
  the.den.df<- data.frame(x=the.den$x, y=scl(the.den$y))
  the.den.df <- the.den.df[the.den.df$x >= min(v) & the.den.df$x <= max(v),]
  the.den.df$y[the.den.df$y > 1] <- 1
  the.den.df
}
decade_maker <- function(x) {
  cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
}

library(aceecostats)
library(raster)
library(feather)
library(dplyr)
aes_region$index <- seq(nrow(aes_region))
aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")

## update for each variable 

do_sst <- do_ice <- do_mag <- FALSE
do_sst <- TRUE
maxdate <- ISOdatetime(2016, 9, 1, 0, 0, 0, tz = "GMT")
sparkline_range <- ISOdatetime(c(1980, 2016), c(1, 11), 1, 0, 0, 0, tz = "GMT")
if (do_mag) {
 outpdf <- "mag_assess_02.pdf"
  ras <- raster("/mnt/acebulk/mag.grd")
  cell_tab <- read_feather("summaries/mag_tab.feather") %>% 
    filter(min_mag > 0) %>% 
    rename(min = min_mag, max = max_mag, mean = mean_mag) %>% 
    mutate(min = log(min), max = log(max), mean = log(mean)) 
  varlabel <- function(ttext) {
    bquote(.(ttext)~ "log(EKE) m/s") 
  }
  
  min_max <- c(-9, 1.3)
  
  decselect <- function(n) {
    stopifnot(length(n) == 1L)
    c("1991-2004","2002-2016")[n]
  }
 # lcols <- c("gray40", "black")
}
if (do_ice) {
  outpdf <- "ice_assess_02.pdf"
  ras <- raster("/mnt/acebulk/ice.grd")
  cell_tab <- read_feather("summaries/ice_tab.feather") %>% 
    rename(min = min_ice, max = max_ice, mean = mean_ice)
  varlabel <- function(ttext) {
    bquote(.(ttext)~ "ICE %") 
  }
  min_max <- c(-5, 105)
}

if (do_sst) {
  outpdf <- "sst_assess_02.pdf"
  ras <- raster("/mnt/acebulk/sst.grd")
  cell_tab <- read_feather("summaries/sst_tab.feather") %>% 
    rename(min = min_sst, max = max_sst, mean = mean_sst)
  varlabel <- function(ttext) {
    bquote(.(ttext)~ "SST" ~ (degree*C)) 
  }
  min_max <- c(-5, 30)
}





cell_tab <- cell_tab %>% 
  mutate(decade = decade_maker(date)) %>% 
  filter(date <  maxdate) %>% 
  filter(!is.na(decade))

ucell <- distinct(cell_tab, cell_)
ucell$index <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_region)), 
                    aes_region)$index

## summ_tab is the mean values over time
summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
  mutate(Season = aes_season(date)) %>% 
  group_by(Season, Zone, decade, SectorName,  date) %>%
  summarize(min = mean(min), max = mean(max)) %>% 
  ungroup()

## raw_tab is all the cell values for density plots
raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
  mutate(Season = aes_season(date))


dummyplot <- function() plot(1, 1, type = "p", axes = FALSE, xlab = "", ylab = "")

lwdths <- c(6,4,2)
lcols <- c("gray70","gray40", "black")
den.range <- c(0, 2)
dplot <- TRUE
if (dplot) pdf(outpdf)

for (seas in c("Spring", "Summer", "Autumn", "Winter")) {
  for (zone in c("Polar",  "Temperate")) {
#for (seas in "Spring") {
#  for (zone in "Polar") {
    layout(layout_m())
    op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.95, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
    
    for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
      titletext<- paste(seas, zone)
      asub <- subset(raw_tab, SectorName == sector & Zone == zone & Season == seas)
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next;
      }
      with(asub, {
        plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
        if (sector %in% c("Atlantic", "EastPacific")) mtext("min", side = 2)
        text(0, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
        for (k in seq_along(lcols)) {
          vals <- min[decade == decselect(k)]
         
          if (length(vals) < 1 | all(is.na(vals))) next;
        
          dens.df <- do_density(vals)
          lines(dens.df, col=lcols[k], lwd=lwdths[k])
        }
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
             col = paste0(sector_colour(sector),40))
        box()
        mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
        plot(min_max, den.range, type = "n", axes = FALSE, xlab = "", ylab = "max")
        if (sector %in% c("Atlantic", "EastPacific")) mtext("max", side = 2)
        text(0, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
        for (k in seq_along(lcols)) {
          vals <- min[decade == decselect(k)]
          if (length(vals) < 1 | all(is.na(vals))) next;
          if (length(vals) < 1) next;
          dens.df <- do_density(max[decade == decselect(k)])
          lines(dens.df, col=lcols[k], lwd=lwdths[k])
        }
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
             col = paste0(sector_colour(sector),40))
        box()
        if (grepl("Pacific", sector)) axis(1)
        mtext(side=1, varlabel(titletext) ,outer =T, line=1.5, cex=1)
      }
      )
      
    }
    

    for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
      asub <- subset(summ_tab, SectorName == sector & Zone == zone & Season == seas)
      if (nrow(asub) < 10) {
        dummyplot()
        dummyplot()
        next; 
      } 
      with(asub, {
        #plot(range(date), range(min), type = "n", axes = FALSE, xlab = "", ylab = "")
        plot(sparkline_range, range(min), type = "n", axes = FALSE, xlab = "", ylab = "")
        segmentlines(cbind(date, min), col = lcols[decade])
        abline(h = mean(min))
        textheadtail(date, min)
       # box()
        #plot(range(date), range(max), type = "n", axes = FALSE, xlab = "", ylab = "")
        plot(sparkline_range, range(max), type = "n", axes = FALSE, xlab = "", ylab = "")
        segmentlines(cbind(date, max), col = lcols[decade])
        abline(h = mean(max))
        textheadtail(date, max)
       # box()
        
      }
      )
      
    }
    par(op)
    
  }}

if (dplot) dev.off()

1
