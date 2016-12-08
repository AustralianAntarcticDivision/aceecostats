



## worker plot functions

dummyplot <- function() plot(1, 1, type = "p", axes = FALSE, xlab = "", ylab = "")

si_layout_m <- function() {
  tx <- textConnection(
    "1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,5,5,5,5,5,1,3,3,7,7,7,7,7,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,6,6,6,6,6,2,4,4,8,8,8,8,8,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4
    2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4")
  as.matrix(read.csv(tx, header=F))
}

textheadtail <- function(x, y) {
  xx <- c(head(x, 1), tail(x, 1))
  yy <- c(head(y, 1), tail(y, 1))
  points(xx, yy, pch=19, cex=0.3, col=c("grey","black"))
  text(xx, yy, label = round(yy, 1), pos=c(2, 4), xpd=NA, cex=0.65, offset=0.2)
  
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
  #c("1980-1992", "1991-2004","2002-2016")[n]
  c("1981-1990", "1990-1999","1999-2008", "2008-2016")[n]
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

 outpdf <- "ice_assess_duration04.pdf"
  ras <- raster(file.path(outf,"seaice_duration_raster.grd"))
  cell_tab <- read_feather(file.path(outf,"seaice_duration_cell_tab.feather")) 
  raw_tab <- read_feather(file.path(outf, "seaice_duration_raw_tab.feather"))
  summ_tab <- read_feather(file.path(outf, "seaice_duration_summ_tab.feather"))
  ##%>%   rename(min = min_ice, max = max_ice, mean = mean_ice)
  varlabel <- function(ttext) {
    bquote(.(ttext)~ "ICE SEASON (days)") 
  }
  min_max <- c(0, 365)






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
    layout(si_layout_m())
    op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.95, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
    ## DENSITY PLOTS
    for (sector in c("Atlantic",  "EastPacific", "Indian", "WestPacific")) {
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

