tabit <- function(x) {
  tibble(val = values(x), cell_ = seq(ncell(x))) %>% filter(!is.na(val))
}

decade_maker <- function(x) {
  #cut(as.integer(format(x, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))
  cut(as.integer(format(x, "%Y")), c(1981, 1990, 1999, 2008, 2016), lab = c("1981-1990", "1990-1999","1999-2008", "2008-2016"))
}

library(raster)
outf <- "/mnt/acebulk"
ret <- readRDS(file.path(outf, "south_retreat.rds"))

adv <- readRDS(file.path(outf,"south_advance.rds") )
duration <- ret - adv
## if retreat is equal to one, it didn't retreat
duration[ret == 1] <- 365
obj <- setZ(duration, ISOdatetime(1979:2015, 2, 15, 0, 0, 0, tz = "GMT"))


library(raster)
library(tibble)
library(dplyr)

library(aceecostats)
library(feather)
aes_region$index <- seq(nrow(aes_region))
aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")
gridarea <- readRDS(file.path(outf,"nsidc_south_area.rds"))/1e6
## put a tidy end to the series
maxdate <- ISOdatetime(2016, 9, 1, 0, 0, 0, tz = "GMT")

vars <- "seaice_duration" 
ivar <- 1
  ras <- raster(obj)
  ## unique integer from 0 to ~nrow(sf)/90 for each three month period
  #segs <- cumsum(c(0, abs(diff(unclass(factor(aes_season(getZ(obj))))))))
  
  listtab <- vector("list", nlayers(obj))
  dates <- as.POSIXct(getZ(obj))
  for (i in seq_along(listtab)) {
    asub <- i # which(segs == unique(segs)[i])
    a_obj <- subset(obj, asub)
    
    tab <- tabit(a_obj) %>% rename(dur = val) %>% mutate(date = dates[asub[1]])  
      #filter(dur > 0)
    tab$dur[tab$dur < 30] <- 365
   # tab$max<- values(max(a_obj))[tab$cell_]
   #  tab$mean <- values(mean(a_obj))[tab$cell_]
    listtab[[i]] <- tab
    print(i)
  }
  
  ## now process the summaries down
  
  cell_tab <- bind_rows(listtab) %>% 
    mutate(decade = decade_maker(date)) %>% 
    filter(date <  maxdate) %>% 
    filter(!is.na(decade))
  
  ucell <- distinct(cell_tab, cell_) %>% mutate(area = extract(gridarea, cell_))
  ucell$index <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_region)), 
                      aes_region)$index
  
  ## summ_tab is the mean values over time
  summ_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
    #mutate(Season = aes_season(date)) %>% 
    group_by(Shelf, Zone, decade, SectorName,  date) %>%
    summarize(min = min(dur), max = max(dur), mean = mean(dur)) %>% 
    ungroup()
  
  #cell_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) 
  
  ## raw_tab is all the cell values for density plots
  raw_tab <- cell_tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) 
  #%>%     mutate(Season = aes_season(date))
  
  write_feather(cell_tab,  file.path(outf, "seaice_duration_cell_tab.feather"))
  writeRaster(ras,        file.path(outf, "seaice_duration_raster.grd"))
  write_feather(summ_tab, file.path(outf, "seaice_duration_summ_tab.feather"))
  write_feather(raw_tab,  file.path(outf, "seaice_duration_raw_tab.feather"))





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
## local path to required cache files





## date range for the sparkline
sparkline_range <- ISOdatetime(c(1980, 2016), c(1, 11), 1, 0, 0, 0, tz = "GMT")

 outpdf <- "ice_assess_duration03.pdf"
  ras <- raster(file.path(outf,"seaice_duration_raster.grd"))
  cell_tab <- read_feather(file.path(outf,"seaice_duration_cell_tab.feather")) 
  raw_tab <- read_feather(file.path(outf, "seaice_duration_raw_tab.feather"))
  ##%>%   rename(min = min_ice, max = max_ice, mean = mean_ice)
  varlabel <- function(ttext) {
    bquote(.(ttext)~ "ICE SEASON (days)") 
  }
  min_max <- c(0, 365)






## plot specifics
lwdths <- c(6,4,2,1)
lcols <- grey(seq(1, 0, length = nlevels(raw_tab$decade) + 2))[-c(1, 2)]
den.range <- c(0, 2)
dplot <- TRUE
if (dplot) pdf(outpdf)

seas <- ""
zone <- "Polar"
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
        text(0, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
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
        textheadtail(date, max)
        
      
        
      }
      )
      
    }
    par(op)
    
  }

if (dplot) dev.off()

