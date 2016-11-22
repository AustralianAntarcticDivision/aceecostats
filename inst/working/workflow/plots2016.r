
layout.mat<-textConnection(
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
layout_m <- as.matrix(read.csv(layout.mat, header=F))

library(aceecostats)
library(raster)
library(feather)
library(dplyr)
aes_region$index <- seq(nrow(aes_region))
aes_region$Shelf <- ifelse(aes_region$BathyClass == "Continent", "Shelf", "Ocean")


ras <- raster("/mnt/acebulk/sst.grd")
tab <- read_feather("summaries/sst_tab.feather")
tab <- tab %>% 
  mutate(decade = cut(as.integer(format(date, "%Y")), c(1980, 1992, 2004, 2016), lab = c("1980-1992", "1991-2004","2002-2016"))) %>% 
  filter(!is.na(decade))

ucell <- distinct(tab, cell_)
ucell$index <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_region)), 
     aes_region)$index

x <- tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
  mutate(Season = aes_season(date)) %>% 
  group_by(Season, Zone, decade, SectorName, Shelf, date) %>%
  
  summarize(min = mean(min_sst), max = mean(max_sst)) %>% 
  ungroup()


raw_tab <- tab %>% inner_join(ucell %>% inner_join(aes_region@data[, c("index", "SectorName", "Zone", "Shelf")])) %>% 
  mutate(Season = aes_season(date))


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
lwdths <- c(6,4,2)
lcols <- c("gray70","gray40", "black")
den.range <- c(0, 2)

layout(layout_m)
op <- par(mar=c(0,0,0,0), oma=c(2.5, 0.5, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")

seas <- "Winter"
zone <- "Temperate"
for (sector in c("Atlantic",  "Indian", "EastPacific", "WestPacific")) {
  titletext<- paste(seas, zone)
  with(subset(raw_tab, SectorName == sector & Zone == zone & Season == seas), {
    plot(range(min_sst), den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(10, den.range[2]*0.9, lab = sector_name(sector), cex=0.5)
    for (k in seq_along(lcols)) {
      dens.df <- do_density(min_sst[decade == decselect(k)])
      lines(dens.df, col=lcols[k], lwd=lwdths[k])
    }
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
         col = paste0(sector_colour(sector),40))
    box()
    mtext(side=1, bquote(.(titletext)~ "SST" ~ (degree*C)) ,outer =T, line=1.5, cex=1)
    
    plot(range(max_sst), den.range, type = "n", axes = FALSE, xlab = "", ylab = "")
    for (k in seq_along(lcols)) {
      dens.df <- do_density(max_sst[decade == decselect(k)])
      lines(dens.df, col=lcols[k], lwd=lwdths[k])
    }
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
         col = paste0(sector_colour(sector),40))
    box()
    mtext(side=1, bquote(.(titletext)~ "SST" ~ (degree*C)) ,outer =T, line=1.5, cex=1)
    
  }
  )
  
}

par(op)



