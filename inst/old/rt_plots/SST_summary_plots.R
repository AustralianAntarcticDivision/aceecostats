load("/home/dougt/assessment/min_max_monthly_SST.Rdata")
library(rgdal)
llpolys<- readOGR("/home/shared/data/assessment/sectors", "BioregionCLIP_Longlat_aggregated")
library(raadtools)
library(rgdal)

library(plyr)
library(ggplot2)

season <-
  function(x) {
    x <- (as.POSIXlt(x)$mon) + 1L
    c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")[x]
  }

dmax <- data.frame(BathyClass = rep(llpolys$BathyClass, ncol(llmax)), SectorName = rep(llpolys$SectorName, ncol(llmax)),
                   Zone = rep(llpolys$Zone, ncol(llmax)), dts = rep(seasondates,each = nrow(llmax)), 
                   llmax = as.vector(llmax))
dmax$SectorName <- factor(dmax$SectorName, levels = c("Atlantic", "Indian", "WestPacific", "EastPacific"))
levels(dmax$SectorName) <- c("Atlantic", "Indian", "West Pacific", "East Pacific")
dmax$Season <- season(dmax$dts)
dmax$Season <- factor(dmax$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))
levels(dmax$Season) <- c("Summer", "Autumn", "Winter", "Spring")

dmin <- data.frame(BathyClass = rep(llpolys$BathyClass, ncol(llmin)), SectorName = rep(llpolys$SectorName, ncol(llmin)),
                   Zone = rep(llpolys$Zone, ncol(llmin)), dts = rep(seasondates,each = nrow(llmin)), 
                   llmin = as.vector(llmin))
dmin$SectorName <- factor(dmin$SectorName, levels = c("Atlantic", "Indian", "WestPacific", "EastPacific"))
levels(dmin$SectorName) <- c("Atlantic", "Indian", "West Pacific", "East Pacific")
dmin$Season <- season(dmin$dts)
dmin$Season <- factor(dmin$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))
levels(dmin$Season) <- c("Summer", "Autumn", "Winter", "Spring")

calc.d<- function(the.lldata,the.llpolys){
  d <- NULL
  
  for (i in seq_len(ncol(the.lldata[[1]]))) d <- rbind(d,  
                                                       data.frame(SectorName = rep(the.llpolys$SectorName, unlist(sapply(the.lldata, nrow))), year = (1981:2014)[i], 
                                                                  sst = unlist(lapply(the.lldata, function(x) x[,i,drop = TRUE]))))
  
  d$dec <- cut(d$year, c(1980, 1991, 2002, 2013), lab = c("1980-1991", "1991-2002","2002-2013"))
  
  d$SectorName <- factor(d$SectorName, levels = c("Atlantic", "Indian", "WestPacific", "EastPacific"))
  levels(d$SectorName) <- c("Atlantic", "Indian", "West Pacific", "East Pacific")
  d
}

d.max.summ.t<- calc.d(llmaxsummertem, llpolystemperate)
d.max.wint.t<- calc.d(llmaxwintertem, llpolystemperate)

d.max.summ.p<- calc.d(llmaxsummerpol, llpolyspolar)
d.max.wint.p<- calc.d(llmaxwinterpol, llpolyspolar)

d.min.summ.t<- calc.d(llminsummertem, llpolystemperate)
d.min.wint.t<- calc.d(llminwintertem, llpolystemperate)

d.min.summ.p<- calc.d(llminsummerpol, llpolyspolar)
d.min.wint.p<- calc.d(llminwinterpol, llpolyspolar)

# Time series
# max
polar.subset.max <- subset(dmax, Zone == "Polar" & BathyClass != "Continent")
psmax <- ddply(polar.subset.max, .(dts, Season, SectorName), summarize, sst=max(llmax,na.rm=TRUE))
temperate.subset.max <- subset(dmax, Zone == "Temperate")
tsmax <- ddply(temperate.subset.max, .(dts, Season, SectorName), summarize, sst=max(llmax,na.rm=TRUE))

## min
polar.subset.min <- subset(dmin, Zone == "Polar" & BathyClass != "Continent")
psmin <- ddply(polar.subset.min, .(dts, Season, SectorName), summarize, sst=min(llmin,na.rm=TRUE))
temperate.subset.min <- subset(dmin, Zone == "Temperate")
tsmin <- ddply(temperate.subset.min, .(dts, Season, SectorName), summarize, sst=min(llmin,na.rm=TRUE))



## new version of plots - 8 main panels for each figure (4 sectors x 2 seasons) and sub-panels for  time series

layout.mat<-textConnection(
  "1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3
1,1,1,1,9,9,9,9,9,1,3,3,3,3,11,11,11,11,11,3
  1,1,1,1,9,9,9,9,9,1,3,3,3,3,11,11,11,11,11,3
  1,1,1,1,9,9,9,9,9,1,3,3,3,3,11,11,11,11,11,3
  1,1,1,1,9,9,9,9,9,1,3,3,3,3,11,11,11,11,11,3
  1,1,1,1,9,9,9,9,9,1,3,3,3,3,11,11,11,11,11,3
  1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3
  2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,10,10,10,10,10,2,4,4,4,4,12,12,12,12,12,4
  2,2,2,2,10,10,10,10,10,2,4,4,4,4,12,12,12,12,12,4
  2,2,2,2,10,10,10,10,10,2,4,4,4,4,12,12,12,12,12,4
  2,2,2,2,10,10,10,10,10,2,4,4,4,4,12,12,12,12,12,4
  2,2,2,2,10,10,10,10,10,2,4,4,4,4,12,12,12,12,12,4
  2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4
  5,5,5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7,7,7
  5,5,5,5,13,13,13,13,13,5,7,7,7,7,15,15,15,15,15,7
  5,5,5,5,13,13,13,13,13,5,7,7,7,7,15,15,15,15,15,7
  5,5,5,5,13,13,13,13,13,5,7,7,7,7,15,15,15,15,15,7
  5,5,5,5,13,13,13,13,13,5,7,7,7,7,15,15,15,15,15,7
  5,5,5,5,13,13,13,13,13,5,7,7,7,7,15,15,15,15,15,7
  5,5,5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7,7,7
  5,5,5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7,7,7
  5,5,5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7,7,7
  5,5,5,5,5,5,5,5,5,5,7,7,7,7,7,7,7,7,7,7
  6,6,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8
  6,6,6,6,14,14,14,14,14,6,8,8,8,8,16,16,16,16,16,8
  6,6,6,6,14,14,14,14,14,6,8,8,8,8,16,16,16,16,16,8
  6,6,6,6,14,14,14,14,14,6,8,8,8,8,16,16,16,16,16,8
  6,6,6,6,14,14,14,14,14,6,8,8,8,8,16,16,16,16,16,8
  6,6,6,6,14,14,14,14,14,6,8,8,8,8,16,16,16,16,16,8
  6,6,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8
  6,6,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8
  6,6,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8
  6,6,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8")
m<- as.matrix(read.csv(layout.mat, header=F))
his.breaks <- seq(-1.8, 32.5, length = 50)  ## yahcothol

sec.cols<- data.frame(sector=c("Atlantic","Indian", "East Pacific","West Pacific"),col=c("#7CAE00", "#00BFC4","#C77CFF", "#F8766D"),lab=c("Atlantic","Indian","East\nPacific","West\nPacific"))

SST_seasonplot<- function(lat_band,season, to.pdf=T, den.max=80000, xmax=24){

  titletext<- paste(season, lat_band)
  if(season%in%c("summer","Summer")){
    if(lat_band%in%c("polar","Polar")){
      d.max<- d.max.summ.p 
      d.min<- d.min.summ.p
    }else{
      d.max<- d.max.summ.t 
      d.min<- d.min.summ.t
    }}else{
      if(lat_band%in%c("polar","Polar")){
        d.max<- d.max.wint.p 
        d.min<- d.min.wint.p
      }else{
        d.max<- d.max.wint.t 
        d.min<- d.min.wint.t
        }
      }

  if(lat_band=="Polar"){
    llpolys<- llpolyspolar
    min.ts<- psmin
    max.ts<- psmax
    }else{
    llpolys<- llpolystemperate
    min.ts<- tsmin
    max.ts<- tsmax
    }
  temp.min<- -2
  temp.max<- xmax
  
 if(to.pdf) pdf(file=paste0(lat_band,season,"_SST_summary_plot.pdf"), height=6, width=4)
  layout(m)
  par(mar=c(0,0,0,0), oma=c(2.5, 1, 0.5, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
  
  # 2 loops: first pdfs then time series sparklines 

  for (i in 1:4){ # one iteration for each sector; do min then max within each
    the.sector<- sec.cols$sector[i]
    for(j in 1:2){  
      if(j==1){d<- d.min}else{d<-d.max} # min then max
      sec.dat<- d[d$Sector%in%the.sector,]
      bgcol<- sec.cols[sec.cols$sector%in%the.sector,]$col
      plot(c(temp.min,temp.max), c(0,den.max), type="n", axes=F, xlab="", ylab="")
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = paste0(bgcol,40))
      
      if(lat_band=="polar"&j==1) text(20, den.max*0.125, sec.cols$lab[i], cex=0.5)
      if(lat_band=="temperate"&j==1) text(2.3, den.max*0.8, sec.cols$lab[i], cex=0.5)
      
      
      if(i>2 & j==2){axis(1)}else{axis(1, labels=F)}  
      for(k in 1:3){
        lwdths <- c(6,3,1)
        lcols <- c("gray70","gray40", "black")
        sec.dec.dat<- sec.dat[as.numeric(sec.dat$dec)==k,]
        
        the.his<- hist(na.omit(sec.dec.dat$sst), breaks=his.breaks, plot = FALSE)
        #print(range(the.his$counts))
        #print(range(the.his$breaks))
        
        multiplier<- na.omit((the.his$counts / the.his$density))[1]
        the.den<- density(na.omit(sec.dec.dat$sst), from=min(sec.dec.dat$sst, na.rm=T), to=max(sec.dec.dat$sst, na.rm=T))
        the.den.df<- data.frame(x=the.den$x, y=the.den$y*multiplier)
       # print(summary(the.den.df))
        #the.den.df <- the.den.df[the.den.df$x>=min(sec.dec.dat$sst, na.rm=T)&the.den.df$x<=max(sec.dec.dat$sst, na.rm=T),]
        ##the.den.df$y[the.den.df$y>1]<-1
        
        lines(the.den.df, col=lcols[k], lwd=lwdths[k])
      }  
    if(i %in% c(1,3) ){
      if(j==1) mtext("min", side=2) else mtext("max", side=2)
    }
      }
    
  }
  box()
  mtext(side=1, bquote(.(titletext)~ "SST" ~ (degree*C)) ,outer =T, line=1.5, cex=1)
  
  # time series data sparklines
  for(i in 1:4){
    the.sector<- sec.cols$sector[i]
    for(j in 1:2){
    if(lat_band%in%c("polar","Polar")){min.ts<- psmin; max.ts<- psmax}else{min.ts<- tsmin; max.ts<- tsmax}
    if(season%in%c("Summer","summer")){
      min.ts<- min.ts[min.ts$Season%in%c("Summer","Autumn"),]
      max.ts<- max.ts[max.ts$Season%in%c("Summer","Autumn"),]
    }else{
      min.ts<- min.ts[min.ts$Season%in%c("Winter","Spring"),]
      max.ts<- max.ts[max.ts$Season%in%c("Winter","Spring"),]
    }
    if(j==1){the.ts<- min.ts}else{the.ts<- max.ts}
      the.ts<- the.ts[the.ts$Sector%in%the.sector,]
      the.ts$year<- (as.POSIXlt(the.ts$dts)$year+1900)
      the.ts$dec <- cut(the.ts$year, c(1978, 1989, 2000, 2012), lab = c(1989, 2000, 2012))
      with(the.ts, plot(dts, sst, type="n", axes=F))
      
      ts<- the.ts[the.ts$Season%in%c("Summer", "Winter"),]
      lines(ts[ts$year<=1991,]$dts, ts[ts$year<=1991,]$sst, lwd=1, col=lcols[1])
      lines(ts[ts$year>=1991&ts$year<=2002,]$dts, ts[ts$year>=1991&ts$year<=2002,]$sst, lwd=1, col=lcols[2])
      lines(ts[ts$year>=2002,]$dts, ts[ts$year>=2002,]$sst, lwd=1, col=lcols[3])
      
      ts<- the.ts[the.ts$Season%in%c("Autumn", "Spring"),]
      lines(ts[ts$year<=1991,]$dts, ts[ts$year<=1991,]$sst, lwd=1, col=lcols[1], lty="dashed")
      lines(ts[ts$year>=1991&ts$year<=2002,]$dts, ts[ts$year>=1991&ts$year<=2002,]$sst, lwd=1, col=lcols[2], lty="dashed")
      lines(ts[ts$year>=2002,]$dts, ts[ts$year>=2002,]$sst, lwd=1, col=lcols[3], lty="dashed")
      
      points(c(min(the.ts$dts),max(the.ts$dts)), c(the.ts$sst[the.ts$dts==min(the.ts$dts)], the.ts$sst[the.ts$dts==max(the.ts$dts)]), pch=19, cex=0.3, col=c("grey","black"))
      lines(x=c(min(the.ts$dts),max(the.ts$dts)), y=rep(mean(the.ts$sst),2), lwd=0.5, col=rgb(.4,.4,.4,.4))
    
    firstsst<- the.ts$sst[the.ts$dts==min(the.ts$dts)]
    lastsst<- the.ts$sst[the.ts$dts==max(the.ts$dts)]
    
    text(labels=as.character(round(firstsst,1)), x=min(the.ts$dts), y=firstsst, pos=2, xpd=NA, cex=0.3, offset=0.2)
    text(labels=as.character(round(lastsst,1)), x=max(the.ts$dts), y=lastsst, pos=4, xpd=NA, cex=0.3, offset=0.2)
    }
   
    } 
 if(to.pdf) dev.off()
}

SST_seasonplot(lat_band = "polar" ,season = "summer", den.max=200000)
SST_seasonplot(lat_band = "polar" ,season = "winter", den.max=200000)

SST_seasonplot(lat_band = "temperate" ,season = "summer", xmax=29)
SST_seasonplot(lat_band = "temperate" ,season = "winter", , xmax=29)
