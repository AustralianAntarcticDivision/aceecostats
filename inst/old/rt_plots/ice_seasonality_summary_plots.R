library(raadtools)

dp <- "/home/shared/data/assessment/seaiceseason"
load(file.path(dp, "south_advret.Rdata"))

## number of days between advance and retreat
duration <- retreat - advance

duration <- setZ(duration, seq(as.Date("1979-01-01"), by = "1 year", length = nlayers(duration)))
names(duration) <- paste("ice season", 1979:2013)
library(rgdal)
apolys<- readOGR("/home/shared/data/assessment/sectors", "BioregionCLIP_Longlat_aggregated")

### double check with mike that the following subset step should still happen 

sduration <- calc(duration, sd, na.rm = TRUE)
ex <- extract(duration, apolys, fun = mean, na.rm = TRUE)
rownames(ex) <- apolys$Name

nms <- gsub("_Polar_Continent", "", apolys$Name)
nms[1:2] <- c("AtlanticEast", "AtlanticWest")

apolys.continent <- subset(apolys, BathyClass == "Continent")
apolys.polar <- subset(apolys, Zone == "Polar")


d.p <- NULL
apolys.polar <- subset(apolys, Zone == "Polar")
mex.p <- extract(duration, apolys.polar)

for (i in seq_len(ncol(mex.p[[1]]))) d.p <- rbind(d.p,  
                                              data.frame(Name = rep(apolys.polar$Name, unlist(sapply(mex.p, nrow))),SectorName = rep(apolys.polar$SectorName, unlist(sapply(mex.p, nrow))), year = (1979:2012)[i], 
                                                         duration = unlist( lapply(mex.p, function(x) x[,i,drop = TRUE]))))

d.p$dec <- cut(d.p$year, c(1978, 1989, 2000, 2012), lab = c(1989, 2000, 2012))


d.p$SectorName <- factor(d.p$SectorName, levels = c("Atlantic", "Indian", "WestPacific", "EastPacific"))
levels(d.p$SectorName) <- c("Atlantic", "Indian", "West Pacific", "East Pacific")

d.c <- NULL
apolys.continent <- subset(apolys, BathyClass == "Continent")
mex.c <- extract(duration, apolys.continent)

for (i in seq_len(ncol(mex.c[[1]]))) d.c <- rbind(d.c,  
                                              data.frame(Name = rep(apolys.continent$Name, unlist(sapply(mex.c, nrow))),SectorName = rep(apolys.continent$SectorName, unlist(sapply(mex.c, nrow))), year = (1979:2012)[i], 
                                                         duration = unlist( lapply(mex.c, function(x) x[,i,drop = TRUE]))))

d.c$dec <- cut(d.c$year, c(1978, 1989, 2000, 2012), lab = c(1989, 2000, 2012))


d.c$SectorName <- factor(d.c$SectorName, levels = c("Atlantic", "Indian", "WestPacific", "EastPacific"))
levels(d.c$SectorName) <- c("Atlantic", "Indian", "West Pacific", "East Pacific")


##### Timeseries plots

ex <- extract(duration, apolys, fun = mean, na.rm = TRUE)
sduration <- extract(duration, apolys, fun = sd, na.rm = TRUE)

d.ts <- data.frame(BathyClass = rep(apolys$BathyClass, ncol(ex)), SectorName = rep(apolys$SectorName, ncol(ex)),
                Zone = rep(apolys$Zone, ncol(ex)), year = rep((1979:2013),each = nrow(ex)),
                meanduration = as.vector(ex),sduration = as.vector(sduration))
d.ts$SectorName <- factor(d.ts$SectorName, levels = c("Atlantic", "Indian", "WestPacific", "EastPacific"))
levels(d.ts$SectorName) <- c("Atlantic", "Indian", "West Pacific", "East Pacific")

polar.subset <- subset(d.ts, Zone == "Polar" & BathyClass != "Continent")
psm <- aggregate(polar.subset,list(polar.subset$year,polar.subset$SectorName),FUN="mean",na.rm=TRUE)

continent.subset <- subset(d.ts, BathyClass == "Continent")
csm <- aggregate(continent.subset,list(continent.subset$year,continent.subset$SectorName),FUN="mean",na.rm=TRUE)



layout.mat<- textConnection(
"1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,5,5,5,5,5,5,1,1,1,1,1,1,1,3,7,7,7,7,7,7,3,3,3,3,3,3,3
1,5,5,5,5,5,5,1,1,1,1,1,1,1,3,7,7,7,7,7,7,3,3,3,3,3,3,3
1,5,5,5,5,5,5,1,1,1,1,1,1,1,3,7,7,7,7,7,7,3,3,3,3,3,3,3
1,5,5,5,5,5,5,1,1,1,1,1,1,1,3,7,7,7,7,7,7,3,3,3,3,3,3,3
1,5,5,5,5,5,5,1,1,1,1,1,1,1,3,7,7,7,7,7,7,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,6,6,6,6,6,6,2,2,2,2,2,2,2,4,8,8,8,8,8,8,4,4,4,4,4,4,4
2,6,6,6,6,6,6,2,2,2,2,2,2,2,4,8,8,8,8,8,8,4,4,4,4,4,4,4
2,6,6,6,6,6,6,2,2,2,2,2,2,2,4,8,8,8,8,8,8,4,4,4,4,4,4,4
2,6,6,6,6,6,6,2,2,2,2,2,2,2,4,8,8,8,8,8,8,4,4,4,4,4,4,4
2,6,6,6,6,6,6,2,2,2,2,2,2,2,4,8,8,8,8,8,8,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4
2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4"
  )
m<- as.matrix(read.csv(layout.mat, header=F))

ice_seasonplot<- function(lat.band, plot.var="Sea ice duration", to.pdf=T,  x.min=0){
  if(lat.band=="polar") {d<- d.p; ts.dat<- psm} #;   fig.y<- c(0.8,0.575,0.35,0.05); y.plus<-0.2}
  if(lat.band=="continent") {d<- d.c; ts.dat <- csm}#; fig.y<- c(0.7,0.45,0.2,0); y.plus<-0.25}
  
  y.max<- 12100
  
  the.title<- paste0(plot.var, " (", lat.band, ", days)")
  lcols <- c("gray70","gray40", "black")
  sec.cols<- data.frame(sector=c("Atlantic","Indian","West Pacific", "East Pacific"),col=c("#7CAE00", "#00BFC4", "#F8766D","#C77CFF"),lab=c("Atlantic","Indian","West\nPacific","East\nPacific"))
  sec.order<- c("Atlantic","East Pacific","Indian", "West Pacific")
  sec.cols<- sec.cols[match(sec.order, sec.cols$sector),]
  
  
  lwdths <- c(6,3,1)
  
  if(to.pdf) pdf(file=paste0(the.title,".pdf"), height=3, width=4)
#   par(mfrow=c(4,1), mar=c(0,0,0,0), oma=c(2, 0.5, 0.55, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75)
  layout(m)
  par(mar=c(0,0,0,0), oma=c(2.5, 0.5, 0.55, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
  # note, can truncate axis at 365 by adding xaxs="i" to par statement above
  for(i in 1:4){
    the.sector<- sec.cols$sector[i]
    sec.dat<- d[d$Sector%in%the.sector,]
    bgcol<- sec.cols[sec.cols$sector%in%the.sector,]$col
    # plot years
    plot(c(x.min,365), c(0,y.max), type="n", axes=F, xlab="", ylab="") 
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = paste0(bgcol,40))
    for(j in 1:3){
      # set up axes
      sec.dec.dat<- sec.dat[as.numeric(sec.dat$dec)==j,]
    
      the.his<- hist(na.omit(sec.dec.dat$duration), breaks=50, plot = FALSE)
      multiplier<- na.omit((the.his$counts / the.his$density))[1]
      
      the.den<- density(na.omit(sec.dec.dat$duration), from=min(sec.dec.dat$duration, na.rm=T), to=max(sec.dec.dat$duration, na.rm=T))
      the.den.df<- data.frame(x=the.den$x, y=the.den$y*multiplier)
      the.den.df <- the.den.df[the.den.df$x<=365&the.den.df$x>min(sec.dec.dat$duration, na.rm=T),]
      print(range(the.den.df$y))
      lines(the.den.df, col=lcols[j], lwd=lwdths[j])
    }
    box()
    if(i%%2) axis(3, labels=F) else {axis(1); mtext(side=1, the.title,outer =T, line=1.15, cex=1)}
    text(320, y.max*0.9, sec.cols[sec.cols$sector%in%the.sector,]$lab, cex=0.5)
    abline(lty="dotted", v=365)
  }
  
 for(k in 1:4){
    the.sector<- sec.cols$sector[k]
    the.ts<- ts.dat[ts.dat$Group.2%in%the.sector,]
    fig.x<- 0.1
    the.lower<- the.ts$meanduration-the.ts$sduration
    the.upper<- the.ts$meanduration+the.ts$sduration
    plot(c(1979, 2013), c(min(the.lower),max(the.upper)), type="n", axes=F)

    lines(the.ts[the.ts$year<=1991,]$year, the.ts[the.ts$year<=1991,]$meanduration, lwd=1, col=lcols[1])
    lines(the.ts[the.ts$year>=1991&the.ts$year<=2002,]$year, the.ts[the.ts$year>=1991&the.ts$year<=2002,]$meanduration, lwd=1, col=lcols[2])
    lines(the.ts[the.ts$year>=2002,]$year, the.ts[the.ts$year>=2002,]$meanduration, lwd=1, col=lcols[3])

    polygon(c(the.ts$year, rev(the.ts$year)), c(the.lower, rev(the.upper)), col=rgb(.4,.4,.4,.2), border=NA)

    points(c(1979,2013), c(the.ts$meanduration[the.ts$year==1979], the.ts$meanduration[the.ts$year==2013]), pch=19, cex=0.3, col=c("grey","black"))
    points(c(1991,2002), c(the.ts$meanduration[the.ts$year==1991], the.ts$meanduration[the.ts$year==2002]), pch="|", cex=0.3)
    lines(x=c(1979,2013), y=rep(mean(the.ts$meanduration),2), lwd=0.5, col=rgb(.4,.4,.4,.4))
    text(labels=as.character(round(the.ts$meanduration[the.ts$year==1979])), x=1979, y=the.ts$meanduration[the.ts$year==1979], pos=1, xpd=NA, cex=0.3)
    text(labels=as.character(round(the.ts$meanduration[the.ts$year==2013])), x=2013, y=the.ts$meanduration[the.ts$year==2013], pos=3, xpd=NA, cex=0.3)
   }
  
    
  if(to.pdf) dev.off() 
}


 ice_seasonplot("polar", to.pdf=T)
 ice_seasonplot("continent", to.pdf=T)
