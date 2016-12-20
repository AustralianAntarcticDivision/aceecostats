library(dplyr)

load("/home/shared/data/assessment/chlorophyll/chla_SeaWiFS_1997_2001.Rdata")
# object is loaded as "chla"
p1.chla<- tbl_df(chla)

load("/home/shared/data/assessment/chlorophyll/chla_MODISA_2002_2007.Rdata")
p2.chla<- tbl_df(chla)

load("/home/shared/data/assessment/chlorophyll/chla_MODISA_2008_2013.Rdata")
p3.chla<- (chla)
rm(chla)

# summarise data ready for plotting
sec.cols<- data.frame(sector=c("Atlantic","Indian","West Pacific", "East Pacific"),col=c("#7CAE00", "#00BFC4", "#F8766D","#C77CFF"),lab=c("Atlantic","Indian","West\nPacific","East\nPacific"), stringsAsFactors =F)
sec.order<- c("Atlantic","East Pacific","Indian", "West Pacific")
sec.cols<- sec.cols[match(sec.order, sec.cols$sector),]
sec.cols$sector.s<- gsub(" ","",sec.cols$sector, fixed=T)


# 2 latitude bands, 4 sectors, 4 seasons; 2 sets of summaries - for pdf and time series 

# summary for pdf
secs.dat<- vector("list", 4)
for(i in 1:4){
  the.sector<- sec.cols$sector.s[i]
  names(secs.dat)[i]<- the.sector
  secs.dat[[i]]<- list("Polar", "Temperate")
  names(secs.dat[[i]])<- c("Polar", "Temperate")
  for(l in 1:2){
    lat.band<- c("Polar", "Temperate")[l]
    secs.dat[[i]][[l]]<- list("Spring","Summer","Autumn","Winter")
    names(secs.dat[[i]][[l]])<- c("Spring","Summer","Autumn","Winter")

    # structure of list is created at this point; code below populates with summayr data
    
    for(s in 1:4){
      the.season<- c("Spring","Summer","Autumn","Winter")[s]
    
      p1.d<- filter(p1.chla, grepl(the.sector, unit)&grepl(lat.band, unit)&season==the.season)
      p2.d<- filter(p2.chla, grepl(the.sector, unit)&grepl(lat.band, unit)&season==the.season)
      p3.d<- filter(p3.chla, grepl(the.sector, unit)&grepl(lat.band, unit)&season==the.season)
    
      secs.dat[[the.sector]][[lat.band]][[the.season]]<- list(p1=p1.d, p2=p2.d, p3=p3.d)
    }
  }
}

secs.ts<-vector("list", 4)
ts.dp<- "/home/shared/data/assessment/chlorophyll/chla_year/"
ts.fs<- list.files(ts.dp, full.names = T)

vl <- vector("list", length = length(ts.fs))
for(f in seq_along(ts.fs)){
  load(ts.fs[f])
  yr <- sapply(strsplit(gsub(".Rdata", "", basename(ts.fs[f])), "_"), function(x) x[3])
  chla_yr<- tbl_df(chla_yr)
  vl[[f]] <- chla_yr %>% group_by(season,unit) %>% summarize(sum = sum(sum), nn = sum(nn), ssq = sum(ssq), year = as.numeric(yr))
  print(f)
}

ts.chla <- do.call(rbind, vl)
# sum/nn for mean
# sqrt of ssq for now for variance


cfv <- function(sum, nn, ssq) {
  (ssq - (sum^2)/nn)/(nn-1)
}

ts.chla <- ts.chla %>% mutate(var=cfv(sum=sum, n=nn, ssq=ssq))

#save.image("chla_summary_plots.RData")

layout.mat<- textConnection(
  "1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,5,5,5,5,5,5,1,3,3,3,3,3,7,7,7,7,7,7,3
  1,1,1,1,1,5,5,5,5,5,5,1,3,3,3,3,3,7,7,7,7,7,7,3
  1,1,1,1,1,5,5,5,5,5,5,1,3,3,3,3,3,7,7,7,7,7,7,3
  1,1,1,1,1,5,5,5,5,5,5,1,3,3,3,3,3,7,7,7,7,7,7,3
  1,1,1,1,1,5,5,5,5,5,5,1,3,3,3,3,3,7,7,7,7,7,7,3
  1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,6,6,6,6,6,6,2,4,4,4,4,4,8,8,8,8,8,8,4
  2,2,2,2,2,6,6,6,6,6,6,2,4,4,4,4,4,8,8,8,8,8,8,4
  2,2,2,2,2,6,6,6,6,6,6,2,4,4,4,4,4,8,8,8,8,8,8,4
  2,2,2,2,2,6,6,6,6,6,6,2,4,4,4,4,4,8,8,8,8,8,8,4
  2,2,2,2,2,6,6,6,6,6,6,2,4,4,4,4,4,8,8,8,8,8,8,4
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4
  2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4"
)

m<- as.matrix(read.csv(layout.mat, header=F))

chla_seasonplot<- function(lat.band, the.season, plot.var="Mean ChlA concentration", to.pdf=T,  ymax=1.3e5, ...){
  the.title<- paste0(plot.var, " (", lat.band,", ", the.season ,")")
  x.title<- expression(paste(plot.var, " (", g.m^-3 ,lat.band,", ", the.season ,")"))
  lcols <- c("gray70","gray40", "black")

  if(to.pdf) pdf(file=paste0(the.title,".pdf"), height=3, width=4)
  
  lwdths <- c(6,3,1)
  xlims<- c(-1.7,1.8)
  ylims<- c(0,ymax)
  
  
  layout(m)
  par(mar=c(0,0,0,0), oma=c(2.5, 0.5, 0.55, 0.5), tcl=0.2, cex=1.25, mgp=c(3, 0.25, 0), cex.axis=0.75, col="gray40", col.axis="gray40", fg="gray40")
  
  # pdf panels
  for(i in 1:4){
    the.sector<- sec.cols$sector.s[i]
    sec.dat<- secs.dat[[the.sector]][[lat.band]][[the.season]]
    plot(xlims, ylims, type="n", axes=F)
    bgcol<- sec.cols[sec.cols$sector.s%in%the.sector,]$col
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = paste0(bgcol,40))
    for(j in 1:3){ # plot periods in reverse order as period 1 has less measurements
      p.d<- sec.dat[[j]]
#       cat("log 10 chl\n")
#       with(p.d, print(summary(log(sum/nn,10))))
#       cat("raw chl values\n")
#       with(p.d, print(summary((sum/nn))))
      chl.h <- try(hist(log(p.d$sum/p.d$nn,10),breaks=50, plot=F))
      #
      # MIKEY: in the line above (and teh density plot line below) should it be sum or sum/nn?
      #
      if (!inherits(chl.h, "try-error")){
        multiplier<- (chl.h$counts /chl.h$density)[1]
        if(j==1) multiplier<- multiplier*4 # adjust period 1 for resolution
        chl.d <- density(log(p.d$sum/p.d$nn,10))
        chl.d$y <- chl.d$y * multiplier
      }
      lines(chl.d, col=lcols[j],lwd=5-j)
    }
    box()
    text(xlims[1]*0.75, ylims[2]*0.85, sec.cols$lab[i],cex=0.5)
    
    if(i%%2) axis(3, labels=F) else {axis(1, at=-2:2, labels=c(0.01, 0.1, 1, 10,100))
      # mtext(side=1, the.title,outer =T, line=1.15, cex=1)
      }
    
  }
  
  # time series panels
  for(i in 1:4){
    the.sector<- sec.cols$sector.s[i]
    ts.dat<- filter(ts.chla , grepl(the.sector, unit)&grepl(lat.band, unit)&season==the.season)
    ts.dat$col<- ifelse(ts.dat$year<2002, lcols[1], ifelse(ts.dat$year<2008, lcols[2], lcols[3]))
    
    upper<- with(ts.dat, sum/nn+sqrt(var))
    lower<- with(ts.dat, sum/nn-sqrt(var))
    
    plot(c(1997,2013), c(min(lower), max(upper)), type="n", axes=F)
    
    
    polygon(c(ts.dat$year, rev(ts.dat$year)), c(upper, rev(lower)),border=NA, col=(rgb(0.2,0.2,0.2,0.2)), xpd=NA )
    
    with(ts.dat[ts.dat$year<=2002,], lines(year, sum/nn, col=col), lwd=2)
    with(ts.dat[ts.dat$year>=2002&ts.dat$year<=2008,],lines(year, sum/nn, col=col), lwd=2)
    with(ts.dat[ts.dat$year>=2008,], lines(year, sum/nn, col=col), lwd=2)
    y02<- with(ts.dat[ts.dat$year==2002,], sum/nn)
    y08<- with(ts.dat[ts.dat$year==2008,], sum/nn)
    if((the.sector=="EastPacific" & the.season=="Winter" & lat.band=="Polar")==F) points(c(2002,2008), c(y02,y08), pch="|", cex=0.3)
    lines(x=c(min(ts.dat$year),max(ts.dat$year)), y=rep(mean(ts.dat$sum/ts.dat$nn),2), lwd=0.5, col=rgb(.4,.4,.4,.4))
    
    
    
    first.chl<- ts.dat$sum[1]/ts.dat$nn[1]
    last.chl<- (ts.dat$sum/ts.dat$nn)[nrow(ts.dat)]
    points(c(min(ts.dat$year),max(ts.dat$year)), c(first.chl, last.chl), pch=19, cex=0.3, col=c("grey","black"))
    
    text(labels=as.character(round(first.chl,1)), x=min(ts.dat$year), y=first.chl, pos=2, xpd=NA, cex=0.3, offset=0.2)
    text(labels=as.character(round(last.chl,1)), x=max(ts.dat$year), y=last.chl, pos=4, xpd=NA, cex=0.3, offset=0.2)
  }
  mtext(side=1, bquote( "Mean" ~.(tolower(the.season)) ~ "Chl A concentration (" ~g.m^-3 ~ "," ~.(tolower(lat.band)) ~ ")"), line=1.5, outer=T, cex=0.9)
  if (to.pdf) dev.off()  
}  

chla_seasonplot("Temperate", "Summer", to.pdf=T)
chla_seasonplot("Temperate", "Winter", to.pdf=T)
chla_seasonplot("Polar", "Summer", to.pdf=T)
chla_seasonplot("Polar", "Winter", to.pdf=T)
