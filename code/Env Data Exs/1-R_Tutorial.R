# This is the script corresponding to the R-Tutorial, with all the commands in one place
# to make it easier to adapt for your own project.

library(ncdf4)
library(httr)

junk <- GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/OceanWatch_aqua_chla_monthly.nc?chla[(2015-05-01T00:00:00Z):1:(2016-05-01T00:00:00Z)][(17):1:(30)][(195):1:(210)]', 
            write_disk("chl.nc"))

nc=nc_open('chl.nc')

#examine which variables are included in the dataset
names(nc$var)

#extract chla 
v1=nc$var[[1]]
chl=ncvar_get(nc,v1)

#examine structure of chl
dim(chl)


dates=as.POSIXlt(v1$dim[[3]]$vals,origin='1970-01-01',tz='GMT')
dates

lon=v1$dim[[1]]$vals

lat=v1$dim[[2]]$vals


nc_close(nc)
rm(junk,v1)
file.remove('chl.nc')

#set color breaks 
h=hist(chl[,,1], 100, plot=FALSE)
breaks=h$breaks
n=length(breaks)-1

#define a color palette
jet.colors <-colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#set color scale using the jet.colors palette
c=jet.colors(n)

x11(width=6, height=4.85)
#prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)

par(mar=c(3,3,3,1))
#plot Chl map
image(lon,lat,chl[,,1],col=c,breaks=breaks,xlab='',ylab='',axes=TRUE,xaxs='i',yaxs='i',asp=1,
      main=paste("Monthly Chlorophyll a concentration", dates[1]))
#example of how to add points to the map 
points(202:205,rep(26,4), pch=20, cex=2)
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)
par(new=TRUE)
contour(lon,lat,chl[,,1],levels=0.09,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)

par(mar=c(3,1,3,3))
#plot color scale using 'image.scale' function from 'scale.R' script)
source('scale.R')
image.scale(chl[,,1], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='Chl a')
axis(4, las=1)
box()


# Time-series
I1=which(lon==200)
I2=which(lon==206)
J1=which(lat==24)
J2=which(lat==26)
chl2=chl[I1:I2,J1:J2,]

n=dim(chl2)[3]

res=rep(NA,n)
for (i in 1:n)
  res[i]=median(chl2[,,i],na.rm=TRUE)

plot(1:n,res,axes=FALSE,type='o',pch=20,xlab='',ylab='Chl a')
axis(2)
axis(1,1:n,dates)
box()


#median chl map

chl.yr=apply(chl[,,1:12],c(1,2),median,na.rm=TRUE)


x11(width=7.55, height=6)
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)

par(mar=c(3,3,3,1))
image(lon,lat,chl.yr,col=c,breaks=breaks,xlab='',ylab='',axes=TRUE,xaxs='i',yaxs='i',asp=1,
      main=paste("Median Chlorophyll a concentration", dates[1],' - ',dates[12]))

par(mar=c(3,1,3,3))
image.scale(chl.yr, col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='Chl a')
axis(4, las=1)
box()
