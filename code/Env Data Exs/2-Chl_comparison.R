### LOAD LIBRARIES

require(lubridate)
require(plyr)
library(rerddap)
library(ncdf4)

#First we define the longitude-latitude boundaries of the box: 
xcoord<-c(198,208)
ycoord<-c(15,25)

#next we define the URL of the ERDDAP we will be using
ERDDAP_Node="https://oceanwatch.pifsc.noaa.gov/erddap/"




# Get monthly seawifs dataset, which starts in 1997. 
# Go to ERDDAP to find the name of the dataset for monthly SeaWIFS -> 'OceanWatch_seawifs_chla_monthly'

# first we need to know what our variable is called 
dataInfo <- rerddap::info('OceanWatch_seawifs_chla_monthly', url=ERDDAP_Node)
var=dataInfo$variable$variable_name

# 'griddap' is a function from the 'rerddap' package. 
# It grabs the data from ERDDAP based on the parameters we give it
# We are grabbing a lot of data so all the griddap comands might take a while.

sw <- griddap(url=ERDDAP_Node,
  'OceanWatch_seawifs_chla_monthly', time = c('1997-12-01', '2010-12-01'),
  latitude = ycoord, longitude = xcoord,
  fields = var
)

# Spatially average all the data within the box 
swAVG <- ddply( sw$data, .(time), function(x) mean(x$chl, na.rm =TRUE) )


# Get monthly MODIS dataset, which starts in 2002.  
#
dataInfo <- rerddap::info('OceanWatch_aqua_chla_monthly', url=ERDDAP_Node)
var=dataInfo$variable$variable_name

MOD <- griddap(url=ERDDAP_Node,
  'OceanWatch_aqua_chla_monthly', time = c('2002-08-01', '2018-07-01'),
  latitude = ycoord, longitude = xcoord,
  fields = var
)

# Spatially average all the data within the box 
MODAVG <- ddply( MOD$data, .(time), function(x) mean(x$chla, na.rm =TRUE) )


# Get monthly VIIRS dataset, which starts in 2012.  
#
dataInfo <- rerddap::info('OceanWatch_nasa-viirs_chla_monthly', url=ERDDAP_Node)
var=dataInfo$variable$variable_name

VIIRS <- griddap(url=ERDDAP_Node,
               'OceanWatch_nasa-viirs_chla_monthly', time = c('2012-02-01', '2018-07-01'),
               latitude = ycoord, longitude = xcoord,
               fields = var
)
# Spatially average all the data within the box 
VIIRSAVG <- ddply( VIIRS$data, .(time), function(x) mean(x$chla, na.rm =TRUE) )



# Get CCI data (September 1997 to June 2018)
# Unfortunately, some datasets are linked from servers that are slower 
# or have the smaller limit on how much can be downloaded.
# In that case, ERDDAP times out and it's easier to download the data straight from their server:
# For the CCI data, go to https://rsg.pml.ac.uk/thredds/ncss/grid/CCI_ALL-v3.1-MONTHLY/dataset.html
# and download the data corresponding to our region, for the whole date range. 
# Save it into your working directory.
# We won't be able to use the griddap function, so we'll use the ncdf package to work with this file.

nc=nc_open('CCI_ALL-v3.1-MONTHLY.nc')
v1=nc$var[[1]]
chl=ncvar_get(nc,v1)
dates=as.POSIXlt(v1$dim[[3]]$vals*3600*24,origin='1970-01-01',tz='GMT')

# Spatially average all the data within the box. 
cciAVG=apply(chl,3,mean,na.rm=TRUE)

#****************************
# Plot time series result
#****************************


plot(as.Date(swAVG$time), swAVG$V1, 
     type='l', col=2,lwd=2, xlab="",
     xlim=as.Date(c("1997-12-01","2018-12-01")),
     ylim=c(0.04,0.12),
     ylab="CHL")
axis(2)
points(as.Date(swAVG$time), swAVG$V1,pch=20,col=2)

lines(as.Date(MODAVG$time), MODAVG$V1, col=4, lwd=2)
points(as.Date(MODAVG$time), MODAVG$V1,pch=20,col=4)

lines(as.Date(VIIRSAVG$time), VIIRSAVG$V1, col=3, lwd=2)
points(as.Date(VIIRSAVG$time), VIIRSAVG$V1,pch=20,col=3)

legend('topleft',legend=c('sw','mod','viirs'),lty=1,col=c(2,4,3),lwd=2)


# Make another plot with CCI as well to compare
plot(as.Date(swAVG$time), swAVG$V1, 
     type='l', col='lightgray',lwd=1, xlab="",
     xlim=as.Date(c("1997-12-01","2018-12-01")),
     ylim=c(0.04,0.12),
     ylab="CHL")
axis(2)
points(as.Date(swAVG$time), swAVG$V1,pch=20,col=2)

lines(as.Date(MODAVG$time), MODAVG$V1, col='lightgray', lwd=1)
points(as.Date(MODAVG$time), MODAVG$V1,pch=20,col=4)

lines(as.Date(VIIRSAVG$time), VIIRSAVG$V1, col='lightgray', lwd=1)
points(as.Date(VIIRSAVG$time), VIIRSAVG$V1,pch=20,col=3)

lines(as.Date(dates),cciAVG,col=1,lwd=1)

legend('topleft',legend=c('sw','mod','viirs','cci'),pch=c(20,20,20,NA),lty=c(NA,NA,NA,1),col=c(2,4,3,1))
