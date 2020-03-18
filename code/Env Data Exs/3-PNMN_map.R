# PMNM_map.R 
#
# Use the Xtractomatic package to download data from within the boundaries of the 
# Papahanaumokuakea Marine National Monument  (PMNM)
#
# This script uses the rerddap version of Xtractomatic:  rerddapXtracto
#
# Note!! if you already have rerddap installed on your computer, you will need to uninstall it 
# (remove.packages('rerddap')) and reinstall it from Github to get the most recent version
# -> see command line below.
#
# This script displays sst values for a particular time period or average sst over the whole period 
# within the PMNM polygon boundary.  

# PMNM boundaries were obtained at http://sanctuaries.noaa.gov/library/imast_gis.html
# The boundary KML file was converted into a CSV file by manually stripping out
# extraneous text in a text editor (and shifting chunks around to get a continuous boundary)
# ask Melanie if you want to try and do this for a different region. It took a bit of tweaking.

### INSTALL PACKAGES
devtools::install_github("ropensci/rerddap")
devtools::install_github("ropensci/plotdap")
devtools::install_github("rmendels/rerddapXtracto")

# and install any packages for the below required libraries that are not
# yet installed
###   FOR EXAMPLE: install.packages("maps")


### LOAD LIBRARIES

library(maps)
library(mapdata)
library(maptools)
library(rerddap)
library(rerddapXtracto)


poly360=read.csv('poly360.csv',header=FALSE)  # polygon for PMNM
names(poly360) <- c("Longitude","Latitude")

poly=poly360
# View the object 'poly' using str to see its structure


# The example below extracts monthly 5km geopolar blended SST data within the monument boundary.
# The time variable passed to xtractogon must be two elements, the start and endpoints of the 
# desired time period


ERDDAP_Node="https://oceanwatch.pifsc.noaa.gov/erddap/"
dataInfo <- rerddap::info('OceanWatch_goes-poes_sst_monthly', url=ERDDAP_Node) #  Global 4km monthly Pathfinder SST data 

parameter=dataInfo$variable$variable_name 

xcoord <- poly$Longitude
ycoord <- poly$Latitude

# Let's download data for a few months.
tcoord <- c("2015-03-15", "2015-11-16")

# Let's extract the data within our polygon.
# Note! This function will not work for regions across the dateline (like PNMN...) and 
# datasets with longitudes between -180 and 180º
# For regions that cross the dateline, you need to find a dataset on ERDDAP 
# with longitudes between 0 and 360º
# this function can take a while to run
sst <- rxtractogon (dataInfo, parameter=parameter, xcoord=xcoord, ycoord=ycoord,
                           tcoord=tcoord, urlbase="https://oceanwatch.pifsc.noaa.gov/erddap")



# The extract contains several time steps (months) of sst data  
# in the monument boundaries. Let's make a plot of the second time step for example:

i=2  

# we need to transform our SST data for that time step into a matrix for the "image" function to work (l.123)
test=as.matrix(sst$sst[,,i])



# Now plot the data 

# let's set the bounds of our region
xlim = c(177,207)
ylim = c(18,32)

# let's define a custom color scale
jet.colors <-colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
h=hist(test, 50, plot=FALSE)
breaks=h$breaks
n=length(breaks)-1
c=jet.colors(n)

# let's open a new graphics window that will be big enough
x11(width=7.6,height=4.9)

# the layout function splits the graphics window. 
# One part for the map, the other for the color scale.
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)

# The map:
par(mar=c(3,3,3,1))
land <- maps::map("world2", fill = TRUE, xlim = xlim, ylim = ylim, plot = FALSE)

ids <- sapply(strsplit(land$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(land, IDs = ids, proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(bPols, col = "grey", axes = FALSE, xlim = xlim, ylim = ylim,
     xaxs='i',yaxs='i',asp=1, cex.axis = 3, main=tcoord[i])
lines(poly360$Longitude,poly360$Latitude,lwd=2)
#let's make pretty axes
x = seq(175, 205, 5)
axis(1, x, sapply(paste(-x + 360, "ºW", sep = ""), function(x) x[1]))
y = seq(10, 45, 5)
axis(2, las = 1, y, sapply(paste(y, "ºN", sep = ""), function(x) x[1]))
box()

# We'll add the color plot on top of the map
par(new=TRUE)

image(sst$longitude,sst$latitude,test,col=c,breaks=breaks,xlab='',ylab='',
      axes=FALSE, xlim = xlim, ylim = ylim,xaxs='i',yaxs='i',asp=1, las=1)


# The color scale
par(mar=c(3,1,3,3))
#plot color scale using 'image.scale' function from 'scale.R' script)
source('scale.R')
image.scale(test, col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4,las=1)
box()

# ON YOUR OWN!
# Plot the average SST for the period we downloaded:
# hint: here is how to compute the average SST
mean_sst=as.matrix(apply(sst$sst,c(1,2),mean,na.rm=TRUE))

# Now use mean_sst to plot instead of test. 
# (Hint. you need to edit l. 123, and the plot title in the 'main' function on l. 111 to edit the title)



