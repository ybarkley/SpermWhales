#March 4, 2020: Prepared but not Sent to EF for help
dir = 'C:/Users/yvers/Documents\\CHP 3/SpermWhales/data'
chla = read.csv(paste0(dir, '/Spermies_CHLA.csv'))
#find NA in chl data
chla_na <- which(is.na(chla), arr.ind = TRUE)
chla[chla_na[1,]]
#select the first row for this example
chla_na <- chla_na[1,]


#load nc file for sperm whale encounter
file_nc = 'aqua_chla_monthly_1604_30.nc'  # OR 'aqua_chla_monthly_1641_60.nc'
ncname = paste0(dir, '/', file_nc) #1604_30

# make raster from nc file
c1 <- raster(ncname, level=1, varname="chlor_a")



#apply focal to raster
cfoc <- focal(c1, w=matrix(1, nc=3, nr=3), fun=mean, rm.na=TRUE) 
getValues(cfoc)  # check out the data -> lots of NA...

#get lat/lon from focal output
xy <- xyFromCell(cfoc, 1:ncell(cfoc)) # MATCH lat/lon from chl_mon2 with lat/lons from focal

#extract chl value that matches the location of the NA from 'chla'

dat = c()
for (i in chla_na[,1]) {
  
#find matching lat.lon from chla_na in chla
matched_na <- chla[i,c(1,4,3)]

dat <- rbind(dat, matched_na)

}



matched_loc <- filter(xy, xy[,1] == matched_na[1,1] | xy$y == round(matched_na[1,2],3))


chlNA_1604a30 <-raster::extract(cfoc, row of xy to extract)