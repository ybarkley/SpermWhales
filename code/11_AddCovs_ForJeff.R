

## Oct 23, 2020
## Examining Standard deviation of temp at 584m and EKE at 584m per Jeff P's suggestions.
## Also used to pull out specific rasters for Margaret's suggestion. Rasters and saved and then
## used in PmSDM_Foraging_VarExam.RMD on line 178.


##################################################
####   Import Libraries
##################################################
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(ncdf4)

##################################################
####   Set up directores
##################################################
dir <- here::here()
covar_dir <- paste0(dir, "/data/For Zack-data/")

##################################################
####   Import dataset for use in scaling variables
##################################################
df = readRDS(paste0(covar_dir, "CompletePm_25km_Comb_forZack_v2.rda"))
df = trainHunt
df = testHunt
df = PmFor  #from PmSDM_Foraging
df$Year_Mo <- format(x = df$UTC,
                     format = "%Y_%m")

##################################################
####   temp600: temperature at 600 m
####   Average values from 2010, 2013, 2016, and 2017
####   Shift longitude values from the 0-360 scale to -180-180 scale by
####          shifting the x-values -360 degrees
####   Match the resolution of the mean_SST scale by resampling cells (thereby
####          also cropping to main_extent)
####   Convert from kelvin to Celcius
####   Center variable
##################################################
for(iyear in c(2010, 2013, 2016, 2017)){
  #Imported stack
  temp_raster <- raster::stack(paste0(covar_dir, iyear, "-temp584.nc"))
  
  #Changed names
  names(temp_raster) <- paste0(substr(x = names(temp_raster),
                                      start = 2, stop = 5),
                               "_",
                               substr(x = names(temp_raster),
                                      start = 7, stop = 8))
  
  #assigned
  assign(x = paste0("temp600_", iyear),
         value = temp_raster)
  
}

saveRDS(object = temp600_2010,
        file = paste0(covar_dir, "2010-temp584m.RDS"))

for (i in unique(df$Year_Mo)) {
  #Make a spatial object subsetting 
  temp_sp <- sp::SpatialPoints(coords = df[df$Year_Mo == i,
                                           c("Longitude", "Latitude")])
  
  #Extracts the raster from a given year and month
  temp_ras <- get(paste0("temp600_",
                         substr(i, start = 1, stop = 4)))[[paste0("X", i)]]
  temp_ras <- temp_ras - 273.15
  
  #Focal stat on the temp_ras
  temp_ras_sd <- raster::focal(x = temp_ras,
                               w = matrix(1/9, 3, 3), #(1/25, 5, 5), #(1/9, 3, 3)
                               fun = sd,
                               na.rm = T)
  
  plot(temp_ras_sd)
  
  #Extract vals
  df$temp600_sd.r[df$Year_Mo == i] <- raster::extract(x = temp_ras_sd,
                                                      y = temp_sp)
}
# save some select data for McManus stuff
saveRDS(object = temp600_2010,
        file = paste0(covar_dir, "2010-temp584m.RDS")) # this is all months in 2010
saveRDS(object = temp600_2017,
        file = paste0(covar_dir, "2017-temp584m.RDS")) # this is all months in 2017
names(temp_ras_sd) <- "2010_08"
saveRDS(object = temp_ras_sd,
        file = paste0(covar_dir, "201008-temp584msd.RDS"))  #specifically 2010_08 raster
names(temp_ras_sd) <- "2017_07"
saveRDS(object = temp_ras_sd,
        file = paste0(covar_dir, "201707-temp584msd.RDS")) #specifically 2017_07 raster

# look at the data
par(mfrow = c(2,1))

plot(df1$temp600_sd.r, df1$pa)
plot(df2$temp600_sd.r, df1$pa)


##################################################
####   U and V for EKE at depth  ####
####   From YB 
####   Uses 'stack' to handle all bands
##################################################


#### u raster ####
for(iyear in c(2010, 2013, 2016, 2017)){
  #Imported stack
  u_raster <- raster::stack(paste0(covar_dir, iyear, "-u.nc"))
  
  #Changed names
  names(u_raster) <- paste0(substr(x = names(u_raster),
                                      start = 2, stop = 5),
                               "_",
                               substr(x = names(u_raster),
                                      start = 7, stop = 8))
  
  #assigned
  assign(x = paste0("u_", iyear),
         value = u_raster)
  
}


for (i in unique(df$Year_Mo)) {
  #Make a spatial object subsetting a;slfj; s
  u_sp <- sp::SpatialPoints(coords = df[df$Year_Mo == i,
                                           c("Longitude", "Latitude")])
  
  #Extracts the raster from a given year and month
  u_ras <- get(paste0("u_",
                         substr(i, start = 1, stop = 4)))[[paste0("X", i)]]

  #Focal stat on the temp_ras
  u_ras_sd <- raster::focal(x = u_ras,
                               w = matrix(1/9, 3, 3), #(1/25, 5, 5), #(1/9, 3, 3)
                               fun = sd,
                               na.rm = T)
  
  plot(u_ras_sd)
  
  #Extract vals
  df$u.r[df$Year_Mo == i] <- raster::extract(x = u_ras_sd,
                                             y = u_sp)
}


#### v raster ####

for(iyear in c(2010, 2013, 2016, 2017)){
  #Imported stack
  v_raster <- raster::stack(paste0(covar_dir, iyear, "-v.nc"))
  
  #Changed names
  names(v_raster) <- paste0(substr(x = names(v_raster),
                                   start = 2, stop = 5),
                            "_",
                            substr(x = names(v_raster),
                                   start = 7, stop = 8))
  
  #assigned
  assign(x = paste0("v_", iyear),
         value = v_raster)
  
}


for (i in unique(df$Year_Mo)) {
  #Make a spatial object subsetting a;slfj; s
  v_sp <- sp::SpatialPoints(coords = df[df$Year_Mo == i,
                                        c("Longitude", "Latitude")])
  
  #Extracts the raster from a given year and month
  v_ras <- get(paste0("v_",
                      substr(i, start = 1, stop = 4)))[[paste0("X", i)]]
  
  #Focal stat on the temp_ras
  v_ras_sd <- raster::focal(x = v_ras,
                            w = matrix(1/9, 3, 3), #(1/25, 5, 5), #(1/9, 3, 3)
                            fun = sd,
                            na.rm = T)
    plot(v_ras_sd)
  
  #Extract vals
  df$v.r[df$Year_Mo == i] <- raster::extract(x = v_ras_sd,
                                             y = v_sp)
}

df$eke600m.r <- 0.5*(df$u.r^2 + df$v.r^2)
plot(df$eke600m.r, df$pa)

PmFor = df
trainHunt2 = df
# saveRDS(trainHunt2, here::here( paste0('output/models/',loctype, '/data/Train_',gridsize, 'km_', loctype2,'_HuntNEW.rda') ))
testHunt = df


### SST Standard Deviation ####

#Merge sliver with rest of study area here
quick_merge <- function(iyear = 2010,
                        var = "SST"){
  main_raster <- raster::stack(paste0(covar_dir, 
                                      iyear, "-", var, ".nc"))
  sliver <- raster::stack(paste0(covar_dir, 
                                 iyear, "-", var, "Sliver.nc"))
  sliver <- raster::shift(x = sliver, 
                          dx = -360) 
  origin(sliver) <- origin(main_raster)
  
  return(merge(main_raster, sliver))
}
##################################################
####   SST: sea surface temperature  ####
####   Average values from 2010, 2013, 2016, and 2017
####   Crop to main_extent
####   Center variable
##################################################
for(iyear in c(2010, 2013, 2016, 2017)){
  assign(x = paste0("SST_", iyear),
         value = quick_merge(iyear = iyear,
                             var = "SST"))
  
}

# This is janky. Did it each raster at a time for SST to rename the different bands based on temp584 names
# Would probably have had to do this altogether in the quick_merge function otherwise. This works, just change the iras and iyear variables to match the year of the data set
for(iras in c( SST_2016)){
  iyear = 2016
  #Imported stack
  temp_raster <- raster::stack(paste0(covar_dir, iyear, "-temp584.nc"))
  
  #Changed names
  names(iras) <- paste0(substr(x = names(temp_raster),
                                      start = 2, stop = 5),
                               "_",
                               substr(x = names(temp_raster),
                                      start = 7, stop = 8))
  
  #assigned
  assign(x = paste0("SST_", iyear),
         value = iras)
  
}


for (i in unique(df$Year_Mo)) {
  #Make a spatial object subsetting a;slfj; s
  sst_sp <- sp::SpatialPoints(coords = df[df$Year_Mo == i,
                                        c("Longitude", "Latitude")])
  
  #Extracts the raster from a given year and month
  sst_ras <- get(paste0("SST_",
                      substr(i, start = 1, stop = 4)))[[paste0("X", i)]]
  
  #Focal stat on the temp_ras
  sst_ras_sd <- raster::focal(x = sst_ras,
                            w = matrix(1, 3, 3), #(1/25, 5, 5), #(1/9, 3, 3)
                            fun = sd,
                            na.rm = T)
                            
  sst_ras_sd <- raster::shift(x = sst_ras_sd, dx = +360)
                              
  # plot(sst_ras_sd)
  
  #Extract vals
  df$SSTsd.r[df$Year_Mo == i] <- raster::extract(x = sst_ras_sd,
                                             y = sst_sp)
}
#for train data to test model in PmSDM_Foraging_VarExam
trainHunt2 = df
PmFor = df
saveRDS(PmFor, here::here('data/Pm_ForagingOnly2.rda'))
