##################################################
## Project: Help Yvonne Diss
## Date: 7/17/2020 Yvonne edited to subset
## Author: Brett Cooper + Yvonne Barkley
##################################################


# Load packages
library(sf)
library(sp)
library(geosphere)
library(maptools)
# Set local dir
# setwd("C:\\Users\\Brett.Cooper\\Downloads\\seamounts\\seamounts")
setwd("C:\\Users\\yvers\\Documents\\CHP 3\\SpermWhales\\data\\seamounts")


# read in the seamounts
seamounts <- st_read("Seamounts.shp")

# convert to spatial polygon df
seamounts_spatial = as_Spatial(seamounts)

# subset the locations of seamounts and end up with a vector of the indices 
# for the seamounts that fall within the Hawaiian EEZ
dump=NULL
for (k in 1:length(seamounts_spatial)){ #length(seamounts_spatial)
  labpt <- seamounts_spatial@polygons[[k]]@labpt
  if (labpt[2] >= 15 & labpt[2] <= 32 & labpt[1] <= -151 & labpt[1] >= -179){#
    smt_ind = k
  } else if (labpt[2] >= 15 & labpt[2] <= 32 & labpt[1] <179 & labpt[1] >= 170){
    smt_ind = k
  } else {
    smt_ind = NA
  }
  
  dump = rbind(dump, smt_ind)
  
}

dump2 <- dump[!is.na(dump)]

subset_of_seamounts <- seamounts_spatial[dump2,]
plot(subset_of_seamounts)
points(178,25, col='red', pch = 16)
points(-160,25, col='blue', pch = 16)

saveRDS(subset_of_seamounts, file = paste0(here::here('data'), '/', 'seamounts_tot.rda'))
# let's get a small subset of seamounts, cause running this with 10k seamounts
# is slooow
# subset_of_seamounts <-  head(seamounts_spatial, 1000)
# import noisy whale places
# Probably start with a small set of points too, and make sure that this
# seems to work
fake_noisy_whale_places <- rbind(c(-150, 22), c(-160, 25), c(-179, 30))


# get the nearest seamount and the distance in meters
# dist.mat <- geosphere::dist2Line(p    = fake_noisy_whale_places, 
#                                  line = subset_of_seamounts)
# 
# 
# saveRDS(dist.mat, file = paste0(here::here('data'), '/', 'dist2seamount_tot.rda'))


########## RUN SEAMOUNT DISTANCES ######
fpath = 
seamounts_HI <- readRDS(here::here('data', 'seamounts_tot.rda')) #load Hawaii seamount data
sw <- readRDS(here::here('output', 'centroids1706_10km.rda')) #load centroid locations from gridding data

lon = sw$lon
lat = sw$lat
dates = sw$UTC

#change lon column to -180-180 to match seamount data
sw$lon2 = ifelse(sw$lon >180, sw$lon - 360, sw$lon) 

sw_smts <- cbind(sw$lon2, sw$lat, sw$effCells)

#################################
## Initiate Cluster
#################################
require(parallel)
require(doParallel)

cl = makeCluster(detectCores())
registerDoParallel(cl, cores = detectCores())


#######################################
##The foreach function is the loop function for parallel processing. 
##It uses %dopar% which partitions the loop among your cores, then combines each output using the .combine argument. 
system.time(

smt_test = foreach(i = 1:nrow(sw_smts), .combine = rbind) %dopar% { 
  
  smt_temp = data.frame(geosphere::dist2Line(p = sw_smts[i,1:2], line = seamounts_HI))
  # smt_temp$effCells[i] = sw$effCells[i]
}

)
#so, for each of the parameter combinations, i.e., rows of the tune_settings df, the RF is run using those 
#settings (line 46) and the error rates for the 3 pop's and the oob are printed (line 47). 
#when all the rows are finished, the results are combined using the rbind function into one matrix, stored as x.

#NB: .combine can be anything, really, for example .combine = c for a vector, .combine = list for a list, etc.

#################################
## Stop Cluster
#################################
stopCluster(cl)

########################################
## Combine settings and results
########################################
dist2smt = data.frame(smt_test, `effCells`= sw_smts[,3]) #add effCells
