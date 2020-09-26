# Sept 18, 2020 Yvonne Barkley
# Compare sighting dataframe from AB with sperm whale dataframe from YB
# Want to check that I'm including all of the sightings from the surveys 

library(swfscDAS)
library(tidyverse)


#load sperm whale dataframes
# swYB <- read.csv(paste0('data/FromYB_spermies.csv'))  #SpermWhales for Eva data, not filtered for excluded spermies, don't use for this.
swYB <- readRDS(here::here( paste0('output/envData/complete env data/', 'CompletePm_', gridsize, 'km_', loctype2, '_scaled.rda') ))  #same as PmScaled
colnames(swYB)[5] <- 'CruzNo'
swAB <- read.csv(paste0('data/FromAB_spermies.csv'))
                 


#filter by survey number
cruzYB = unique(swYB$CruzNo)
cruzYB = 1642
# cruzAB = unique(swAB$Survey.Number)

addSID <- NULL
addSID2 <- NULL
addNEW <- NULL

for ( cz in cruzYB ) {
  
 swABsub <- filter(swAB, CruzNo == cz) #filter each by survey/cruise
 swYBsub <- filter(swYB, CruzNo == cz)
  swYBsub2 <- filter(swYBsub, sid < 999)
# unique(swYBsub2$Sighting.ID) 
notINyb <-  filter(swABsub, !(SightNo %in% swYBsub2$sid ))   #what sightings are included in swAB but not in my dataset, swYB?
notINab <-  filter(swYBsub2, !(sid %in% swABsub$SightNo ))   #what sightings are not included in swAB? Hopefully none!
  
das <- das_read( paste0('data/', cz, '.das') )
das2 <- das_process(das)
stg <- das_sight((das2))  #all sightings from das file

swdas <- stg %>% filter(SpCode == '046') %>% select(DateTime, Lat, Lon, SightNo, DistNm, SpCode) #all spermies from das
swdas$SightNo <- as.numeric(swdas$SightNo)

sw1641b <- filter(swdas, SightNo %in% notINyb$SightNo )  #get the info for sightings not in yb dataset

abNEW <- data.frame(select(sw1641b, DateTime, Lon, Lat), notINyb)

addSID <- rbind(addSID, notINyb)  #all sightings missing from current modeling dataset
addSID2 <- rbind(addSID2, notINab)

addNEW <- rbind(addNEW, abNEW)

}

addNEW$acid <- 999

addNEW <- unite(addNEW, "ID", CruzNo, acid, SightNo, sep='_', remove=FALSE) #makes ID column

saveRDS( addNEW, paste0('data/SpermiesSightingsMISSED-1642.rda'))
write.csv( addNEW, paste0('data/SpermiesSightingsMISSED-1642.csv') )
