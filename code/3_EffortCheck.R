
#Sept 22, 2020
################################################################
## GOOD FOR LOADING ALL CENTROIDS TO COMBINE INTO ONE DATAFRAME (originally made for AcOnly, but scroll down for combined models)
################################################################
# Checking the centroids for the Acoustics Only models to get the centroids out of the existing environmental dataset.

#Load Centroid data for new Acoustics Only models that include when acoustics is on and disregards the visual effort.
s = 1303
gridsize = 25
loctype = 'allsw'
loctype2 = 'all'
#load grid data
gridEffort <- readRDS(here::here( paste0('output/gridEffort/AcOnly/', gridsize, ' km-', loctype, 
                                         '/gridEffort', s, '_', gridsize, 'km_', loctype2, '_AC.rda') ))
View(gridEffort$detections)
#load total dataset with env data for all effort
AllEffCents <- readRDS(here::here(paste0('output/envData/complete env data/CompletePm_25km_all_raw2.rda')))

#load all Acoustics Only centroids
allsurveys = c(1641, 1303, 1604, 1705, 1706)



### FOR ACOUSTICS ONLY MODELS
AcOnlyCents = NULL
AcOnlyEnv = NULL
pa1Check = NULL
for (s in c(1641, 1303, 1604, 1705, 1706)){
  aconlyi <- readRDS(here::here( paste0('output/centroids/AcOnly/', gridsize, ' km-', loctype, '/',  #new AcOnly centroids
                                       'centroids', s, '_', gridsize, 'km_', loctype2, '_AC.rda') ))
  aconlyi$survey = s
  # DATA checking
  # aconlypa1 <- nrow(filter(aconlyi, pa>0))
  # aconlypa1 <- filter(aconlyi, pa>0)
  # ##
  #1604 needs effCell 740 pa ==2 instead of ==1
  # aconlyi2 <- aconlyi[-which(aconlyi$effCells==740),]
  # aconly1604 = filter(aconlyi, effCells == 740)
  # aconly1604$pa[aconly1604$pa == 1] <- 2
  # aconlyi = rbind(aconlyi2, aconly1604)
  # saveRDS(aconlyi, here::here( paste0('output/centroids/AcOnly/', gridsize, ' km-', loctype, '/',  #new AcOnly centroids
  #                            'centroids', s, '_', gridsize, 'km_', loctype2, '_AC.rda') ))
  # aconlyi2 <- aconlyi[-which(aconlyi$effCells==740),] #effdets is the index from grid detections
  
  alleffsub <- filter(AllEffCents, survey == s)  #filter total env dataset for survey
  alleffpa1 <- nrow(filter(alleffsub, pa >0))
  
  aconlyEnv <- filter(alleffsub, effCells %in% aconlyi$effCells)

    pa1diff <- data.frame(survey=s, TotalPa1 = alleffpa1, AcOnlyPa1 = aconlypa1) #make dataframe showing presences
  
  AcOnlyCents = rbind(AcOnlyCents, aconlyi)  
  AcOnlyEnv = rbind(AcOnlyEnv, aconlyEnv)
  pa1Check = rbind(pa1Check, pa1diff)
  
}



# nrow(AllEffCents)-nrow(AcOnlyEnv)


saveRDS(AcOnlyCents, here::here( paste0('output/centroids/AcOnly/', gridsize, ' km-', loctype, '/',  #new AcOnly centroids
                                                                 'centroids_AllSurveys', '_', gridsize, 'km_', loctype2, '_AConly.rda') ))




###########################
### Combined Models

CombCents = NULL
# AcOnlyEnv = NULL
# pa1Check = NULL
for (s in c(1641, 1303, 1604, 1705, 1706)){
  combi <- readRDS(here::here( paste0('output/centroids/', gridsize, ' km-', loctype, '/',  #new AcOnly centroids
                                        'centroids', s, '_', gridsize, 'km_', loctype2, '.rda') ))

  CombCents = rbind(CombCents, combi)  
  
}
saveRDS(CombCents, here::here( paste0('output/centroids/', gridsize, ' km-', loctype, '/',  #new Combined centroids
                                        'centroids_AllSurveys', '_', gridsize, 'km_', loctype2, '.rda') ))





#Old code and plot options
#verify new AcOnly centroids
s = 1303
ctest <- filter(AcOnlyCents,  survey == s & pa >0)
newcents <- filter(AcOnlyEnv,  survey == s)
test2 <- filter(AllEffCents,  survey == s)
oldpas <- filter(AllEffCents,  survey == s & pa >0)



#cross-checking the cells included in the Acoustics Only dataset with the cells in the Total dataset with acoustics OR visuals on
#subset total data by survey
s = 1604
test <- filter(AcOnlyEnv,  survey == s & pa >0)
newcents <- filter(AcOnlyEnv,  survey == s)
test2 <- filter(AllEffCents,  survey == s)
oldpas <- filter(AllEffCents,  survey == s & pa >0)

#load grid data
gridEffort <- readRDS(here::here( paste0('output/gridEffort/', gridsize, ' km-', loctype, 
                                         '/gridEffort', s, '_', gridsize, 'km_', loctype2, '.rda') ))
View(gridEffort$detections)

x <- gridEffort
actualArea <- as.numeric(st_area(x$grid))
coveragePct <- round(x$effort$area / actualArea, 3)
coveragePct <- ifelse(coveragePct > 1, 1, coveragePct)

pdf(here::here('figures', paste0('Grid_AcOnly', s, '_', gridsize, 'km_', loctype, '_CHECK.pdf')), width=11,height=8)

plot(x$grid, col = gray(1 - coveragePct, alpha = .9), lwd = 0.2)
points(x=x$gps$Longitude, y=x$gps$Latitude, col='blue', pch=16, cex = 0.1)
points(test2$Longitude, test2$Latitude, col='red', pch = 19, cex=0.1, lwd=0.1) #from the old centroids with env data
points(newcents$Longitude, newcents$Latitude, col='green', pch = 0, cex=0.3, lwd=0.1)   #from the new AcOnly centroids with env data

points(x=test$Longitude, y=test$Latitude, col='deepskyblue', pch=1, cex=0.3, lwd=0.3) #pa1 for AcOnly
text(test$Longitude, test$Latitude,col='magenta', labels=test$acid, cex=0.3, lwd = 0.3 )

points(x=oldpas$Longitude, y=oldpas$Latitude, col='deepskyblue', pch=2, cex=0.8, lwd=0.4) #pa1 for AcOnly
text(oldpas$Longitude, oldpas$Latitude,col='magenta', labels=test$acid, cex=0.3, lwd = 0.3 )

dev.off()


