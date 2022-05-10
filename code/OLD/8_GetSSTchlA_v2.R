## Trying out Taiki's Environmental Data Extraction Code
# SST, Chl a, Temp @ 600 m, Wave Power

library(PAMmisc)
library(here)

#### Which Survey? ####
surveynum = 1706
gridsize = 10
loctype = 'localized'
loctype2 = 'loc'

# centroidData <- sw
centroidData <- readRDS(here::here(paste0('output/envData/', gridsize, ' km-', loctype, '/', 
                                           surveynum, '_', gridsize, 'km_', loctype2, '_sw.rda' )))


colnames(centroidData)[5] <- "Longitude"
colnames(centroidData)[6] <- "Latitude"
# [c(1,2)] <- c("good", "better")


#### START HERE ####
envType = 'sst'
envType = 'chla' 

envType = 'wp'

#for SST
envDataset = 'erdMH1sstdmday'
http = 'https://coastwatch.pfeg.noaa.gov/erddap/'

#for CHL
envDataset = 'hawaii_soest_69ee_7d0d_74e6'
http = 'http://apdrc.soest.hawaii.edu/erddap/'

#for Temp at 600m
envDataset = 'hawaii_soest_d346_28ac_fccf'  #potdsl
http = 'http://apdrc.soest.hawaii.edu/erddap/'

#for WaveWatch3 (wave power)
envDataset = 'hawaii_soest_98bb_253a_eb1c'
http = 'http://apdrc.soest.hawaii.edu/erddap/'


#1 of 4. Get dataset info
# edinfo <-browseEdinfo(var='sst') #use if the dataset is in the package
edinfo <- erddapToEdinfo(envDataset, baseurl = http)

#2 of 4. Run centroids with no buffer, getting data from single location of the centroid and saving it
matchedData <- matchEnvData(centroidData, nc=edinfo,
                            fileName = here::here('output/envData', paste0(gridsize, ' km-', loctype, '/',
                                                                           surveynum, "_", gridsize, 'km_',
                                                                           loctype2, '_', envType, "700.nc")))

#3 of 4. Deal with any NAs in the dataset
matchedData[matchedData[,9]=='NA'] <- NA                  #put in NaNs to variable column

#### MEGA IFELSE ####
# Series of ifelse statements to check for NAs and fill them using the 'buffer' argument with 1km steps to get data
if (nrow(matchedData[is.na(matchedData[,9]),]) > 0){ #if NAs exist

matchedNA <- matchedData[is.na(matchedData[,9]),]         #make dataframe of NA rows
#Subset complete cases  
matchedDataComplete <- matchedData[complete.cases(matchedData[,9]),]  #make dataframe of complete rows in variable column
    
    #Run match function again even if there are no NAs, it will otherwise go to the 'else' statement.
    matchedData2 <- matchEnvData(matchedNA[,1:8], nc=edinfo, buffer = c(gridsize/100+0.01,gridsize/100+0.01,0)) #use buffer of 1km to get data outside the grid size in either direction on same day
    matchedData2[matchedData2[,9]=='NA'] <- NA

        if (nrow(matchedData2[is.na(matchedData2[,9]),]) > 0){ #if using 0.11 buffer results in NAs
        
        matchedNA2 <- matchedData2[is.na(matchedData2[,9]),]  #make second dataframe of NA rows
        matchedDataComplete2 <- matchedData2[complete.cases(matchedData2[,9]),]  #make dataframe of complete rows in variable column
            
          #Run match function again
        matchedData3 <- matchEnvData(matchedNA2[,1:8], nc=edinfo, buffer = c(gridsize/100+0.02,gridsize/100+0.02,0)) #use buffer to get data outside the grid size in either direction on same day
        matchedData3[matchedData3[,9]=='NA'] <- NA
        
                if (nrow(matchedData3[is.na(matchedData3[,9]),]) > 0){ #if using 0.12 buffer still produces NAs
                matchedNA3 <- matchedData3[is.na(matchedData3[,9]),]  #make third dataframe of remaining NA rows
                matchedDataComplete3 <- matchedData3[complete.cases(matchedData3[,9]),]  #make dataframe of complete rows 
                
                  #Run match function again
                  matchedData4 <- matchEnvData(matchedNA3[,1:8], nc=edinfo, buffer = c(gridsize/100+0.03,gridsize/100+0.03,0)) #use buffer to get data outside the grid size in either direction on same day
                  matchedData4[matchedData4[,9]=='NA'] <- NA
                  
                          if (nrow(matchedData4[is.na(matchedData4[,9]),]) > 0){
                          matchedNA4 <- matchedData4[is.na(matchedData4[,9]),]  #make third dataframe of remaining NA rows
                          matchedDataComplete4 <- matchedData4[complete.cases(matchedData4[,9]),]  #make dataframe of complete rows 
                    
                            #Run match function again
                            matchedData5 <- matchEnvData(matchedNA4[,1:8], nc=edinfo, buffer = c(gridsize/100+0.1,gridsize/100+0.1,0)) #use buffer to get data outside the grid size in either direction on same day
                            matchedData5[matchedData5[,9]=='NA'] <- NA
                            
                                    if (nrow(matchedData5[is.na(matchedData5[,9]),]) > 0){ #if NA's still remain
                                      print('Reevaluate how to fill NAs')
                                      matchedNA5 <- matchedData5[is.na(matchedData5[,9]),]  #make third dataframe of remaining NA rows
                                      matchedDataComplete5 <- matchedData5[complete.cases(matchedData5[,9]),]  #make dataframe of complete rows 
                                    } else {
                                    print(paste0('No NAs, finally! Used buffer=', gridsize/100+0.1, ' for final NAs'))
                                    matchedDataFinal <- rbind(matchedDataComplete, matchedDataComplete2, matchedDataComplete3, matchedDataComplete4, matchedData5)
                                    print('The matched env data has the same number of rows as the centroid dataset') 
                                    nrow(matchedDataFinal) == nrow(centroidData)
                                    }
                  
                    
                        } else { #if no NAs in matchedData4 
                        print(paste0('No NAs, finally! Used buffer=', gridsize/100+0.03))
                        matchedDataFinal <- rbind(matchedDataComplete, matchedDataComplete2, matchedDataComplete3, matchedData4)
                        print('The matched env data has the same number of rows as the centroid dataset') 
                        nrow(matchedDataFinal) == nrow(centroidData)
                        }
                  
                } else { #if no NAs in matchedData3 
                  print(paste0('No NAs, finally! Used buffer=', gridsize/100+0.02))
                  matchedDataFinal <- rbind(matchedDataComplete, matchedDataComplete2, matchedData3)
                  print('The matched env data has the same number of rows as the centroid dataset') 
                  nrow(matchedDataFinal) == nrow(centroidData)
                }
                
        } else { #if no NAs in matchedData2
          print(paste0('No NAs, yay! Used buffer=', gridsize/100+0.01))
            matchedDataFinal <- rbind(matchedDataComplete, matchedData2)
          print('The matched env data has the same number of rows as the centroid dataset') 
            nrow(matchedDataFinal) == nrow(centroidData)
        }


        
} else { #if no NAs in matchedData, ie: no NAs during the first data grab
      print('No NAs, yay! All centroids filled on first try. No buffer necessary.')
      matchedDataFinal <- matchedData
      print('The matched env data has the same number of rows as the centroid dataset') 
      nrow(matchedDataFinal) == nrow(centroidData)
    
}


#4 of 4 SAVE!
EnvData_tmp=assign(paste0('EnvData_', envType), matchedDataFinal) 
saveRDS(EnvData_tmp, paste0('output/envData/', gridsize, ' km-', loctype, '/', surveynum, '_10km_',loctype2, '_', envType, '.rda'))

# EnvData_SST = matchedDataFinal
# saveRDS(EnvData_SST, here::here('output/envData', paste0(surveynum,'_10km_', envType, '_',loctype2, '.rda')))
# 
# EnvData_CHL = matchedDataFinal
# saveRDS(EnvData_CHL, here::here('output/envData', paste0(surveynum, '_10km_', envType, '_',loctype2, '.rda')))











EnvData_T600 = matchedDataFinal

EnvData_WP = matchedDataFinal



#3. 
matchedData5 <- matchEnvData(matchedNA4[,1:8], nc=edinfo, buffer = c(gridsize/100+0.1,gridsize/100+0.1,0)) #use buffer to get data outside the grid size in either direction on same day





mySST20 <- matchEnvData(mySSTdataNA[,1:8], nc=edinfo, buffer = c(0.2,0.2,86400))
mySST10 <- matchEnvData(mySSTdataNA[,1:8], nc=edinfo, buffer = c(0.1,0.1,86400))
mySST11 <- matchEnvData(mySSTdataNA[,1:8], nc=edinfo, buffer = c(0.11,0.11,86400))
mySST1 <- matchEnvData(mySSTdataNA[,1:8], nc=edinfo, buffer = c(0.01,0.01,86400))
mySST5 <- matchEnvData(mySSTdataNA[,1:8], nc=edinfo, buffer = c(0.05,0.05,86400))
mySST10b <- matchEnvData(mySSTdataNA[,1:8], nc=edinfo, buffer = c(0.10,0.10,0))

mySST20[mySST20$sst_mean=='NA'] <- NA
nrow(mySST20[is.na(mySST20$sst_mean),]) #check for NAs

par(mfrow = c(2,2))

hist(mySST10$sst_mean, breaks = 10, col = 'magenta')
hist(mySST11$sst_mean, breaks = 10,col = 'magenta')
hist(mySST20$sst_mean, breaks = 5,col = 'magenta')
hist(mySSTdataComplete$sst_mean, breaks = 10,col = 'blue')



dev.off()



edinfoCHL <-erddapToEdinfo('hawaii_soest_69ee_7d0d_74e6', baseurl = 'http://apdrc.soest.hawaii.edu/erddap/')
