gridTime <- function(x, gps) {
  effCells <- which(x$effort>0)
  dets <- x$detections$gridIndex
  
  result = NULL
  for (eff in effCells){
    
    gridTest<-x$grid[[eff]]
    ct <- st_centroid(gridTest)  #get lat lon of centroid 
    
    #get closest matches to lon & lat. Doesn't give exact match, but then check if it's within the same day (1440 mins)
    ct_lonlat <- which(abs(gps$Longitude-ct[1]) == min(abs(gps$Longitude-ct[1])) |
                         abs(gps$Latitude-ct[2]) == min(abs(gps$Latitude-ct[2])))
    
    ct_time <- ifelse(gps[ct_lonlat[1],]$UTC-gps[ct_lonlat[2],]$UTC < 1440, gps[ct_lonlat[1],]$UTC, NA) #shouldn't get NAs... 
    
    #convert times back to posix format
    if (ct_time > 0){
      ct_time<-as.POSIXct(ct_time, tz='GMT', origin = '1970-01-01')
    } else {
      ct_time
    }
    
    #mark presences and absences (1 or 0)
    if (eff %in% dets == TRUE){
      pa = 1
    } else {
      pa = 0
    }
    
    #combine it all
    ctdf <- data.frame(effCells=eff, UTC=ct_time, lon=ct[1], lat=ct[2], pa=pa)
    
    result <- rbind(result, ctdf)
}
  
  result
}
