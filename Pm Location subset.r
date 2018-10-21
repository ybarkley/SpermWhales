#subsetting Lat/Long from Sperm whale data

swdata <-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\Sperm_Whale_Acoustic_Locations.csv')
swdata <- swdata[, c(1,2,5,8:13)]
swsub <- subset(swdata, (latitude > 15 & latitude < 32 ) & (longitude > 177 | longitude < -151)) #Thinking about the values on a number line, my dataset does not have values >177 AND < -151. But it does have values that are >15 AND <32. If converting lat/longs to 360 scale, then -179 = 181deg. Then the longitude values would be >177 & <360-151=209. 

swsub$sighted <- NA
swsub$sighted <- ifelse(swsub$visual_id != 999, "1", "0")

write.csv(swsub, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\Sperm_Whale_AcLocations_EEZonly_20180420.csv', row.names = F)




## FOR MAPPING
# By switching all the negative values of W to positive, it makes the subsetting prettier
#remove them negatives and make a new column for the new values, make a new column called lat.new (or long.new), check IF E_W is "W", if so multiply by -1, otherwise leave it as it was"
#thanks Maia Kapur
swdata$lon.new = ifelse(swdata$E_W == 'W', swdata$longitude*-1, swdata$longitude)




