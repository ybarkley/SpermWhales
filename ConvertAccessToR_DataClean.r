
### March 19, 2018
## This script pulls and edits depth and GPS from the original databases. 
## Depth data are first, then GPS data. Zeros are filtered from GPS data.

library(RODBC)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)


testdb <- file.path("C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\PICEAS2011\\PICEAS_1108_HFLogger.mdb") #1108
testdb <- file.path("HICEAS_1642_SETTE_HFLogger.mdb") #1642
testdb <- file.path("HICEAS_1641_MAC_MFLogger.mdb") #1641
testdb <- file.path("C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\PACES1303\\PACES_AcousticDatabase_YBpostcruise.accdb") #1303

# 1. Select 32 bit R (is just a setting in R studio)
# 2. search on windows for Set up ODBC data sources (32 bit)
# 3. Go to System DSN>Add
# 4. Choose Driver do Microsoft Access (*.mdb) > Finish
# 5. Data source name: ProjecnameAcc
# 6. Description: ProjectnameAcc
# 7. Make sure to actually select the database > OK
channel <- odbcConnect("1108Sette")
channel <- odbcConnect("1303Sette")
channel <- odbcConnect("1642Sette")
channel <- odbcConnect("1641Mac2MF")

depth_data <- sqlFetch(channel, "Hydrophone_Depth_Data") #1303
depth_data <- sqlFetch(channel, "Depth") #1108, 1642
GPS_data <- sqlFetch(channel, "GpsData")



# 1303 Depth suffers from wrong Month entry on 5/12/13. It is entered as 6/12/13, what a terrible affliction.

png(filename='C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\screenshots\\Depth data/Depth_1303test.png', width=790, height=520)

ggplot(depth_data, aes(UTC, Id)) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.margin = margin(10,20,10,10))+
  geom_point()+ geom_line()+
  #scale_y_discrete(breaks = seq(-55, 50, by=10), labels = seq(-55, 50, by=10))+
  scale_x_datetime(
    date_breaks="2 days", 
    date_labels = ("%m-%d-%Y")) +
  xlab("Date") +
  #ylab("Depth (m)")+
  labs(title = "Depth_1303")

dev.off()


depth_data$Month <- month(depth_data$UTC)
#wrongMonth <- filter(depth_data, UTC >= "2013-06-12")

#Parse it out
depth_data$Year <- year(depth_data$UTC)
depth_data$Month <- month(depth_data$UTC)
depth_data$Day <- day(depth_data$UTC)
depth_data$hour <- hour(depth_data$UTC)
depth_data$min <- minute(depth_data$UTC)
depth_data$sec <- second(depth_data$UTC)

#Fix the problem
depth_data$Month[which(depth_data$Month != 05 & depth_data$Day >= 08 & depth_data$Day <= 31)] = '5' 
#wrongMonth$Month[which(wrongMonth$Month == 06)] = '05'
#Put it back together
depth_data$UTC = ymd_hms(paste(depth_data$Year, depth_data$Month, depth_data$Day,  depth_data$hour, depth_data$min, depth_data$sec))
#Remove extra rows
depth_data<- depth_data[, c(1:10)]
depth_data<- depth_data[, -c(3:6)]

write.csv(depth_data, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All Depth Data\\Depth_1303_raw.csv', row.names = F)


#1108 & 1642 Depth (no depth for 1641)
depth_data<- depth_data[, -2]
colnames(depth_data)<-c("Id", "UTC", "Sensor_0_Raw", "Sensor_0_Depth")
#Add "Sensor_1_Raw", "Sensor_1_Depth") #1108
depth_data$Sensor_1_Raw <- NA
depth_data$Sensor_1_Depth <- NA
#Sort by dates using POSIX format
depth_data<-depth_data[order(depth_data$UTC, decreasing = F),]
depth_data$year<-year(depth_data$UTC) 

depth_data<-filter(depth_data, year==2011)
depth_data<-depth_data[, -7]

#Plot
png(filename='C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\screenshots\\Depthdata/Depth_1642test.png', width=790, height=520)

ggplot(depth_data, aes(UTC, Id)) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.margin = margin(10,20,10,10))+
  geom_point()+ geom_line()+
  #scale_y_discrete(breaks = seq(-55, 50, by=10), labels = seq(-55, 50, by=10))+
  scale_x_datetime(
    date_breaks="2 days", 
    date_labels = ("%m-%d-%Y")) +
  xlab("Date") +
  #ylab("Depth (m)")+
  labs(title = "Depth_1642")

dev.off()

write.csv(depth_data, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All Depth Data\\Depth_1642_raw.csv', row.names = F)

#Examine 1642 depth
depth1642 <- depth_data[, c(1:4)]
depth1642$Month <- month(depth1642$UTC)
depth1642$day <- day(depth1642$UTC)
depth1642 <- filter(depth1642, UTC >= "2010-09-29", UTC <= "2010-10-10")


#############
#### GPS ####
#############

#1108 GPS / 1642 GPS / 1641 GPS

# 1108 delete first rows due to error in GPS readings
GPS_data<-GPS_data[-c(1:15),] #1108 only
colnames(GPS_data)[colnames(GPS_data) == 'Index'] <- 'Id'
colnames(GPS_data)[colnames(GPS_data) == 'PCTime'] <- 'UTC'
GPS_data<-GPS_data[order(GPS_data$UTC, decreasing = F),] #make sure it's in order
GPS_data <- filter(GPS_data, Latitude > 0)
gpsSEPT4 <- filter(GPS_data, UTC >= "2010-09-04", UTC <= "2010-09-05")



#Plot
png(filename='C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\screenshots\\GPS_1642-sept4.png', width=960, height=400)

ggplot(gpsSEPT4, aes(UTC, Id)) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.margin = margin(10,20,10,10))+
  geom_point()+ geom_line()+
  #scale_y_discrete(breaks = seq(-55, 50, by=10), labels = seq(-55, 50, by=10))+
  scale_x_datetime(
    date_breaks="1 day", 
    date_labels = ("%m-%d-%Y")) +
  xlab("Date") +
  #ylab("Depth (m)")+
  labs(title = "GPS_1642_no order")

dev.off()

# Just inspecting Nov 3, several minutes of GPS data are missing and multiple days
GPS_data$Month<-month(GPS_data$UTC)
GPS_data$Day<-day(GPS_data$UTC)
nov3 <- filter(GPS_data, Day==3)
GPS_data<- GPS_data[, -c(14,15)] #remove Month & Day columns

#Add column and reorder to match 1303 GPS
GPS_data$MagneticVariation <-NA
GPS_data<-GPS_data[, c(1, 2, 5, 3, 4, 8, 9, 6, 7, 12, 13, 14, 10,11)]

write.csv(GPS_data, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All GPS Data\\GPS_16421_aw.csv', row.names = F)


###
####1303 GPS####
GPS_data <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All GPS Data/GPS_1303_raw.csv')

GPS_data$UTC<- ymd_hms(GPS_data$UTC)

png(filename='C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\screenshots\\GPS data/GPS_1303test2.png', width=960, height=400)

ggplot(gps1303, aes(UTC, Id)) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.margin = margin(10,20,10,10))+
  geom_point()+ geom_line()+
  #scale_y_discrete(breaks = seq(-55, 50, by=10), labels = seq(-55, 50, by=10))+
  scale_x_datetime(
    date_breaks="2 days", 
    date_labels = ("%m-%d-%Y")) +
  xlab("Date") +
  #ylab("Depth (m)")+
  labs(title = "GPS_1303")

dev.off()

gps1303 <- GPS_data

gps1303$Month <- month(GPS_data$UTC)
#wrongMonth <- filter(depth_data, UTC >= "2013-06-12")

#Parse it out
gps1303$Year <- year(gps1303$UTC)
gps1303$Month <- month(gps1303$UTC)
gps1303$Day <- day(gps1303$UTC)
gps1303$hour <- hour(gps1303$UTC)
gps1303$min <- minute(gps1303$UTC)
gps1303$sec <- second(gps1303$UTC)

#Fix the problem
gps1303$Month[which(gps1303$Month != 05 & gps1303$Day >= 08 & gps1303$Day <= 31)] = '5' 
#wrongMonth$Month[which(wrongMonth$Month == 06)] = '05'
#Put it back together
gps1303$UTC = ymd_hms(paste(gps1303$Year, gps1303$Month, gps1303$Day,  gps1303$hour, gps1303$min, gps1303$sec))
#Remove extra columns
gps1303<- gps1303[, c(1:18)]
gps1303<- gps1303[, -c(3:6)]

write.csv(gps1303, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All GPS Data\\GPS_1303_raw.csv', row.names = F)

#### 1604 GPS####
#Used Taiki's code to extract GPS table from SQLite db

GPS_data <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All GPS Data\\GPS_1604-Test.csv')
GPS_data$UTC<- mdy_hms(GPS_data$UTC) #Converts UTC to POSIXct format
GPS_data<-GPS_data[order(GPS_data$UTC, decreasing = F),] #make sure it's in order


#Plot 
png(filename='C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\screenshots\\GPS data/GPS_1604_subset.png', width=960, height=400)


ggplot(gps1604, aes(UTC, Latitude)) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.margin = margin(10,20,10,10))+
  geom_point()+ geom_line()+
  #scale_y_discrete(breaks = seq(-55, 50, by=10), labels = seq(-55, 50, by=10))+
  scale_x_datetime(
    date_breaks="2 days", 
    date_labels = ("%m-%d-%Y")) +
  xlab("Date") +
  #ylab("Depth (m)")+
  labs(title = "GPS_1604")

dev.off()

#Found that most of the GPS was not working for most of the first half of 1604 
#Examination: Subset to see which dates actually have GPS data
gps1604 <- filter(GPS_data, Latitude >= 0 & Latitude <= 30)
jul0405 <-filter(gps1604, UTC >="2016-07-03" & UTC < "2016-07-05" )

#Reorder columns to match 1303
gps1604 <- gps1604[ , c(1,2,7:18)]

write.csv(gps1604, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\All GPS Data\\GPS_1604_subset.csv', row.names = F)
