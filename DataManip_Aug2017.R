 #########
## August 1, 2017
## Data Manipulation of acoustic datasets


###Add negative sign to longitudes since they are all 'W' in HICEAS Sette acoustic data
settedata <-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\HICEAS_1642_SETTE_AcousticDetections_WLABEL.csv')

settedata$Longitude <- (settedata$Longitude*-1)

write.csv(settedata, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\HICEAS_1642_SETTE_AcousticDetections_WLABEL.csv')

macdata <-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\HICEAS_1641_MAC_AcousticDetections_WLABEL.csv')
macdata$Longitude <- ifelse(macdata$E.W == 'E', macdata$Longitude*-1, macdata$Longitude)
#macdata$Longitude <- (macdata$Longitude*-1)

write.csv(macdata, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\HICEAS_1641_MAC_AcousticDetections_WLABEL.csv', )




#HITEC needs label with cruise #, ac ID and vis ID
hitecdata <-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\HITEC_1604_Sette_AcousticDetections.csv')
hitecdata$Label1 <- '1604'
hitecdata$Label2 <- paste('A',hitecdata$ac_id,sep="")
hitecdata$Label3 <- paste('S',hitecdata$vis_id, sep="")
hitecdata$Label <-paste(hitecdata$Label1,hitecdata$Label2,hitecdata$Label3, sep=".")
hitecdata <- hitecdata[, -c(41:43)] #or hitecdata[41:43]<-NULL
hitecdata <- hitecdata[, c(40, 1:39)]
write.csv(hitecdata, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\HITEC_1604_Sette_AcousticDetections.csv')

#piceas1108 needs labels too!!!
pic1108 <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\PICEAS_1108_AcousticDetections_WLABEL.csv')
pic1108$Label1 <- '1108'
pic1108$Label2 <- paste('A',pic1108$Acoustic.ID,sep="")
pic1108$Label3 <- paste('S',pic1108$Visual.Sighting, sep="")
pic1108$Label <- paste(pic1108$Label1, pic1108$Label2, pic1108$Label3, sep=".")
pic1108 <- pic1108[, -c(31:33)]
pic1108 <- pic1108[, c(31, 1:30)]
write.csv(pic1108, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\PICEAS_1108_Sette_AcousticDetections_WLABEL.csv')

#Make PICEAS one datasheet for crying out loud
pic1108new <-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\PICEAS_1108_Sette_AcousticDetections_WLABEL.csv')
pic1203 <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\PICEAS_1203_AcousticDetections_WLABEL.csv')
pic1108new <- pic1108new[, -1]
colnames(pic1108new)[colnames(pic1108new)=='X']<- "Comments"
piceas <- rbind(pic1108new, pic1203)
write.csv(piceas, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\PICEAS_Sette_AcousticDetections_WLABEL.csv')




