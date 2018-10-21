nrow(nwhi_350[which(nwhi_350$group=='24'),])


nrow(nwhi_350[which(nwhi_350$group=='22'),])


nrow(test_nwhi[which(test_nwhi$group=='24'),])


hist(pcnew$FREQPOSSLOPEMEAN, pcnew$population, pcnew )

#subsetting out entire columns
pmdata<-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP3&4-Sperm\\data\\CNPSpermWhaleSightings_1986-2010_Yvonne.csv')

pm=pmdata[,c(5,6,7)] #index it!