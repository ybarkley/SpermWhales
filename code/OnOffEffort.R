filename <- 'HICEAS17.das'

print("Replacing # event characters with C because read.fwf chokes on #")
filename2 <- paste(filename,"2",sep="")
unlink(filename2)
con <- file(filename, "r", blocking = FALSE)  # open connection to DAS file
zz <- 1
while (zz==1) {
  newline <- readLines(con, n=1)
  if (length(newline) == 0) {break}           # exit while loop at eof
  substr(newline,4,4) <- gsub("#","C",substr(newline,4,4))    
  newline <- gsub("#"," ",newline)    
  write (newline, filename2, append=TRUE)
}
close(con)

#Note, the above no longer works in R version 3.5.2. Create das2 file in earlier version of
#R and START HERE:
filename2 <- 'HICEAS17.das2'

#Read DAS line to create fixed-width array elements:
#014B.062918 081817 N32:02.23 W120:02.70 1705    P   -7    Y
#009R.151510 080410 N32:36.26 W117:17.86    S
#011V.151510 080410 N32:36.26 W117:17.86    3   03  280      08.0
#017E 152242 080410 N32:35.88 W117:19.37    C
#(3)  1 = line no.
#(2)  2 = event code, dot
#(6)  3 = time
#(7)  4 = date
#(2)  5 = space, N/S
#(2)  6 = LatDeg
#(1)  7 = :
#(5)  8 = LatMin
#(2)  9 = space, E/W
#(3) 10 = LonDeg
#(1) 11 = :                                                          
#(5) 12 = LonMin  
#                   	R       	V		E 
#(5) 13 = Field1:	Survey/Eff  	Beauf		Survey/Eff

#Establish data frame to work with
x <- read.fwf(filename2, width=c(3,2,2,2,2,3,2,2,2,2,1,5,2,3,1,5,5), 
              col.names=c("Line","Event","Time1","Time2","Time3","Date1","Date2","Date3","N_S",
                          "LatDeg","Nada","LatMin","E_W","LonDeg","Nada2","LonMin","Field1"), 
                          stringsAsFactors=FALSE)

#Clean up data frame (delete unuseful columns)
x$Nada <- NULL
x$Nada2 <- NULL
x <- subset(x,Line>=0)

#Get time and date in better format for .csv
x$Time <- paste(x$Time1,x$Time2,sep=":")
x$Time <- paste(x$Time,x$Time3,sep=":")
x$Date <- paste(x$Date1,x$Date2,sep="/")
x$Date <- paste(x$Date,x$Date3,sep="/")
x$Time1 <- NULL
x$Time2 <- NULL
x$Time3 <- NULL
x$Date1 <- NULL
x$Date2 <- NULL
x$Date3 <- NULL

#Create decimal lat/lon positions with correct W/E orientation
#Should theoretically do this for N/S, but I know this survey did not cross the equator
x$Lat <- x$LatDeg + (x$LatMin/60)
x$Lon <- x$LonDeg + (x$LonMin/60)
x$LatDeg <- NULL
x$LonDeg <- NULL
x$LatMin <- NULL
x$LonMin <- NULL
for(i in 1:length(x$Line)) {
  if (x$E_W[i]==" W")
  {x$Lon[i] <- 0-x$Lon[i]}
}
x$N_S <- NULL
x$E_W <- NULL

#Code each line as on-effort (1) or not (0)
x$Effort <- 0
for(i in 1:length(x$Line)) {
  if (x$Event[i]=="R." & x$Field1[i]=="    S")
  {x$Effort[i] <- "On"}
}
for(i in 1:length(x$Line)) {
  if (x$Event[i]=="R." & x$Field1[i]=="    N")
  {x$Effort[i] <- "On"}
}
for(i in 1:length(x$Line)) {
  if (x$Event[i]=="E ")
  {x$Effort[i] <- "Off"}
}

#Add ship name
x$Ship <- NA
for(i in 1:length(x$Line)) {
  if (x$Event[i]=="B." & x$Field1[i]==" 1705")
  {x$Ship[i] <- "Lasker"}
}
for(i in 1:length(x$Line)) {
  if (x$Event[i]=="B." & x$Field1[i]==" 1706")
  {x$Ship[i] <- "Sette"}
}
for(i in 1:length(x$Line)) {
  if (is.na(x$Ship[i]))
  {x$Ship[i] <- x$Ship[i-1]}
}

Final <- subset(x,Effort!=0)
Final$Line <- NULL
Final$Event <- NULL
Final$Field1 <- NULL

Final <- Final[,c(2,1,3,4,5,6)]
write.csv(Final,"HICEAS2017_OnOffEffort.csv",row.names=FALSE)
