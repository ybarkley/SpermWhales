# Compile loggers to make JKs life easier
library(RSQLite)
# What tables do you want to get?
loggersToGet <- c('Array', 'ARS', 'Deploy', 'Detection', 'Effort', 'UserInput')
loggersToGet <-c('Hydrophone_Depth_Data')
# Where are your sqlite dbs?
folder <- 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP2&3-Sperm\\data\\HICEAS_2017'

dbsToCombine <- list.files(folder, pattern='\\.sqlite3')

myData <- vector('list', length = length(loggersToGet))
names(myData) <- loggersToGet

for(db in dbsToCombine) {
  con <- dbConnect(paste(folder, db, sep='/'), drv=SQLite())
  tables <- dbListTables(con)
  for(log in loggersToGet) {
    if(log %in% tables) {
      tbl <- dbReadTable(con, log)

      # Adding in a column showing which DB each entry came from. Comment this part out if you dont want that
      ####
      if(nrow(tbl) > 0) {
        tbl$DB <- db
      }
      ####

      myData[[log]] <- rbind(myData[[log]], tbl)
    } else {
      cat('Table ', log, ' is not in database ', db, '\n')
    }
  }
  dbDisconnect(con)
}

# Prefix for the csv file names. Will be written as fileNameTableName.csv
fileName <- 'SetteDepth_July_'

for(log in loggersToGet) {
  write.csv(myData[[log]], file=paste0(fileName, log, '.csv'))
}
