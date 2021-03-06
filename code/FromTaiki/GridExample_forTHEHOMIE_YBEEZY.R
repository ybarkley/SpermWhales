# all the way through
source('./code/FromTaiki/gridFunctions.R')
# EXAMPLE WORK FLOW
# 1) First read in gps, then you need to convert longitudes to 0-360,
# convert UTC to POSIXct format, and create a column called "effort" that is TRUE or FALSE
# Column names needed: Longitude, Latitude, UTC, effort

gpsAll <- read.csv('./tests/straightPathWeffort_1706.csv', stringsAsFactors = FALSE) #read.csv('./tests/SpermWhale_1706_0716_effort.csv', stringsAsFactors = FALSE)
# gpsAll <- gpsAll %>% mutate_if(is.character, str_trim) #white space after join.veffort!?!?
gpsAll$effort <- gpsAll$straight == TRUE & gpsAll$join.aeffort == "on" # & gpsAll$join.veffort =="on"
gpsAll$Longitude <- ifelse(gpsAll$Longitude <= 0, gpsAll$Longitude + 360, gpsAll$Longitude)

gpsAll$UTC <- lubridate::ymd_hms(gpsAll$UTC)

# 2) Read in detections, similar to above convert column names to Longitude, Latitude, UTC,
# and "distance" if you need to create a detection function using Distance package
# Longitude must be matching 0-360 like above
# pmDets <- read.csv('./tests/SpermWhale_1706_BEST_detections.csv', stringsAsFactors = FALSE)
pmDets <- filter(SwEnvFinal, loc == 1 | loc == 0 & sid < 999) # localized encs and sightings
pmDets <- rename(pmDets, Longitude = lon, Latitude=lat, distance = pdist)
pmDets$Longitude <- ifelse(pmDets$Longitude <= 0, pmDets$Longitude + 360, pmDets$Longitude)
pmDets$UTC <- lubridate::ymd_hms(pmDets$UTC)
pmDets$distance <- abs(pmDets$distance)*1000

pmDetsAc <- filter(pmDets, loc==1)#, distance < 15)

# 3) fit a detection function if you need one
dsm <- Distance::ds(pmDets, key='hr')
# dsm <- Distance::ds(pmDetsAc, key='unif')


# 4) Run grid stuff. This will take a while, and should have a couple progress bars showing you how long the slow parts
# are taking. Set "trunc_m" to whatever truncation distance you might want to use
# It will ask you what pixel size you want  (this is the grid size), in the future you can specify this when you call it

#filter for just 1706 to test things
pmDetsub <- filter(pmDets, survey == 1706, loc == 1 | loc == 0 & sid < 999 ) # localized encs and sightings


gridEffort10 <- doAllGrid(gps = gpsAll,
                     bounds = NULL,
                     dets = pmDetsub,
                     trunc_m = 15e3, #METERS
                     dsmodel = dsm, #NULL or dsm 
                     pixel = NULL,
                     grid = NULL,
                     plot = F)

# Result of this is a list with the gps you provided, the grid it created, the effort values for each grid, and
# the detections you provided (these have the effortArea attached to them, as well as the actualArea of that grid
# cell - actualArea is mostly there as a reference point for sanity check, prob not used for much)

# Theres also a plotty function, but it will look pretty messy for all your data. Trackline is (roughly) in blue,
# detections are red dots, amount of effort is shaded in gray, but for a big grid you wont really be able to see the shading
plotGridResult(gridEffort10)


saveRDS(gridEffort10, file = paste0(here::here('output'), '/', 'gridEffort10km.rda'))
