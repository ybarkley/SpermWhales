# Check environmental data

#load data for a survey
surveynum = 1303
gridsize = 10
loctype = 'localized'

env1_sst      <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_sst.rda')))
env2_chla     <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_chla.rda')))
env3_godas600 <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_temp600.rda')))
env4_ww3      <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_wavepow.rda')))
env5_bath     <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_bathy.rda')))
env67_slpasp  <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_slpasp.rda')))
env8_d2land   <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_d2land.rda')))
env9_ssh      <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_ssh.rda')))
env10_sshsd   <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_sshsd.rda')))
env11_eke     <- readRDS(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_eke.rda')))
env12_d2smt   <- read.csv(here::here(paste0('output/data_output/', gridsize, ' km-', loctype), paste0(surveynum, '_', gridsize, 'km_dist2smt.csv')))
env12_d2smt   <- filter(env12_d2smt, effCells %in% env5_bath$effCells)
#Use bathymetry and distance to land as guides.
  # When bathymetry and distance to land are positive, this means the grid cell is on land.

SwEnv1706 <- read.csv(here::here('output','SpermiesWithEnvData_1706.csv'))

tmp <- filter(SwEnv1706, bath < 0)
tmp2 <- filter(tmp, bath < -100)

SwEnv1706b=tmp2
colnames(SwEnv1706b)[5] <- "Longitude"
colnames(SwEnv1706b)[6] <- "Latitude"

SwEnv1706b$UTC <- as.character(SwEnv1706b$UTC)

SwEnv1706b$UTC <- lubridate::ymd_hms(SwEnv1706b$UTC)
