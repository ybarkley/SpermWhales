# Plot box plots to look at the environmental data between encounters
# March 19, 2020

#load libraries
library(tidyverse)
library(reshape2)


#load data set
dir = 'C:\\Users\\yvers\\Documents\\CHP 3\\SpermWhales\\output/'
SwEnvData_tot <- read.csv(paste0(dir, 'SpermiesWithEnvData_20200326.csv'))



# center and scale predictors
SwEnvData_pred <- select(SwEnvData_tot, lat, lon2, sstAQ_m:wavepow)
SwEnvData_scale <- scale(SwEnvData_pred, scale = TRUE)
SwEnvData_scale <- cbind(select(SwEnvData_tot, ID:type, pdist), SwEnvData_scale)
write.csv(SwEnvData_scale, here::here('output', 'SpermiesWithEnvData_20200326-scaled.csv'), row.names = F)



# OR center data with 'colMeans()', from https://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/

#mean-centering function
# center_colmeans <- function(x) {
#   xcenter = colMeans(x)
#   x - rep(xcenter, rep.int(nrow(x), ncol(x)))
# }

# SwEnvData_cent <- center_colmeans(SwEnvData_pred)
# SwEnvData_cent <- cbind(select(SwEnvData_tot, ID:type, pdist), SwEnvData_cent)



#melt data into long format then plot
SwSome <- select(SwEnvData_scale, ID, sstAQ_m:wavepow)

SwDynamic <- select(SwSome, -(c(bath, d2land_km, slp_deg, asp_deg)))
# SwDynamic <- select(SwSome, ID, chla_m)
SwMeltDyn <- melt(SwDynamic, id.vars = 'ID')

boxDyn <- ggplot(SwMeltDyn, aes(variable, value)) +
  geom_boxplot()
boxDyn
ggsave(paste0(dir, 'boxplot_dynamic-scaled.png'), width = 6, height = 4)
# ggsave(paste0(dir, 'boxplot_chla.png'), width = 6, height = 4)
# ggsave(paste0(dir, 'boxplot_ssh.png'), width = 6, height = 4)


###
SwStatic <- select(SwEnvData_scale, ID, bath, d2land_km, slp_deg, asp_deg)
SwStatic <- select(SwEnvData_tot, ID, d2land_km)
SwMeltStat <- melt(SwStatic, id.vars = 'ID')

boxStat <- ggplot(SwMeltStat, aes(variable, value)) +
  geom_boxplot()
boxStat

ggsave(paste0(dir, 'boxplot_static-scaled.png'), width = 6, height = 4)
# ggsave(paste0(dir, 'boxplot_slope.png'), width = 6, height = 4)


###
SwLatLon <- select(SwEnvData_scale, ID, lat, lon2)
SwMeltLatLon <- melt(SwLatLon, id.vars = 'ID')

boxlatLon <- ggplot(SwMeltLatLon, aes(variable, value)) +
  geom_boxplot()
boxlatLon
ggsave(paste0(dir, 'boxplot_latlon-scaled.png'), width = 6, height = 4)










