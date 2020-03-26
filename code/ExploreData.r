# Plot box plots to look at the environmental data between encounters
# March 19, 2020

#load libraries
library(tidyverse)
library(reshape2)


#load data set
dir = 'C:\\Users\\yvers\\Documents\\CHP 3\\SpermWhales\\output/'
SwEnvData_tot <- read.csv(paste0(dir, 'SpermiesWithEnvData_20200310.csv'))


# center data with 'colMeans()', from https://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/

center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

# apply it
SwEnvData_pred <- select(SwEnvData_tot, lat, lon2, sstAQ_m:wind_ms)

SwEnvData_cent <- center_colmeans(SwEnvData_pred)
SwEnvData_cent <- cbind(select(SwEnvData_tot, ID:type, pdist), SwEnvData_cent)



#melt data into long format then plot
SwSome <- select(SwEnvData_cent, ID, sstAQ_m:wind_ms)
SwDynamic <- select(SwSome, -(c(bath, d2land_km, slp_deg, asp_deg)))
# SwDynamic <- select(SwSome, ID, chla_m)
SwMeltDyn <- melt(SwDynamic, id.vars = 'ID')

boxDyn <- ggplot(SwMeltDyn, aes(variable, value)) +
  geom_boxplot()
boxDyn
ggsave(paste0(dir, 'boxplot_dynamic.png'), width = 6, height = 4)
# ggsave(paste0(dir, 'boxplot_chla.png'), width = 6, height = 4)
# ggsave(paste0(dir, 'boxplot_ssh.png'), width = 6, height = 4)


###
SwStatic <- select(SwEnvData_cent, ID, bath, d2land_km, slp_deg, asp_deg)
# SwStatic <- select(SwEnvData_cent, ID, slp_deg)
SwMeltStat <- melt(SwStatic, id.vars = 'ID')

boxStat <- ggplot(SwMeltStat, aes(variable, value)) +
  geom_boxplot()
boxStat

ggsave(paste0(dir, 'boxplot_static.png'), width = 6, height = 4)
# ggsave(paste0(dir, 'boxplot_slope.png'), width = 6, height = 4)


###
SwLatLon <- select(SwEnvData_cent, ID, lat, lon2)
SwMeltLatLon <- melt(SwLatLon, id.vars = 'ID')

boxlatLon <- ggplot(SwMeltLatLon, aes(variable, value)) +
  geom_boxplot()
boxlatLon
ggsave(paste0(dir, 'boxplot_latlon.png'), width = 6, height = 4)










