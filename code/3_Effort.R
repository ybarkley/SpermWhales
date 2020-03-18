# Open gps data for straightPath function


gps1303 <- read.csv(here('data', 'GPS_1303.csv'))


#filter gps data for speeds between 8-10 knots
gps1303sub1 <- filter(gps1303, Speed > 8 & Speed < 10)


dv = gps1303sub1$UTC

hh = floor(gps1303$GPSTime/10000);
mm = floor((gps1303$GPSTime-hh*10000)/100);
ss = floor((gps1303$GPSTime-hh*10000-mm*100));
dv = c(dv, hh, mm, ss)
