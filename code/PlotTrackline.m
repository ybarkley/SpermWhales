 
sn = '1303';
dirname2 = 'C:\Users\yvers\Documents\CHP 2\data\All GPS Data\';
GPSdata = readtable([dirname2 'GPS_' sn '.csv'],'TreatAsEmpty',{'.','NA'});

dirname2 = 'C:\Users\yvers\Documents\CHP 3\SpermWhales\output\';
GPSdata = readtable([dirname2 'TestPath_1303subAllSpeeds.csv'],'TreatAsEmpty',{'.','NA'});


Pboat = GPSdata{:,{'GPSTime','Latitude','Longitude','Speed','Heading'}};
iboat = find(Pboat(:,4) >= 8 & Pboat(:,4) <= 10.5);
Pboat2 = Pboat(iboat, :);

figure; 
plot(Pboat(iboat,3), Pboat(iboat,2), '.')

figure
plot(Pboat(:, 3), Pboat(:, 2), '.')