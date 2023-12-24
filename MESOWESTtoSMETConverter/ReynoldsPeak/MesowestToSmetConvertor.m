%Script to gather data from MESOWEST Reynolds Peak site and put it into
%SNOWPACKS *.smet format.
%*.smet data files can be easily visualized using niViz at https://niviz.org
%Eric Pardyjak 11/2020

 clear all;
 close all;

start_time = '202011052200'; %YYYYMMDDHHMM UTC;
end_time =   '202011061800';
%get Mesowest data
mesowest_url = strcat('http://api.mesowest.net/v2/stations/timeseries?stid=REY&token=3d5845d69f0e47aca3f810de0bb6fd3f&start=',start_time,'&end=',end_time)

%Parse JSON data from Mesowest for use in Matlab
matlab_results = JSON.parse(urlread(mesowest_url));
data_length = length(matlab_results.STATION{1}.OBSERVATIONS.date_time);

for i=1:data_length
year(i) = str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{i}(1:4));
month(i) = str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{i}(6:7));
day(i) =  str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{i}(9:10));
hour(i) =  str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{i}(12:13));
min(i) =  str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{i}(15:16));
second(i) =  str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{i}(18:19));
end


TA = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.air_temp_set_1)'+273.15;
TSS = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.surface_temp_set_1)'+273.15;
RH = (cell2mat(matlab_results.STATION{1}.OBSERVATIONS.relative_humidity_set_1)')/100; %Snowpack needs RH as fraction between 0 and 1
ISWR = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.solar_radiation_set_1)'; %Incoming shortwave radiation (W/m^w)
VW = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.wind_speed_set_1)'; %Wind speed (m/s)
DW = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.wind_direction_set_1)'; %Wind direction
HS = (cell2mat(matlab_results.STATION{1}.OBSERVATIONS.snow_depth_set_1)')/100; %snow depth in meters
TSG = zeros(length(HS))+273.15;%Assume ground surface below snow is 0 C

fields = 'timestamp TA RH TSG TSS HS VW DW ISWR'

%Removing negative solar radation values
for i = 1:length(ISWR)
    if(ISWR(i)<0)
        ISWR(i) = 0;
    end
end

istart =1;
iend = length(year);
dtn = [year(istart:iend)',month(istart:iend)',day(istart:iend)',hour(istart:iend)',min(istart:iend)',second(istart:iend)']; %date vector
dnum = datenum(dtn); %puts date in 7.3748e+05 format

%Smet file operations
StationID =  matlab_results.STATION{1}.STID;%From Mesowest data
StationName = matlab_results.STATION{1}.NAME;
latitude = str2num(matlab_results.STATION{1}.LATITUDE); %Site lat
longitude = str2num(matlab_results.STATION{1}.LONGITUDE); %Site lon
altitude = str2num(matlab_results.STATION{1}.ELEV_DEM);  %Site altitude in meters above sea level
UTM_zone  = utmzone(latitude,longitude);
%code from mapping toolbox to compute UTMX/UTMY from lat/lon
[ellipsoid,estr] = utmgeoid(UTM_zone);
utmstruct = defaultm('utm');
utmstruct.zone = num2str(UTM_zone);
utmstruct.geoid = ellipsoid;
utmstruct = defaultm(utm(utmstruct));
[UTMx,UTMy] = mfwdtran(utmstruct,latitude,longitude); 
easting = UTMx; %UTMX
northing = UTMy; %UTMY
nodata = -999;
tz = -6; %Utah offset between MST and UTC time zones MST = UTC - 6
source = 'University of Utah EFD Lab';

%Start writing out Snowpack .smet file

fileID = fopen(strcat(StationID,'.smet'),'w');

% SMET 1.1 ASCII
% [HEADER]
% station_id       = WindyMeadow
% station_name     = WindyMeadowTestPlot
% latitude         = 40.840781
% longitude        = -111.035792
% altitude         = 2994.355
% easting          = 496982.61
% northing         = 4521083.10
% nodata           = -999
% tz               = -6
% source           = Univerity of Utah EFD Lab
% fields           = timestamp TA RH TSG TSS HS VW DW OSWR ISWR ILWR PSUM TS1 TS2 TS3
% units_offset = 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0    #this whole line could be ommitted
% units_multiplier = 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1        #this whole line could be ommitted
% [DATA]

fprintf(fileID,'%s \n','SMET 1.1 ASCII');
fprintf(fileID,'%s \n','[HEADER]');
fprintf(fileID,'%s %s \n','station_id       =',StationID);
fprintf(fileID,'%s %s \n','station_name     =',StationName);
fprintf(fileID,'%s %s \n','latitude         =',num2str(latitude));
fprintf(fileID,'%s %s \n','longitude        =',num2str(longitude));
fprintf(fileID,'%s %s \n','altitude         =',num2str(altitude));
fprintf(fileID,'%s %s \n','easting          =',num2str(easting));
fprintf(fileID,'%s %s \n','northing         =',num2str(northing));
fprintf(fileID,'%s %s \n','nodata           =',num2str(nodata));
fprintf(fileID,'%s %s \n','tz               =',num2str(tz));
fprintf(fileID,'%s %s \n','source           =',source);
fprintf(fileID,'%s %s \n','fields           =',fields);
fprintf(fileID,'%s \n','[DATA]');


for i=1:length(HS)
 iso_dates = datestr8601(dnum(i), '*ymdHMS'); %converstion to .smet ISO8601 date format
    
 fprintf(fileID,'%s %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f \n',iso_dates,TA(i),RH(i),TSG(i),TSS(i),HS(i),VW(i),DW(i),ISWR(i));
end

fclose(fileID);

%Make some plots to quickly check data
figure;
sgtitle('Reynolds Peak SNOWPACK data')
subplot(5,1,1);
plot(dnum,VW);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('Wind Speed (m/s)')
datetickzoom

subplot(5,1,2);
plot(dnum,ISWR);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('Solar Radiation (W/m^2)')
datetickzoom

subplot(5,1,3);
plot(dnum,TSS,dnum,TA);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('Temperatures (C)')
legend('Air Temp','Surface Temp')
datetickzoom

subplot(5,1,4);
plot(dnum,RH*100);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('RH (%)')
datetickzoom

subplot(5,1,5);
plot(dnum,HS);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('Snow Depth (cm)')
datetickzoom

