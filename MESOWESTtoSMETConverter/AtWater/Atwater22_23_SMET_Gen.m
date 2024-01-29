%Script to gather data from MESOWEST and put it into
%SNOWPACKS *.smet format.
%*.smet data files can be easily visualized using niViz at https://niviz.org
%Eric Pardyjak 11/2020
%
% This code was edited in Winter 2023-2024 to run SNOWPACK for the UAC to
% begin their transition to running snowpack. The data analyzed is from
% Atwater which was had troubles with both snow depth and solar radiation.
% To combat the data issues, data retiming and gap will is performed. This
% code will serve as a one off, but will continue to evole as the UAC
% begins implimenting SNOWPACK for more real-time runs


clear all;
close all;

start_time = '202310050000'; %YYYYMMDDHHMM UTC;
end_time =   '202401290000';
%get Mesowest data
mesowest_url = strcat('http://api.mesowest.net/v2/stations/timeseries?stid=ATH20&token=3d5845d69f0e47aca3f810de0bb6fd3f&start=',start_time,'&end=',end_time)

%Parse JSON data from Mesowest for use in Matlab
matlab_results = webread(mesowest_url);

dnum=parse_mesowest_date(matlab_results);

TA = matlab_results.STATION.OBSERVATIONS.air_temp_set_1'+273.15;
TSS = matlab_results.STATION.OBSERVATIONS.surface_temp_set_1'+273.15;
RH = (matlab_results.STATION.OBSERVATIONS.relative_humidity_set_1')/100; %Snowpack needs RH as fraction between 0 and 1
ISWR = matlab_results.STATION.OBSERVATIONS.solar_radiation_set_1'; %Incoming shortwave radiation (W/m^w)
VW = matlab_results.STATION.OBSERVATIONS.wind_speed_set_1'; %Wind speed (m/s)
DW = matlab_results.STATION.OBSERVATIONS.wind_direction_set_1'; %Wind direction
HS = (matlab_results.STATION.OBSERVATIONS.snow_depth_set_1')/1000; %snow depth in meters
HS(1:452) = 0;
TSG = zeros(length(HS))+273.15;%Assume ground surface below snow is 0 C
%% Merge Guard station snow depth
mesowest_url_AGD = strcat('http://api.mesowest.net/v2/stations/timeseries?stid=AGD&token=3d5845d69f0e47aca3f810de0bb6fd3f&start=',start_time,'&end=',end_time)

%Parse JSON data from Mesowest for use in Matlab
matlab_results_AGD = webread(mesowest_url_AGD);
HS_AGD = (matlab_results_AGD.STATION.OBSERVATIONS.snow_depth_set_1')/100; %snow depth in meters

dnum_AGD = parse_mesowest_date(matlab_results_AGD);
%% Convert to time table to resample the vars
%need to remap to hourly data for both datasets...
% TO: Remap all vars to 1 hr time step
ATH_TT = retime(timetable(datetime(datevec(dnum)),HS',ISWR'),'hourly','linear');
AGD_TT = retime(timetable(datetime(datevec(dnum_AGD)),HS_AGD'),'hourly','linear');


%Plot HS 
% Based on this it appears that the HST from Atwater will work for intial
% modeling. It also looks best if we run a moving mean over 5 hours on the
% data to smooth the signal
figure()
plot(AGD_TT.Time,movmean(AGD_TT.Var1,5))
hold on
plot(ATH_TT.Time,movmean(ATH_TT.Var1,5))
legend('AGD','ATH')
ylabel('HST')

%% Now the ANN for gapfilling the solar rad...

%%

fields = 'timestamp TA RH TSG TSS HS VW DW ISWR'

%Removing negative solar radation values
for i = 1:length(ISWR)
    if(ISWR(i)<0)
        ISWR(i) = 0;
    end
end


%Smet file operations
StationID =  matlab_results.STATION.STID;%From Mesowest data
StationName = matlab_results.STATION.NAME;
latitude = str2num(matlab_results.STATION.LATITUDE); %Site lat
longitude = str2num(matlab_results.STATION.LONGITUDE); %Site lon
altitude = str2num(matlab_results.STATION.ELEV_DEM)*0.3048;  %Site altitude in ft, converted to m above sea level
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

%% Make some plots to quickly check data
figure;
sgtitle('Atwater SNOWPACK data')
subplot(5,1,1);
plot(dnum,VW);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('Wind Speed (m/s)')
datetickzoom

subplot(5,1,2);
plot(dnum,ISWR);
%plot(ATH_TT.Time,ATH_TT.Var2);
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
%plot(ATH_TT.Time,ATH_TT.Var1)
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
ylabel('Snow Depth (m)')
datetickzoom

