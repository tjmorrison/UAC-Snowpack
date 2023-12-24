%Use JSON parser to put data for Mesowest 
%Data are pulled from MESOWEST's server using their API
%Info available at: http://mesowest.org/api/
%Contact at Mesowest: Joe Young

clear all;
close all;
%start_time = 201507290700; %MST = UTC - 7;
%end_time =   201507300800;
mesowest_url = 'http://api.mesowest.net/v2/stations/timeseries?token=demotoken&start=201507290000&end=201507290100&state=UT&county=Salt%20Lake'
%mesowest_url = 'http://api.mesowest.net/v2/stations/timeseries?stid=wbb&start=201312010000&end=201312011200&token=1234567890';

matlab_results = JSON.parse(urlread(mesowest_url));

%disp(matlab_results)

Rsraw = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.solar_radiation_set_1)'; %W/m^2
WSraw = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.wind_speed_set_1)';
RH = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.relative_humidity_set_1)';
Temp = cell2mat(matlab_results.STATION{1}.OBSERVATIONS.air_temp_set_1)';

year = str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{1}(1:4));
month = str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{1}(6:7));
day =  str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{1}(9:10));
hr =  str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{1}(12:13));
mins = str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{1}(15:16));
sec = str2num(matlab_results.STATION{1}.OBSERVATIONS.date_time{1}(18:19));

altitude = str2num(matlab_results.STATION{1}.ELEVATION)/3.281; % meters
lat = str2num(matlab_results.STATION{1}.LATITUDE);
long = str2num(matlab_results.STATION{1}.LONGITUDE);
zm = str2num(matlab_results.STATION{1}.SENSOR_VARIABLES.wind_speed.wind_speed_set_1.position);


