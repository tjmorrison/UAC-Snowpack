 clear all;
 close all;

start_time = '202101200000'; %YYYYMMDDHHMM UTC;
end_time =   '202103121800';
%get Mesowest data
mesowest_url = strcat('http://api.mesowest.net/v2/stations/timeseries?stid=CLN&token=3d5845d69f0e47aca3f810de0bb6fd3f&start=',start_time,'&end=',end_time)

%Parse JSON data from Mesowest for use in Matlab
%matlab_results = JSON.parse(urlread(mesowest_url)); old code
matlab_results = webread(mesowest_url,'Timeout', 30)

%data_length = length(matlab_results.STATION{1}.OBSERVATIONS.date_time);
data_length = length(matlab_results.STATION.OBSERVATIONS.date_time);


for i=1:data_length
year(i) = str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(1:4));
month(i) = str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(6:7));
day(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(9:10));
hour(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(12:13));
min(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(15:16));
second(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(18:19));
end


HS =  matlab_results.STATION.OBSERVATIONS.snow_depth_set_1/10;
HourlyPrecip = matlab_results.STATION.OBSERVATIONS.precip_accum_one_hour_set_1;

%Clean up bad snow depth data; compute the standard deviation on a 6-point 
%moving average, if the standard deviation is more than 20 cm replace
%the reported snow depth with the median from the window

HS_med = HS;
for j = 1:2 %multiple passes if necessary
movWin = 7;
bad_data_count =0;
movstd_snow_depth = movstd(HS_med,movWin);
movmeedian_snow_depth = movmedian(HS_med,movWin);


for i=1:data_length
    if movstd_snow_depth(i) > 20
        HS_med(i) = movmeedian_snow_depth(i);
        bad_data_count = bad_data_count +1;
    end
end
bad_data_count %number of bad data points printed to the screen
end

SnowDepth = HS_med;

istart =1;
iend = length(year);
dtn = [year(istart:iend)',month(istart:iend)',day(istart:iend)',hour(istart:iend)',min(istart:iend)',second(istart:iend)']; %date vector
dnum = datenum(dtn); %puts date in 7.3748e+05 format

figure;
plot(dnum,SnowDepth);
datetick('x','mm/dd','keeplimits');
title('Snow Depth')
xlabel('UTC');
ylabel('Snow Depth (cm)')

figure;
plot(dnum,HourlyPrecip);
datetick('x','mm/dd','keeplimits');
xlabel('UTC');
title('Precipitation Accumulated in One Hour (mm)')
ylabel('SWE (mm/hr)')
