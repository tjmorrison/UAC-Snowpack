%Use JSON parser to put data for DAQ MWR into a format like excel
%Data are pulled from MESOWEST synoptic server
clear all;
close all;
%synoptic_search = 'https://asn.synoptic.io/api/v1/series?senabbr=DEQMR&variables=ANGLE_A_TEMP,ANGLE_A_RH&End=1422378168&Begin=1422370168&token=sdfdsfa';
synoptic_search = 'https://asn.synoptic.io/api/v1/series?senabbr=DEQMR&variables=ANGLE_A_VAPDEN,ANGLE_A_TEMP,ANGLE_A_RH&End=1422378168&Begin=1422370168&token=sdfdsfa';

matlab_results = parse_json(urlread(synoptic_search));

disp(matlab_results{1})

time_unix = cell2mat(matlab_results{1}.DATTIM); % epoch time 
time_matlab =  datenum([1970,1,1,0,0,0]) + time_unix/86400;
time_matlab_string = datestr(time_matlab, 'yyyymmdd HH:MM:SS.FFF');

height = cell2mat(matlab_results{1}.HEIGHT)'; %heights above ground
for i=1:length(matlab_results{1}.ANGLE_A_RH) %loop through time
    for j=1:length(height) 
        RH(i,j) = cell2mat(matlab_results{1}.ANGLE_A_RH{i}(j))'; %ANGLE_A_RH
        Temperature(i,j) = cell2mat(matlab_results{1}.ANGLE_A_TEMP{i}(j))'; %ANGLE_A_TEMP
        vapor_density(i,j) = cell2mat(matlab_results{1}.ANGLE_A_VAPDEN{i}(j))'; %ANGLE_A_VAPDEN
    end

end

figure;
plot(RH(1,:)',height)
title(strcat('Relative Humidity ',time_matlab_string(1,:)))
xlabel('RH(%)')
ylabel('z(m)')

figure;
plot(Temperature(1,:)',height)
title(strcat('Temperature ', time_matlab_string(1,:)))
xlabel('Temperature(K)')
ylabel('z(m)')
figure;
plot(vapor_density(1,:)',height)
title(strcat('Vapor Density ',time_matlab_string(1,:)))
xlabel('\rho_v (g m^{-3})')
ylabel('z(m)')

