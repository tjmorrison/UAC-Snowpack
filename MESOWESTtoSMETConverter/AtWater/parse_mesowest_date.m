function dnum = parse_mesowest_date(matlab_results)

data_length = length(matlab_results.STATION.OBSERVATIONS.date_time);
for i=1:data_length
    year(i) = str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(1:4));
    month(i) = str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(6:7));
    day(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(9:10));
    hour(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(12:13));
    min(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(15:16));
    second(i) =  str2num(matlab_results.STATION.OBSERVATIONS.date_time{i}(18:19));
end


istart =1;
iend = length(year);
dtn = [year(istart:iend)',month(istart:iend)',day(istart:iend)',hour(istart:iend)',min(istart:iend)',second(istart:iend)']; %date vector
dnum = datenum(dtn); %puts date in 7.3748e+05 format

end