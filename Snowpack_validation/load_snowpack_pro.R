library(sarp.snowprofile)
## Import a CAAML file
Filename <- "/Users/travismorrison/OneDrive/Research/Projects/UAC_SNOWPACK/SP_Results/Atwater_2024/ATH20_10_05_2023_03_22_2024.pro"
#Profile <- snowprofilePro(Filename)

## Scan dates in file
Dates <- scanProfileDates(Filename)
print(Dates)

## Read a single profile by date and plot
ProfileDateInterest <- Dates[1765]
Profile <- snowprofilePro(Filename, ProfileDate = ProfileDateInterest)
plot(Profile)

## Read entire time series and plot
#Profiles <- snowprofilePro(Filename)
#plot(Profiles, main = 'Timeseries read from example.pro')

