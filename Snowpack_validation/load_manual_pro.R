

library(sarp.snowprofile)
#Load validation pit
Filename <- "/Users/travismorrison/Library/CloudStorage/OneDrive-Personal/Research/Projects/UAC_SNOWPACK/validation_pits/Atwater1_26_24-26-Jan-v6.caaml"
Profile <- snowprofileCaaml(Filename)

plot(Profile)


##Load validation pit
## Empty layers object:
#snowprofileLayers()
## simple layers example that recycles the hardness 1F+: with warning issued!
## Try what happens if you provide ddate as character array without a timezone.
#snowprofileLayers(height = c(6, 24, 50),
#        hardness = char2numHHI('1F+'),
#        gtype = c('FC', NA, 'PP'),
#        ddate = as.POSIXct(c(NA, NA, "2020-02-15 10:45:00"),
 #       tz = "Etc/GMT+7"))