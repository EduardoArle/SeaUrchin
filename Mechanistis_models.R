#load packages
library(raster)

#list WDs
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Variables'

#load variables 
setwd(wd_variables)
minT <- raster('Present.Benthic.Max.Depth.Temperature.Min.tif')
meanT <- raster('Present.Benthic.Max.Depth.Temperature.Mean.tif')
maxT <- raster('Present.Benthic.Max.Depth.Temperature.Max.tif')

#crop variables to mediterranean extent
med_extent <- extent(-6, 36.5, 30, 46)
minT_med <- crop(minT, med_extent)
meanT_med <- crop(meanT, med_extent)
maxT_med <- crop(maxT, med_extent)

#exclude cells with lethal temperature
non_lethal <- maxT_med
non_lethal[] <- ifelse(non_lethal[] >= 36, NA, non_lethal[])

#### no cells were excluded

#identify cells with the optimal T for metabolic rates (22-28 C)
met_rates <- maxT_med
met_rates[] <- ifelse(met_rates[] < 22, 0, met_rates[])
met_rates[] <- ifelse(met_rates[] > 28, 0, met_rates[])

plot(met_rates)

#identify cells with the optimal T for gonads growth and maturation (20-24 C)
gon_rates <- maxT_med
gon_rates[] <- ifelse(gon_rates[] < 20, 0, gon_rates[])
gon_rates[] <- ifelse(gon_rates[] > 24, 0, gon_rates[])

plot(gon_rates)
