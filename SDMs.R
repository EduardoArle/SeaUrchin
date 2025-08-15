#load packages
library(sf); library(raster); library(sdm); library(sp)

#list WDs
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/SeaUrchin/Data'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Variables'

#load variables 
setwd(wd_variables)
minT <- rast('Present.Benthic.Max.Depth.Temperature.Min.tif')
meanT <- rast('Present.Benthic.Max.Depth.Temperature.Mean.tif')
maxT <- rast('Present.Benthic.Max.Depth.Temperature.Max.tif')
min_so <- rast('Present.Benthic.Max.Depth.Salinity.Min.asc')
mean_so <- rast('Present.Benthic.Max.Depth.Salinity.Mean.asc')
max_so <- rast('Present.Benthic.Max.Depth.Salinity.Max.asc')

#load occurrences
setwd(wd_occ)
occ <- read.csv('Diadema_setosum_pr_PA.csv')

#