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
so <- rast('Present.Benthic.Max.Depth.Salinity.Mean.asc')

#load occurrences
setwd(wd_occ)





cat_pres <- read.csv("Prionailurus bengalensis_clean_thin.csv")
cat_pa <- read.csv("Prionailurus bengalensis_pseudoabsence.csv")

plant_pres <- read.csv("Zamia prasina_clean_thin.csv")
plant_pa <- read.csv("Zamia prasina_pseudoabsence.csv")

#harmonise the occurrence table with the requirements of the package
cat_pres2 <- data.frame(occurrence = 1,
                        lon = cat_pres$decimalLongitude,
                        lat = cat_pres$decimalLatitude)
