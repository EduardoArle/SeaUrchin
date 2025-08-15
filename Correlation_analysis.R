#load packages
library(sf); library(terra); library(usdm)

#list WDs
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/SeaUrchin/Data'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Variables'

#load variables
setwd(wd_variables)
vars <- lapply(list.files(), rast)

#make a raster stack
vars_stack <- rast(vars)

#load points
setwd(wd_occ)
dia_occ <- read.csv('Diadema_setosum_pr_PA.csv')

#make sf objects 
dia_occ_sf <- st_as_sf(dia_occ, 
                       coords = c('decimalLongitude', 'decimalLatitude'),
                       crs = crs(vars))

#extract variable values in all point locations
dia_vars <- extract(vars_stack, dia_occ_sf)

#calculate the VIF to select less correlated variables
dia_VIF <- vifcor(dia_vars[,-1], th = 0.7)         #[,-1] ID col

#print screen results and save in a folder

## test if min_sa and maxT are of to use

cor(dia_vars$Present.Benthic.Max.Depth.Salinity.Min,
    dia_vars$Present.Benthic.Max.Depth.Temperature.Max)

### OK 