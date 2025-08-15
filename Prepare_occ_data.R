#load packages
library(sf); library(terra); library(data.table)

#list WDs
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/SeaUrchin/Data'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Variables'

#load data for all sea urchins
setwd(wd_occ)
occ <- read.csv('Sea_urchins_region.csv', sep = '\t')

#split data into Diadema setosum and not Diadema setosum
occ_dia <- occ[occ$species == 'Diadema setosum',]
occ_others <- occ[occ$species != 'Diadema setosum',]

#load one variable to thin the data
setwd(wd_variables)
var <- rast(list.files()[1])

#make a polygon of the study area with same coords to download GBIF data
coords <- matrix(c(31.10389, 8.02564, 63.00973, 8.02564, 63.12492, 30.76862,
                   44.04014, 30.83969, 30.56813, 30.70241, 31.10389, 8.02564),
                 ncol = 2,  byrow = TRUE)

poly <- st_polygon(list(coords))
study_area <- st_sf(geometry = st_sfc(poly, crs = crs(var)))

#make ID raster
ID_rast <- var
non_na_cells <- which(!is.na(values(ID_rast)))
new_vals <- rep(NA, ncell(ID_rast))
new_vals[non_na_cells] <- seq_along(non_na_cells)
ID_rast[] <- new_vals
names(ID_rast) <- 'ID'

#create spat object occ
occ_dia_sf <- st_as_sf(occ_dia,
                       coords = c('decimalLongitude', 'decimalLatitude'),
                       crs = crs(var))

occ_others_sf <- st_as_sf(occ_others,
                       coords = c('decimalLongitude', 'decimalLatitude'),
                       crs = crs(var))

#extract cell ID 
ID <- extract(ID_rast, occ_dia_sf)
occ_dia_sf$ID <- ID[,2]

ID <- extract(ID_rast, occ_others_sf)
occ_others_sf$ID <- ID[,2]

#keep only cells in the ocean
occ_dia_sf2 <- occ_dia_sf[!is.na(occ_dia_sf$ID),]
occ_others_sf2 <- occ_others_sf[!is.na(occ_others_sf$ID),]

#eliminate duplicated cells in Diadema and PA
occ_dia_sf3 <- unique(as.data.table(occ_dia_sf2), by = 'ID')
occ_others_sf3 <- unique(as.data.table(occ_others_sf2),
                         by = c('species', 'ID'))

#retransform objects into sf
occ_dia_sf3 <- st_as_sf(occ_dia_sf3)
occ_others_sf3 <- st_as_sf(occ_others_sf3)

#make a 50 km buffer around presences to avoid selection pseudo-absence
dia_sf_buf <- st_buffer(occ_dia_sf3, dist = 50000)

#make a spatial polygon object with only one feature
no_pa_dia <- st_union(dia_sf_buf)

# this fixes possible 'duplicate vertex' errors
no_pa_dia <- st_make_valid(no_pa_dia) 

#make a holes in the study areas by the small buffer around points
pa_area_dia <- st_difference(study_area, no_pa_dia)
pa_area_dia <- st_make_valid(pa_area_cat)

#define number of pseudo abs to be created (same as presences)
n_pa_dia <- nrow(occ_dia_sf3)

#select candidates for pa (other points same taxon) within in the pa_area
pa_dia1 <- vapply(st_intersects(occ_others_sf3, pa_area_dia), 
                  function(x) if (length(x)==0) NA_integer_ else x[1],
                  FUN.VALUE = 1)

pa_dia2 <- occ_others_sf3[!is.na(pa_dia1),]

#randomly select n pa points amongst the occ of other urchins
pa_dia3 <- pa_dia2[sample(c(1:nrow(pa_dia2)), n_pa_dia),]

#visualise PA are
plot(st_geometry(pa_area_dia), col = 'orange')
plot(occ_dia_sf3, add = T, col = '#000000', bg = '#30A530', pch = 21, cex = 0.4)

#visualise pres and PA
plot(st_geometry(study_area), border = NA, col = '#d5d5d5')
plot(ID_rast, add = T)
plot(occ_dia_sf3, add = T, col = '#30A530', pch = 19, cex = 0.4)
plot(pa_dia3, add = T, col = '#d58920', pch = 19, cex = 0.4)

#prepare tables to save
coords_dia <- as.data.frame(st_coordinates(occ_dia_sf3)) #get coordinates
coords_PA <- as.data.frame(st_coordinates(pa_dia3)) #get coordinates

names(coords_dia) <- c('decimalLongitude', 'decimalLatitude')
names(coords_PA) <- c('decimalLongitude', 'decimalLatitude')

occ_dia_sf4 <- cbind(as.data.frame(occ_dia_sf3), coords_dia)
pa_dia4 <- cbind(as.data.frame(pa_dia3), coords_PA)

#create col with occurrence status
occ_dia_sf4$occurrence <- 1
pa_dia4$occurrence <- 0

#join pr and PA
dia_pr_PA <- rbind(occ_dia_sf4, pa_dia4)

#save presence and pseudo-absences
setwd(wd_occ)

write.csv(dia_pr_PA,
          'Diadema_setosum_pr_PA.csv',
          row.names = F)


