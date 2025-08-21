#load packages
library(sf); library(raster); library(sdm); library(sp); library(pROC)
library(PresenceAbsence)

#list WDs
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Data'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Variables'
wd_SDM <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Results'
wd_proj <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Iris/Projections_native'

#load variables 
setwd(wd_variables)
maxT <- raster('Present.Benthic.Max.Depth.Temperature.Max.tif')
min_so <- raster('Present.Benthic.Max.Depth.Salinity.Min.asc')

#stack variablrs
vars <- stack(maxT, min_so)

#crop vars by native study area
coords <- matrix(c(31.10389, 8.02564, 63.00973, 8.02564, 63.12492, 30.76862,
                   44.04014, 30.83969, 30.56813, 30.70241, 31.10389, 8.02564),
                 ncol = 2, byrow = TRUE)

poly <- Polygon(coords)
poly2<- Polygons(list(poly), 1)
poly3 <- SpatialPolygons(list(poly2), proj4string = crs(vars))

r_crop <- crop(vars, poly3)   # crops to bounding box
vars_crop <- mask(r_crop, poly3)  # masks outside polygon

#crop vars by study area
coords_med <- matrix(c(-15, 30, 46.5, 30, 46.5, 46, -15, 46, -15, 30),  
                 ncol = 2, byrow = TRUE)

poly_med <- Polygon(coords_med)
poly2_med <- Polygons(list(poly_med), 1)
poly3_med <- SpatialPolygons(list(poly2_med), proj4string = crs(vars))

r_crop_med <- crop(vars, poly3_med)   # crops to bounding box
vars_crop_med <- mask(r_crop_med, poly3_med)  # masks outside polygon




#load occurrences
setwd(wd_occ)
occ <- read.csv('Diadema_setosum_pr_PA.csv')

#create a spatial points data frame (sp)
occ_sp <- occ
coordinates(occ_sp) <- ~ decimalLongitude + decimalLatitude

#inform the geographic system
proj4string(occ_sp) <- crs(vars)

#extract values from all points
vals <- as.data.frame(extract(vars, occ_sp))

#prepare data frame
vals2 <- cbind(occ$occurrence, vals)

#fix col names
names(vals2)[1] <- 'occurrence'

#prepare data object
data <- sdmData(formmula = occurrence ~ . + coords(decimalLongitude+decimalLatitude),
                 train = vals2)

#save sdmData objects
setwd(wd_SDM)
write.sdm(data, 'sdmData_Diadema_setosum')

#run models
sdm <- sdm(occurrence ~ ., data = data,
           methods = c('brt', 'cart', 'fda', 'glm', 'mars', 'maxlike',
                       'mda', 'gam', 'rf', 'svm'), 
           replication = 'cv', cv.folds = 5, n = 5)

#save model objects
setwd(wd_SDM)
write.sdm(sdm, 'models_Diadema_setosum')

#visualise in gui
gui(sdm)

#get model evaluation
eval_model <- getEvaluation(sdm, w = 1:250,
                             wtest='test.dep', 
                             stat=c('AUC','TSS','th'), opt = 2)

#include column informing algorithm
eval_model$Algorithm <- NA
eval_model$Algorithm[1:25] <- 'brt'
eval_model$Algorithm[26:50] <- 'cart'
eval_model$Algorithm[51:75] <- 'fda'
eval_model$Algorithm[76:100] <- 'glm'
eval_model$Algorithm[101:125] <- 'mars'
eval_model$Algorithm[126:150] <- 'maxlike'
eval_model$Algorithm[151:175] <- 'mda'
eval_model$Algorithm[176:200] <- 'gam'
eval_model$Algorithm[201:225] <- 'rf'
eval_model$Algorithm[226:250] <- 'svm'

#select models with TSS higher than 0.5 and AUC higher than 0.7
sel_models <- eval_model[which(eval_model$TSS >= 0.5 & eval_model$AUC >= 0.7),]

#project all selected models
#project models
setwd(wd_proj)

pred_pr <- list()
for(i in 1:250)
{
  pred_pr[[i]] <- predict(sdm, id = i, newdata = vars_crop, 
                          filename = paste0('Pred_Diadema_', i, '.grd'))
}

#load models
setwd(wd_proj)

#load selected projections
dia_pr <- list()
for(i in 1:nrow(sel_models))
{
  dia_pr[[i]] <- raster(paste0('Pred_Diadema_', sel_models$modelID[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
dia_pr_bin <- list()
for(i in 1:length(dia_pr))
{
  dia_pr_bin[[i]] <- dia_pr[[i]]
  th <- sel_models$threshold[i]
  dia_pr_bin[[i]][] <- ifelse(dia_pr_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
dia_pr_bin <- stack(dia_pr_bin)

#sum all layers and calculate percentage of agreement
dia_pr_ens <- sum(dia_pr_bin) / nlayers(dia_pr_bin) * 100

plot(dia_pr_ens, main = 'Present')

#calculate metrics of the ensemble

#extract precictions in locations of occurrences
pred <- extract(dia_pr_ens, occ_sp) / 100

##  AUC  ##
roc_obj <- roc(occ$occurrence, pred)
auc_val <- roc_obj$auc
print(auc_val)

##  TSS  ##

# TSS = Sensitivity + Specificity - 1
# We need to choose a threshold to convert probabilities to binary predictions

# Use PresenceAbsence package to calculate across thresholds
df <- data.frame(ID = 1:nrow(occ), obs = occ$occurrence, pred = pred)

# Calculate accuracy measures for many thresholds
acc <- presence.absence.accuracy(df, threshold = seq(0, 1, 0.01))

# Find threshold with maximum TSS
best_row <- acc[which.max(acc$sensitivity + acc$specificity - 1), ]
tss_val <- best_row$sensitivity + best_row$specificity - 1

print(tss_val)
print(best_row$threshold)  # threshold that maximizes TSS

# calculate means of models used for ensemble
mean(sel_models$AUC[sel_models$Algorithm == 'brt'])
mean(sel_models$AUC[sel_models$Algorithm == 'cart'])
mean(sel_models$AUC[sel_models$Algorithm == 'fda'])
mean(sel_models$AUC[sel_models$Algorithm == 'glm'])
mean(sel_models$AUC[sel_models$Algorithm == 'mars'])
mean(sel_models$AUC[sel_models$Algorithm == 'maxlike'])
mean(sel_models$AUC[sel_models$Algorithm == 'mda'])
mean(sel_models$AUC[sel_models$Algorithm == 'gam'])
mean(sel_models$AUC[sel_models$Algorithm == 'rf'])
mean(sel_models$AUC[sel_models$Algorithm == 'svm'])

mean(sel_models$TSS[sel_models$Algorithm == 'brt'])
mean(sel_models$TSS[sel_models$Algorithm == 'cart'])
mean(sel_models$TSS[sel_models$Algorithm == 'fda'])
mean(sel_models$TSS[sel_models$Algorithm == 'glm'])
mean(sel_models$TSS[sel_models$Algorithm == 'mars'])
mean(sel_models$TSS[sel_models$Algorithm == 'maxlike'])
mean(sel_models$TSS[sel_models$Algorithm == 'mda'])
mean(sel_models$TSS[sel_models$Algorithm == 'gam'])
mean(sel_models$TSS[sel_models$Algorithm == 'rf'])
mean(sel_models$TSS[sel_models$Algorithm == 'svm'])

### AUC ensemble = 0.9155

### AUC brt = 0.8176
### AUC cart = 0.7937
### AUC fda = 0.7981
### AUC glm = 0.798
### AUC mars = 0.7957
### AUC maxlike = 0.7824
### AUC mda = 0.7682
### AUC gam = 0.7923
### AUC rf = 0.8027
### AUC svm = 0.8034

### TSS ensemble = 0.6889

### TSS brt = 0.5881
### TSS cart = 0.5830
### TSS fda = 0.6093
### TSS glm = 0.6047
### TSS mars = 0.5635
### TSS maxlike = 0.5446
### TSS mda = 0.5641
### TSS gam = 0.5945
### TSS rf = 0.5777
### TSS svm = 0.6058
