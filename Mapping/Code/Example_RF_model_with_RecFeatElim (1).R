####  Working on BASF study area to predict continuous soil properties (fertility, texture, C, N, etc.)
#  set working directory for bringing in raster data
setwd("D:/Projects/Projects/2020_BASF/Dawson/basf_field_bndry_covariates")

setwd("D:/Projects/Projects/2020_BASF/Dawson")
#________________________________________________________________
# 1.0 #  Load libraries needed 

#install.packages(c('raster', 'ggplot2', 'caret', 'psych', 'car', 'Boruta', 'GSIF', 'plotKML'), dependencies = T)
library(raster)
library(ggplot2)
library(caret)
library(psych)
library(car)
library(Boruta)
library(maptools)

# Lins CCC from https://search.r-project.org/CRAN/refmans/DescTools/html/CCC.html 
library(DescTools)

# For calculation of Bias from: https://www.rdocumentation.org/packages/Metrics/versions/0.1.4/topics/bias
library(Metrics)


#________________________________________________________________
# 2.0 #  Load raster covariate layers
list.files()

________________________________________________________
# 2.1 #  Load the topographic attributes, EC, and EVI
# 1 meter

# directory is "D:/Projects/Projects/2020_BASF/Dawson/basf_field_bndry_covariates"
An.Hillshade <- raster("AnalyticalHillshading_basfbndry2.tif" , band=1)
TASM.ClsV1 <- raster("BASF_TASM_ClassV1_basfbndry2.tif", band=1)
Conv.Index <- raster("ConvergenceIndex_basfbndry2.tif", band=1)
Insol.AprOct <- raster("DirectInsolationApril-Oct_basfbndry.tif", band=1)
EC.SH <- raster("EC_SH_nad83utm16n_basfbndry2.tif" , band=1)
EC.DP <- raster("EC_DP_nad83utm16n_basfbndry2.tif", band=1)
EVI.2009 <- raster("Enhanced Vegetation Index_20090720_basfbndry2.tif", band=1)
EVI.2010 <- raster("Enhanced Vegetation Index_20100905_basfbndry2.tif", band=1)
Geomophons <- raster("Geomorphons_basfbndry2.tif", band=1)
LS.5gauss <- raster("LS Factor_5Gauss_basfbndry2.tif", band=1)
LS.2.5gauss <- raster("LS Factor2.5Gauss_basfbndry2.tif", band=1)
MRRTF <- raster("Multi-resolution_ridge_top_flatness_basfbndry2.tif", band=1)
TPI <- raster("Multi-resolution_topographic_position_index(TPI)_basfbndry2.tif", band=1)
MRVBF <- raster("Multi-resolution_valley_bottom_flatness_basfbndry2.tif", band=1)
Open.neg <- raster("Negative Openness_5gauss_basfbndry2.tif", band=1)
Open.pos <- raster("Positive Openness_5gaus_basfbndry2.tif", band=1)
plan.curv <- raster("Plan Curvature_basfbndry2.tif", band=1)
porf.curv <- raster("Profile Curvature_basfbndry2.tif", band=1)
Rel.slope.pos <- raster("RelativeSlopePosition_basfbndry2.tif", band=1)
slope.ZT <- raster("Slope_ZT_basfbndry2.tif", band=1)
SWIE4_Gauss5 <- raster("SWIE4_Gauss5_BASF_basfbndry2.tif", band=1)
tan.curv <- raster("Tangential Curvature_basfbndry2.tif", band=1)
TRI <- raster("TerrainRuggednessIndex(TRI)_basfbndry2.tif", band=1)
TWI <- raster("TWI_MFD1.1_ZT_gauss5_basfbndry2.tif", band=1)

order= xmin, xmax, ymin, ymax


extent(raster::resample(Geomophons,EVI.2009))

________________________________________________________
# 2.2 #  Load the top PCA layers from imagery (Drone and 8-band Planet)
# 1 meter
setwd("D:/Projects/Projects/2020_BASF/Dawson/Data-20220621T135230Z-001/Data/rasters/sampled to 1m and aligned")
# directory is "D:/Projects/Projects/2020_BASF/Dawson/Data-20220621T135230Z-001/Data/rasters/sampled to 1m and aligned"
list.files()
# Here need to add PCA layers from imagery
drone.pc1 <- raster("PCA_4band_Drone_Dawson_nad83utm16n.tif", band=1)
drone.pc2 <- raster("PCA_4band_Drone_Dawson_nad83utm16n.tif", band=2)
drone.pc3 <- raster("PCA_4band_Drone_Dawson_nad83utm16n.tif", band=3)
planet.pc1 <- raster("PCA_8band_Planet_Dawson_nad83utm16n.tif", band=1)
planet.pc2 <- raster("PCA_8band_Planet_Dawson_nad83utm16n.tif", band=2)
planet.pc3 <- raster("PCA_8band_Planet_Dawson_nad83utm16n.tif", band=3)
planet.pc4 <- raster("PCA_8band_Planet_Dawson_nad83utm16n.tif", band=4)

#________________________________________________________________

# ________________________________________________________
# 2.3 #  Resample all variables to the smallest extent variable (EVI.2009 and EVI.2010) 

An.Hillshade <- raster::resample(An.Hillshade,EVI.2009)
TASM.ClsV1 <- raster::resample(TASM.ClsV1,EVI.2009)
Conv.Index <- raster::resample(Conv.Index,EVI.2009)
Insol.AprOct <- raster::resample(Insol.AprOct,EVI.2009)
EC.SH <- raster::resample(EC.SH,EVI.2009)
EC.DP <- raster::resample(EC.DP,EVI.2009)
# EVI.2009 
# EVI.2010  
Geomophons <- raster::resample(Geomophons,EVI.2009)
LS.5gauss <- raster::resample(LS.5gauss,EVI.2009)
LS.2.5gauss <- raster::resample(LS.2.5gauss,EVI.2009)
MRRTF <- raster::resample(MRRTF,EVI.2009)
TPI <- raster::resample(TPI,EVI.2009)
MRVBF <- raster::resample(MRVBF,EVI.2009)
Open.neg <- raster::resample(Open.neg,EVI.2009)
Open.pos <- raster::resample(Open.pos,EVI.2009)
plan.curv <- raster::resample(plan.curv,EVI.2009)
porf.curv <- raster::resample(porf.curv,EVI.2009)
Rel.slope.pos <- raster::resample(Rel.slope.pos,EVI.2009)
slope.ZT <- raster::resample(slope.ZT,EVI.2009)
SWIE4_Gauss5 <- raster::resample(SWIE4_Gauss5,EVI.2009)
tan.curv <- raster::resample(tan.curv,EVI.2009)
TRI <- raster::resample(TRI,EVI.2009)
TWI <- raster::resample(TWI,EVI.2009)
# Now PC layers
drone.pc1 <- raster::resample(drone.pc1,EVI.2009)
drone.pc2 <- raster::resample(drone.pc2,EVI.2009)
drone.pc3 <- raster::resample(drone.pc3,EVI.2009)
planet.pc1 <- raster::resample(planet.pc1,EVI.2009)
planet.pc2 <- raster::resample(planet.pc2,EVI.2009)
planet.pc3 <- raster::resample(planet.pc3,EVI.2009)
planet.pc4 <- raster::resample(planet.pc4,EVI.2009)



# ________________________________________________________
# create a brick of all 31 covariates
cov.brick <- brick(
  An.Hillshade, 
  TASM.ClsV1, 
  Conv.Index , 
  Insol.AprOct , 
  EC.SH , 
  EC.DP , 
  EVI.2009,  
  EVI.2010  , 
  Geomophons , 
  LS.5gauss , 
  LS.2.5gauss , 
  MRRTF , 
  TPI , 
  MRVBF , 
  Open.neg,  
  Open.pos , 
  plan.curv , 
  porf.curv , 
  Rel.slope.pos, 
  slope.ZT , 
  SWIE4_Gauss5,  
  tan.curv , 
  TRI , 
  TWI , 
  drone.pc1,  
  drone.pc2 , 
  drone.pc3 , 
  planet.pc1 , 
  planet.pc2 , 
  planet.pc3 , 
  planet.pc4 )

names(cov.brick)
names(cov.brick) <- 
  c( "An.Hillshade", 
     "TASM.ClsV1", 
     "Conv.Index" , 
     "Insol.AprOct" , 
     "EC.SH" , 
     "EC.DP" , 
     "EVI.2009",  
     "EVI.2010"  , 
     "Geomophons" , 
     "LS.5gauss" , 
     "LS.2.5gauss" , 
     "MRRTF" , 
     "TPI" , 
     "MRVBF" , 
     "Open.neg",  
     "Open.pos" , 
     "plan.curv" , 
     "porf.curv" , 
     "Rel.slope.pos", 
     "slope.ZT" , 
     "SWIE4_Gauss5",  
     "tan.curv" , 
     "TRI" , 
     "TWI" , 
     "drone.pc1",  
     "drone.pc2" , 
     "drone.pc3" , 
     "planet.pc1" , 
     "planet.pc2" , 
     "planet.pc3" , 
     "planet.pc4")

plot(cov.brick)
plot(cov.brick[[1:16]])
plot(cov.brick[[17:31]])

# ________________________________________________________
# 2.4 #  Load point shapefile with all lab data joined 

#  set working directory for bringing in points with lab data
setwd("D:/Projects/Projects/2020_BASF/Dawson/Data-20220621T135230Z-001/Data/GIS/Soil_Point_Data")
list.files()
basf.points <- readShapePoints( "boxRand120_Merge_w_lab_data_nad83utm16n.shp"  )

plot(basf.points)
plot(basf.points[ 1:100 , ])
plot(basf.points[ 101:120 , ], col='red', add=T)
plot(basf.points[ 101:120 , ], col='red')


plot(basf.points[ 1:100 , ], col='blue', add=T)



str(basf.points)
summary(basf.points)

# Need to make the continuous variable numeric
basf.points$LBC_0 <- as.numeric(basf.points$LBC_0)
basf.points$LBCeq_0   <- as.numeric(basf.points$LBCeq_0  )
basf.points$Ca_0   <- as.numeric(basf.points$Ca_0  )
basf.points$LBC_10      <- as.numeric(basf.points$LBC_10     )
basf.points$LBCeq_10   <- as.numeric(basf.points$LBCeq_10  )
basf.points$Ca_10       <- as.numeric(basf.points$Ca_10      )
basf.points$Zn_10       <- as.numeric(levels(basf.points$Zn_10))[basf.points$Zn_10]
basf.points$LBCeq_10   <- as.numeric(basf.points$LBCeq_10  )

summary(basf.points)


# ________________________________________________________
# 3.0 #  Extract covariate data to point shapefile with all lab data joined 

extract.cov <- raster::extract(cov.brick,basf.points     )
str(extract.cov) # this worked
names(extract.cov)
class(extract.cov)
head(extract.cov)
tail(extract.cov)


# cbind the extracted variables and the lab data

head(cbind(basf.points,extract.cov))
all.data <- cbind(basf.points,extract.cov)
head(all.data)      

# remove columns that have no usable information (date, duplicate ID, etc.)
names(all.data)
names(all.data[ , -c(1:7,30)])

# ________________________________________________________
# 3.1 #  # Look at correlations
# install.packages('corrplot')
library(corrplot)

res2 <- cor(all.data@data[ , -c(1:7,30)])
round(res2, 2)


corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90)
testRes = cor.mtest(all.data@data[ , -c(1:7,30)], conf.level = 0.95)

corrplot(res2, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 5, hclust.method = "complete") 
corrplot(res2, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 5, hclust.method = "ward") 
corrplot(res2, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 5, hclust.method = "centroid") 

corrplot(res2, p.mat = testRes$p, sig.level = 0.05, order = 'original', addrect = 2, type = "upper") 

summary(all.data@data)

# Convert the geomorphons variable to a factor because the current numeric values from 0 to 8 are categories
all.data@data$Geomophons   <- as.factor(all.data@data$Geomophons)

# ________________________________________________________
# 4.0 #  # Prepare data for developing machine learning models
# subset data for modeling
# set working directory for where I want the output to go
setwd("D:/Projects/Projects/2020_BASF/Dawson/model_output")
set.seed(667)
# subset the dataframe to just be the variables of interest with 
# training <- sample(nrow(all.data@data), 0.8 * nrow(all.data@data))
cDat <- all.data@data[ 1:100 ,]  # use the 100 samples designed for modeling to train
vDat <- all.data@data[ 101:120 , ]  # use the 20 independent design samples for validation
str(vDat)


#______________________________________________________-
#Create a Empty DataFrame with 0 rows and n columns to hold the performance info for the 10-fold cross validation repeated 10 times per the trainControl statement

performance.df.crosval <- data.frame()
columns = c("RMSE","Rsquared","MAE", "property") 
performance.df.crosval = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(performance.df.crosval) = columns
#______________________________________________________-

#Create a Empty DataFrame with 0 rows and n columns to hold the performance info for the predictions from the final model (100 train samples and 20 validation samples)

performance.df.final <- data.frame()
columns = c("RMSE","Rsquared","MAE", "CCC", "Bias", "property") 
performance.df.final = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(performance.df.final) = columns
#______________________________________________________-



###__________________________________-

# Using code from the SSRG San Bernardino soil map disaggretation model

# 10.0 #  ###  MODEL TRAINING

library(caret)


# fitControl <- trainControl(method = "repeatedcv", 
# number = 10, 
# p = 0.7, #30% (99 observations) used for test set, 70% (232) used for training set
# selectionFunction = 'oneSE', 
# classProbs = T,
# savePredictions = T, 
# returnResamp = 'final')

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


################
# SILT
# subset df to just clay 0-10 and the predictors
silt <- cDat[ , c(32,38:68)]

silt.valid <- vDat[ , c(32,38:68)]

#####_____________________________________
#________________________________________________________________
# 9.5 #  ###  Recursive Feature Selection (RFE) using raw covariate data with NA value rows of component removed
#________________________________________________________________

set.seed(9)

ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verbose = FALSE) 

#The simulation will fit models with subset sizes of 31,30,29,.....1. 
subsets <- c(1:31) 

rf.RFE.silt.0 <- rfe(x = silt[,-1],
                     y =  silt$SILT_0 ,
                     sizes = subsets,
                     rfeControl = ctrl.RFE,
                     allowParallel = TRUE,
                     preProc = c("center", "scale"))


#  Look at the results  # This picked 3 variables to develop the model with
rf.RFE.silt.0

plot(rf.RFE.silt.0)
# str(rf.RFE.silt.0)
# 9.5.1.2 #  ###  Variables selected using RFE, put into a formula for easy modelling later. 
names((silt[,c(predictors(rf.RFE.silt.0))]))



# try out the rf model using the RFE selected variables to compare to the original model
#________________________________________________
# All 100 training samples
RFE.train.all.silt.0 <- silt[,c("SILT_0", predictors(rf.RFE.silt.0))]

set.seed(4801)
rfFit.100.silt.0.rfe = train(SILT_0 ~ ., data=RFE.train.all.silt.0, "rf", trControl = fitControl, tuneLength=10, metric = 'RMSE')
rfFit.100.silt.0.rfe # This provides a summary of the RF model that we just made
#  str(rfFit.100.silt.0.rfe)
plot(rfFit.100.silt.0.rfe)	

pred.rfFit.100.SILT_0.rfe <- predict(object = cov.brick[[c(names((silt[,c(predictors(rf.RFE.silt.0))])))]]
                                     , model=rfFit.100.silt.0.rfe) 


# my.test.brick <-  cov.brick[[c(names((silt[,c(predictors(rf.RFE.silt.0))])))]]
#  plot(my.test.brick)

#  pred.rfFit.100.SILT_0.rfe <- predict(rfFit.100.silt.0.rfe , type = "raw", na.action = na.omit , object = my.test.brick ) 


plot(pred.rfFit.100.SILT_0.rfe, main ="SILT 0-10 cm - RF model (n=100) w/ RFE")


#####

#  pred.rfFit.100.SILT_0.rfe <- predict(object = c.valid , model=rfFit.100.silt.0.rfe ) 

pred.rfFit.20.SILT_0.rfe <- predict(rfFit.100.silt.0.rfe,  newdata = silt.valid  ) 
names(silt.valid)
silt.valid[ , 1]

plot(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1] )
CCC(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1], ci = "z-transform", conf.level = 0.95)

# Use final model to predict value for 100 training points
pred.rfFit.100.SILT_0.rfe.train <- predict(rfFit.100.silt.0.rfe,  newdata = RFE.train.all.silt.0  ) 


#  Model performance summary
# this is the performance metrics for the training dataset
performance.df.crosval <- (rbind(performance.df.crosval ,  data.frame (rfFit.100.silt.0.rfe$results[rfFit.100.silt.0.rfe$results$mtry == rfFit.100.silt.0.rfe$finalModel$mtry,][ , 2:4] , property = paste("silt.train.0.RF")) )) 
# computing model performance metrics for validation dataset of n=20
performance.df.final <- (rbind(performance.df.final , data.frame(RMSE = RMSE(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1]),
                                                                 Rsquared = R2(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1]),
                                                                 MAE = MAE(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1]),
                                                                 CCC = CCC(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1], ci = "z-transform", conf.level = 0.95)$rho.c$est,
                                                                 Bias = bias(pred.rfFit.20.SILT_0.rfe, silt.valid[ , 1]),
                                                                 property = paste("silt.valid.0.RF")) ))

performance.df.final <- (rbind(performance.df.final , data.frame(RMSE = RMSE(pred.rfFit.100.SILT_0.rfe.train, silt[ , 1]),
                                                                 Rsquared = R2(pred.rfFit.100.SILT_0.rfe.train, silt[ , 1]),
                                                                 MAE = MAE(pred.rfFit.100.SILT_0.rfe.train, silt[ , 1]),
                                                                 CCC = CCC(pred.rfFit.100.SILT_0.rfe.train, silt[ , 1], ci = "z-transform", conf.level = 0.95)$rho.c$est ,
                                                                 Bias = bias(pred.rfFit.100.SILT_0.rfe.train, silt[ , 1]),
                                                                 property = paste("Silt.train.0.RF.final")) ))
plot(pred.rfFit.100.SILT_0.rfe.train, silt[ , 1])


