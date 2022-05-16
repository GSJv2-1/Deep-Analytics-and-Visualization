G.Johnson
C4T3
Mar-08-2022

#Load libraries
library(readr) #read csv files
library(plyr) #change values
library(caret) #predictions
library(dplyr) #needed to explore data
library(explore) #runs EDA
library(corrplot) # needed to plot correlations
library(doParallel) # Parallel Processing
library(C50) # C5.0 Model for Classification
library(kknn) # k-Nearest Neighbors for Classification
library(randomForest) # Random Forest for Classification
library(earth) # Flexible Discriminant Analysis
library(gbm) # Stochastic Gradient Boosting
library(deepnet) # Stacked AutoEncoder Deep Neural Network
library(xgboost)
library(kerndwd) # Distance Weighted Discrimination with Polynomial Kernel

#Increase processing speed (Parallel Processing)
detectCores() # Result = 8
#Create Cluster with desired number of cores. NEVER use all cores
c1 <- makeCluster(4)
#Register Cluster
registerDoParallel(c1)
#Confirm how many cores are "assigned" to R and RStudio
getDoParWorkers() # Result 6
#Stop Cluster. AFTER performing your tasks registerDoSEQ()
#stopCluster(c1)

#Re-ordered columns within Excel so Longitude,Latitude,Floor,Buildingid,Spaceid,
#Relativeposition,Userid,Phoneid,Timestamp come before WAP***

#load dataframe
IndoorLocDF <- read_csv("trainingData1.csv")
#What is the dataframe
attributes(IndoorLocDF)
summary(IndoorLocDF)
str(IndoorLocDF)
names(IndoorLocDF)

head(IndoorLocDF)
class(IndoorLocDF)

#Check data for missing values
is.na(IndoorLocDF)

#Use Explore to generate EDA
explore(IndoorLocDF)

#Create subset to focus on only Building 2 which is 47.6% of all observations
IDF <- filter(IndoorLocDF, BUILDINGID == 2)

# Create a single unique identifier for each location
#IndoorLocDF <- cbind(IndoorLocDF, paste(IndoorLocDF, IndoorLocDF$BUILDINGID, IndoorLocDF$FLOOR, IndoorLocDF$SPACEID, IndoorLocDF$RELATIVEPOSITION, sep= "_")) #Works
#IDF <- cbind(IDF, paste(IDF, IDF$FLOOR, IDF$SPACEID, IDF$RELATIVEPOSITION, sep= "_")) #Doesn't work, creates illegible junk
IDF <- within(IDF, LOCATION <- paste(FLOOR, SPACEID, RELATIVEPOSITION, sep = "_"))

# Give new attribute in the 530th column a new name
#colnames(IndoorLocDF) [530] <- "LOCATION" #No longer needed
#colnames(IDF) [530] <- "LOCATION" #No longer needed

# Convert LOCATION data type to factor
IDF$LOCATION <- as.factor(IDF$LOCATION)
str(IDF$LOCATION)
attributes(IDF$LOCATION)
str(IDF)

#Remove attributes that are no longer needed
IDF$LONGITUDE <- as.null(IDF$LONGITUDE)
IDF$LATITUDE <- as.null(IDF$LATITUDE)
IDF$FLOOR <- as.null(IDF$FLOOR)
IDF$BUILDINGID <- as.null(IDF$BUILDINGID)
IDF$SPACEID <- as.null(IDF$SPACEID)
IDF$RELATIVEPOSITION <- as.null(IDF$RELATIVEPOSITION)
IDF$USERID <- as.null(IDF$USERID)
IDF$PHONEID <- as.null(IDF$PHONEID)
IDF$TIMESTAMP <- as.null(IDF$TIMESTAMP)

## Reduce number of Wifi units with NearZeroVar 
nzv <- nearZeroVar(IDF, saveMetrics = T)
head(nzv)
summary(nzv) #Remove attributes with zero variance as 100 means not used

zeroVarData1 <- which(nzv$zeroVar == T)
zeroVarData1

df <- IDF[,-(zeroVarData1)]  # remove these observations from the list

#Double check dependent variable
str(df$LOCATION)

#Modeling Classification: DO NOT USE SVM/SVR or Linear Model(LM)
# Create Training and Testing 
set.seed(123)
inTraining <- createDataPartition(df$LOCATION, p = .70, list = FALSE)
training <- df[inTraining, ]
testing <- df[-inTraining, ]

# 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

#train C5.0 - type Classification model with a tuneLength = 5 
system.time({
  c50Fit1 <- train(LOCATION~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 5) #    user   system  elapsed 46.290   12.468 2687.148 
})
#train C5.0 - type Classification model with a tuneLength = 10 ### CRASHED SYSTEM
#system.time({
#  c50Fit2 <- train(LOCATION~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 10)
#})

#train k-Nearest Neighbors - type Classification model with a tuneLength = 5 
system.time({
  kknnFit1 <- train(LOCATION~., data = training, method = "kknn", trControl=fitControl, tuneLength = 5) #      user   system  elapsed 23.073    7.463 1206.149    
})

#train Random Forest - type Classification model with a tuneLength = 5 
system.time({
  rfFit1 <- train(LOCATION~., data = training, method = "rf", trControl=fitControl, tuneLength = 5) #        user   system  elapsed 94.030   25.012 7195.321  
})

#train Flexible Discriminant Analysis - type Classification model with a tuneLength = 5 
system.time({
  dwdFit1 <- train(LOCATION~., data = training, method = "fda", trControl=fitControl, tuneLength = 5) #         user   system  elapsed 159.401   19.852 4903.359   
})

#train Stochastic Gradient Boosting - type Classification model with a tuneLength = 5 
#system.time({
#  gbmFit1 <- train(LOCATION~., data = training, method = "gbm", trControl=fitControl, tuneLength = 2) #         user   system  elapsed 159.401   19.852 4903.359   Error in { : 
  #task 1 failed - "arguments imply differing number of rows: 0, 673" Timing stopped at: 8.845 11.74 4022
#})

#train eXtreme Gradient Boosting - type Classification model with a tuneLength = 5 
system.time({
  xgbFit1 <- train(LOCATION~., data = training, method = "xgbTree", trControl=fitControl, tuneLength = 1) 
})


#Results
c50Fit1
plot(c50Fit1)
kknnFit1
plot(kknnFit1)
rfFit1
plot(rfFit1)
dwdFit1
plot(dwdFit1)
xgbFit1
plot(xgbFit1)

# Test the models with Predicts
c50Pred <- predict(c50Fit1,newdata = testing)
kknnPred <- predict(kknnFit1,newdata = testing)
rfPred <- predict(rfFit1,newdata = testing)
dwdPred <- predict(dwdFit1,newdata = testing)
xgbPred <- predict(xgbFit1,newdata = testing)

#### Model Comparison

#confusionMatrix(df$LOCATION, sample(df$LOCATION))
c50CM <- confusionMatrix(c50Pred, testing$LOCATION)
c50CM

kknnCM <- confusionMatrix(kknnPred, testing$LOCATION)
kknnCM

rfCM <- confusionMatrix(rfPred, testing$LOCATION)
rfCM

dwdCM <- confusionMatrix(dwdPred, testing$LOCATION)
dwdCM

xgbCM <- confusionMatrix(xgbPred, testing$LOCATION)
xgbCM


# Evaluating models

ModelData <- resamples(list(C50 = c50Fit1, KNN = kknnFit1, RF = rfFit1, FDA = dwdFit1, XGB = xgbFit1))
summary(ModelData)
#Models: C50, KNN, RF, FDA, XGB 
#Number of resamples: 50 
#
#Accuracy 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#C50 0.6651515 0.6864442 0.6963762 0.6966992 0.7041774 0.7287630    0
#KNN 0.6038576 0.6249459 0.6354780 0.6355676 0.6464972 0.6812689    0
#RF  0.7618343 0.7983323 0.8117492 0.8101468 0.8225081 0.8470765    0
#FDA 0.2590361 0.2770716 0.2859330 0.2882077 0.2979886 0.3179104    0
#XGB 0.6928783 0.7070455 0.7151351 0.7167203 0.7250092 0.7441176    0
#
#Kappa 
#         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#C50 0.6640989 0.6854360 0.6954152 0.6957350 0.7032397 0.7279091    0
#KNN 0.6026338 0.6237856 0.6343403 0.6344228 0.6453691 0.6802694    0
#RF  0.7610752 0.7976738 0.8111378 0.8095328 0.8219332 0.8465681    0
#FDA 0.2568618 0.2748874 0.2837582 0.2860901 0.2959326 0.3159309    0
#XGB 0.6918745 0.7060872 0.7142074 0.7157920 0.7241180 0.7432960    0


