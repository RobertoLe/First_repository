# Descripción: Doing classification with wifi locations in C4T3
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-15
##########################D#####################################################
library(tidyr)
library(dplyr)
library(caret)

library(ggplot2)
library(lattice)
library(C50)
library(kernlab)
library(mlbench)
library(randomForest)
library(caretEnsemble)
library(MASS)
library(klaR)
library(nnet)

data <- read.csv('R Data Sets/UJIndoorLoc/trainingData.csv')

# Meade the names of columns lower case
names(data) <- tolower(names(data))
names(data)

# do Parallel cores ######################################################################
# Required
library(doParallel)

# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# Stop Cluster. After performing your tasks, stop your cluster. 
# stopCluster(cl)

# First EDA ######################################################################

str(data)
summary(data[,1:5])

# cop <- data

# cop$BUILDINGID <- as.factor(cop$BUILDINGID)
# cop$floor <- as.factor(cop$floor)

# summary(cop$floor)

# cop1 <- filter(cop, buildingid == 2)
# summary(cop1$floor)
# attributes(cop1)
# nrow(cop1)
# summary(cop1[,1:5])
# cop1 <- filter(cop1, floor == 1)
# dim(cop1)

# summary(cop$BUILDINGID)
#  0    1    2 
# 5249 5196 9492 
# half of the data is from building 2

# Sample ######################################################################

# We sample the data to the building 2 because it is half the data
data1 <- filter(data, buildingid == 2 )

summary(data1[,1:10])

names(data1)

str(data1)

# Focus on building 2 on the 1st floor
floor1 <- filter(data1, floor == 1)
names(floor1)
str(floor1)

# Combine to make response var ######################################################################

# Combine the floor,spaceid, and relativeposition to make the response variable
floor1 <- cbind(floor1,paste(floor1$floor,floor1$spaceid,floor1$relativeposition, sep = '_'), stringsAsFactors=FALSE)
colnames(floor1)[530] <- "y"

## Move the Y attribute within the dataset
floor1 <- floor1[,c(ncol(floor1), 1:(ncol(floor1)-1))]
head(floor1)

floor1$y <- as.factor(floor1$y)

str(floor1)

# Feature selection ######################################################################

# Won't be using all these features for the analysis
floor1$longitude <- NULL
floor1$latitude <- NULL
floor1$floor <- NULL
floor1$buildingid <- NULL
floor1$spaceid <- NULL
floor1$relativeposition <- NULL
floor1$userid <- NULL
floor1$phoneid <- NULL
floor1$timestamp <- NULL


#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, percentage unique, zero variance and near zero variance 

nzvMetrics <- nearZeroVar(floor1, saveMetrics = TRUE)
head(nzvMetrics)
summary(nzvMetrics) # attributes can be removed for zero variance

zeroVariance1 <- which(nzvMetrics$zeroVar == T)
zeroVariance1

# remove variables with zero variance
df <- floor1[,-(zeroVariance1)] 

# just checking the response variable
str(df$y)

# Split for train/test ######################################################################
set.seed(1492)

inTraining <- createDataPartition(df$y, p =.70, list = FALSE)
training <- df[inTraining,]
testing <- df[-inTraining,]

# checking the response var
str(training$y)


# cross validation

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# Models ######################################################################

# C5.0 simple decision tree model
system.time({
c5fit1 <- train(y~., data = training, 
                 method = "C5.0", 
                 trControl=fitControl,
                 tuneLength = 5)
}) # elapsed: 161.793 

c5fit1
summary(c5fit1)
c5fit1$finalModel$tuneValue

testPredc501 <- predict(c5fit1, testing)
postResample(testPredc501, testing$y)
# Accuracy     Kappa 
# 0.7653543 0.7621860 

cmc51 <- confusionMatrix(testPredc501, testing$y)
cmc51

# KNN requires variables to be normalized or scaled, so it needs pre-processing
# trainX <- training[,names(training) != "y"]
# preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
# preProcValues
# summary(trainX)

# KNN model
system.time({
  knnfit1 <- train(y~., data = training, 
                  method = "knn", 
                  trControl=fitControl,
                  preProcess = c("center","scale"),
                  tuneLength = 15)
})

knnfit1
summary(knnfit1)
knnfit1$finalModel$tuneValue

testPredknn1 <- predict(knnfit1, testing)
postResample(testPredknn1, testing$y)
# Accuracy     Kappa 
# 0.6236220 0.6184057 

cmknn1 <- confusionMatrix(testPredknn1, testing$y)
cmknn1



# Random Forest model
system.time({
  rffit1 <- train(y~., data = training, 
                   method = "rf",
                   trControl= fitControl,
                   tuneLength = 5)
})

rffit1
summary(rffit1)
rffit1$finalModel$tuneValue

testPredrf1 <- predict(rffit1, testing)
postResample(testPredrf1, testing$y)

# Accuracy     Kappa 
# 0.8503937 0.8483409

cmrf1 <- confusionMatrix(testPredrf1, testing$y)
cmrf1


plot(rffit1)
summary(rffit1$results)

plot(c5fit1$results$Accuracy)
plot(knnfit1$results$Accuracy)
plot(rffit1$results$Accuracy)

ModelData <- resamples(list(C50 = c5fit1, KNN = knnfit1, RF = rffit1))

summary(ModelData)


metrics <- data.frame("Model"= c("C5.0","KNN","RF"),
                      "Accuracy"= c(0.7653543,0.6236220,0.8503937),
                      "Kappa" = c(0.7621860,0.6184057,0.8483409))

library(reshape2)
df2 <- melt(metrics, id.vars='Model')

library(ggplot2)
ggplot(df2, aes(x=Model, y= value, fill=variable )) + 
  geom_bar(stat='identity', position='dodge')+
  coord_flip()+
  theme_classic()+
  geom_text(aes(label = round(value,2)),position = position_dodge(0.70))+
  ggtitle("Accuracy and Kappa of the Models")


