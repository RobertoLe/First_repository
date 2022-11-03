# Descripción: Prediccion models of sentiment towards a product C5T3
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-29
##########################Cores#####################################################

# This code adds additional cores to R to increase the processing power
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
#stopCluster(cl)

# Definitions ######################################################################

# 0: unclear

# 1: negative

# 2: somewhat negative

# 3: somewhat positive

# 4: positive

# 5: very positive


# iOS – counts mentions of iOS on a webpage
# iphonecampos – counts positive sentiment mentions of the iphone camera
# galaxydisneg – counts negative sentiment mentions of the Galaxy display
# htcperunc – counts the unclear sentiment mentions of HTC performance

# Data ######################################################################

library(tidyr)
library(dplyr)
library(caret)

galaxy <- read.csv("R Data Sets/smallmatrix_labeled_8d/galaxy_smallmatrix_labeled_9d.csv")

iphone <- read.csv("R Data Sets/smallmatrix_labeled_8d/iphone_smallmatrix_labeled_8d.csv")


# EDA ######################################################################

summary(galaxy)
summary(iphone)

str(iphone)
str(galaxy)


library(visdat)
# This code helps to see graphically the type of variables in a data set
visdat::vis_dat(iphone,sort_type = FALSE) 

# This code helps to see graphically the missing values in a data set
visdat::vis_miss(iphone) 
visdat::vis_miss(galaxy) 
# Looks like there are no missing values

# This package plots a histogram of each numeric variable, so it is very 
# useful because every variable in both data sets is numeric.
library(funModeling)
plot_num(iphone[1:15])
plot_num(iphone[16:30])
plot_num(iphone[31:45])
plot_num(iphone[46:59])

plot_num(galaxy[1:15])
plot_num(galaxy[16:30])
plot_num(galaxy[31:45])
plot_num(galaxy[46:59])


library(plotly)
# A quick histogram to see the dependent variable
plot_ly(iphone, x= ~iphonesentiment, type='histogram')
plot_ly(galaxy, x= ~galaxysentiment, type = 'histogram')


# shows the frequency of each category in a table
table(iphone$iphonesentiment)
table(galaxy$galaxysentiment)


# This contains the data of the sentiment scores of iphone
p1 <- data.frame("sentiment"= c("0","1","2","3","4","5"),
                 "frequency"= c(1962,390,454,1188,1439,7540))

library("ggplot2")
# Iphone barplot
# This code creates a barplot with the numbers showing on top of each bar
## Make the frequencies numbers (rather than factors)
p1$frequency <- as.numeric(as.character(p1$frequency))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(p1$frequency))
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p1$frequency, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Frequency of sentiment scores of iphone",
              ylab = "Frequency",
              border = "white",
              col = "skyblue")
## Add text at top of bars
text(x = xx, y = p1$frequency, label = p1$frequency, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p1$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

p2 <- data.frame("sentiment"= c('0','1','2','3','4','5'), 
                 "frequency"= c(1696,382,450,1175,1417,7791))

# Galaxy barplot
# This code creates a barplot with the numbers showing on top of each bar
## Make the frequencies numbers (rather than factors)
p2$frequency <- as.numeric(as.character(p2$frequency))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(p2$frequency))
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p2$frequency, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Frequency of sentiment scores of galaxy",
              ylab = "Frequency",
              border = "white",
              col = "thistle")
## Add text at top of bars
text(x = xx, y = p2$frequency, label = p2$frequency, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p2$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

# percentage iphone
#15.12,3,3.5,9.16,11.1,58.12 
# percentage galaxy
#13.1,3,3.5,9.1,11,60.3

# this is iphone
p3 <- data.frame("sentiment"= c('0','1','2','3','4','5'),
                 "percentage"= c(15.12,3,3.5,9.16,11.1,58.12))

p3$percentage <- as.numeric(as.character(p3$percentage))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 100)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p3$percentage, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Percentage of sentiment scores of iphone",
              ylab = "Percentage",
              border = "white",
              col = "skyblue")
## Add text at top of bars
text(x = xx, y = p3$percentage, label = p3$percentage, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p3$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

# this is galaxy
p3 <- data.frame("sentiment"= c('0','1','2','3','4','5'),
                 "percentage"= c(13.1,3,3.5,9.1,11,60.3))

p3$percentage <- as.numeric(as.character(p3$percentage))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 100)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p3$percentage, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Percentage of sentiment scores of galaxy",
              ylab = "Percentage",
              border = "white",
              col = "thistle")
## Add text at top of bars
text(x = xx, y = p3$percentage, label = p3$percentage, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p3$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)


# This gives you a graph with scatter plots, histograms, and the correlation
# of each variable.
# It is recommended for a small amount of variables, no more than 6.
#library(PerformanceAnalytics)
#system.time({
#chart.Correlation(data, histogram = TRUE, pch = 15)
#})

# Separate the data to make it visually aesthetic in the corrplot
data <- cbind(galaxy[,1:15],'sentiment'= galaxy[,59])
data1 <- cbind(galaxy[,16:30],'sentiment'= galaxy[,59])
data2 <- cbind(galaxy[,31:45],'sentiment'= galaxy[,59])
data3 <- cbind(galaxy[,46:58],'sentiment'= galaxy[,59])

library(corrplot)
corrplot(cor(data), method = 'number')
corrplot(cor(data1), method = 'number')
corrplot(cor(data2), method = 'number')
corrplot(cor(data3), method = 'number')
# None of the features are highly correlated to the response variable.
# However, some features have a high correlation between each other which
# indicates collinearity

# Feature selection ######################################################################
# This code helps me identify the attributes that are highly correlated
set.seed(1492)
# load the library
library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix <- cor(galaxy)
# summarize the correlation matrix
options(max.print=1000000)
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelatedGalaxy <- findCorrelation(correlationMatrix, cutoff=0.75)
#names = TRUE, verbose = TRUE)
# print indexes of highly correlated attributes
print(highlyCorrelatedGalaxy)

galaxyCor <- galaxy[,-highlyCorrelatedGalaxy]

# with iphone
correlationMatrix <- cor(iphone)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelatedIphone <- findCorrelation(correlationMatrix, cutoff=0.75)
# names = TRUE, verbose = TRUE)
# print indexes of highly correlated attributes
print(highlyCorrelatedIphone)

iphoneCor <- iphone[,-highlyCorrelated]


# This is iphone
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, percentage unique, zero variance and near zero variance 

nzvMetrics <- nearZeroVar(iphone, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(iphone, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
iphoneNZV <- iphone[,-nzv]
str(iphoneNZV)


# This is galaxy
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, percentage unique, zero variance and near zero variance 

nzvMetrics <- nearZeroVar(galaxy, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(galaxy, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
galaxyNZV <- galaxy[,-nzv]
str(galaxyNZV)


# Let's sample the data before using RFE for Iphone

iphoneSample <- iphone[sample(1:nrow(iphone), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
system.time({
  rfeResults <- rfe(iphoneSample[,1:58], 
                    iphoneSample$iphonesentiment, 
                    sizes=(1:58), 
                    rfeControl=ctrl)
})
# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
iphoneRFE <- iphone[,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphone$iphonesentiment

# review outcome
str(iphoneRFE)


# Let's sample the data before using RFE for Galaxy

galaxySample <- galaxy[sample(1:nrow(galaxy), 1000, replace=FALSE),]


# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
system.time({
  rfeResultsGalaxy <- rfe(galaxySample[,1:58], 
                          galaxySample$galaxysentiment, 
                          sizes=(1:58), 
                          rfeControl=ctrl)
}) # 3 hours

# Get results
rfeResultsGalaxy

# Plot results
plot(rfeResultsGalaxy, type=c("g", "o"))

# create new data set with rfe recommended features
galaxyRFE <- galaxy[,predictors(rfeResultsGalaxy)]

# add the dependent variable to iphoneRFE
galaxyRFE$galaxysentiment <- galaxy$galaxysentiment

# review outcome
str(galaxyRFE)

# Pre- processing Galaxy######################################################################

set.seed(1492)

# Correlation Feature Selection
inTraining <- createDataPartition(galaxyCor$galaxysentiment, p = .70, list = FALSE)
trainingCor <- galaxyCor[inTraining,]
testingCor <- galaxyCor[-inTraining,]

trainingCor$galaxysentiment <- as.factor(trainingCor$galaxysentiment)
testingCor$galaxysentiment <- as.factor(testingCor$galaxysentiment)
str(trainingCor)
str(testingCor)

# Near Zero Variance Feature Selection
inTraining <- createDataPartition(galaxyNZV$galaxysentiment, p=.70, list = FALSE)
trainingNZV <- galaxyNZV[inTraining,]
tesingNZV <- galaxyNZV[-inTraining,]

trainingNZV$galaxysentiment <- as.factor(trainingNZV$galaxysentiment)
tesingNZV$galaxysentiment <- as.factor(tesingNZV$galaxysentiment)
str(trainingNZV)
str(tesingNZV)

# Recursive Feature Elimination 
inTraining <- createDataPartition(galaxyRFE$galaxysentiment, p=.70, list=FALSE)
trainingRFE <- galaxyRFE[inTraining,]
testingRFE <- galaxyRFE[-inTraining,]

trainingRFE$galaxysentiment <- as.factor(trainingRFE$galaxysentiment)
testingRFE$galaxysentiment <- as.factor(testingRFE$galaxysentiment)
str(trainingRFE)
str(testingRFE)

# Models Galaxy ######################################################################

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# C5.0 model
system.time({
  c5fit1 <- train(galaxysentiment~., data = trainingCor,
                  method = "C5.0",
                  trControl = fitControl,
                  tunelength = 5)
})

c5fit1
summary(c5fit1)
c5fit1$finalModel$tuneValue

testPredc501 <- predict(c5fit1, testingCor)
postResample(testPredc501, testingCor$galaxysentiment)
#Accuracy     Kappa 
#0.7412190 0.4825711 

system.time({
  c5fit2 <- train(galaxysentiment~., data = trainingNZV,
                  method = "C5.0",
                  trControl = fitControl,
                  tunelength = 5)
})

c5fit2
summary(c5fit2)
c5fit2$finalModel$tuneValue

testPredc502 <- predict(c5fit2, tesingNZV)
postResample(testPredc502, tesingNZV$galaxysentiment)
#Accuracy     Kappa 
#0.7518079 0.4944048 

system.time({
  c5fit3 <- train(galaxysentiment~., data = trainingRFE,
                  method = "C5.0",
                  trControl = fitControl,
                  tunelength = 5)
})

c5fit3
summary(c5fit3)
c5fit3$finalModel$tuneValue

testPredc503 <- predict(c5fit3, testingRFE)
postResample(testPredc503, testingRFE$galaxysentiment)
#Accuracy     Kappa 
#0.7618802 0.5170860 

# Random Forest Model


system.time({
  rFfit1 <- train(galaxysentiment~., data = trainingCor,
                  method = "rf",
                  trControl = fitControl,
                  tunelength = 5)
})

rFfit1
summary(rFfit1)
rFfit1$finalModel$tuneValue

testPredrf1 <- predict(rFfit1, testingCor)
postResample(testPredrf1, testingCor$galaxysentiment)
#Accuracy     Kappa 
#0.7401860 0.4821303 


system.time({
  rFfit2 <- train(galaxysentiment~., data = trainingNZV,
                  method = "rf",
                  trControl = fitControl,
                  tunelength = 5)
})
rFfit2
summary(rFfit2)
rFfit2$finalModel$tuneValue

testPredrf2 <- predict(rFfit2, tesingNZV)
postResample(testPredrf2, tesingNZV$galaxysentiment)
#Accuracy     Kappa 
#0.7574897 0.5023972 


system.time({
  rFfit3 <- train(galaxysentiment~., data = trainingRFE,
                  method = "rf",
                  trControl = fitControl,
                  tunelength = 5)
})

testPredrf3 <- predict(rFfit3, testingRFE)
postResample(testPredrf3, testingRFE$galaxysentiment)
#Accuracy     Kappa 
#0.7585227 0.5162388 

# Support Vector Machine Model
library(e1071)

# Lineal kernel
system.time({
  svmfit1 <- train(galaxysentiment~., data = trainingCor,
                   method = "svmLinear",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm1 <- predict(svmfit1, testingCor)
postResample(testPredsvm1, testingCor$galaxysentiment)
#Accuracy     Kappa 
#0.7027376 0.3600613 

system.time({
  svmfit2 <- train(galaxysentiment~., data = trainingNZV,
                   method = "svmLinear",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm2 <- predict(svmfit2, tesingNZV)
postResample(testPredsvm2, tesingNZV$galaxysentiment)
#Accuracy     Kappa 
#0.6776860 0.3060065 

system.time({
  svmfit3 <- train(galaxysentiment~., data = trainingRFE,
                   method = "svmLinear",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm3 <- predict(svmfit3, testingRFE)
postResample(testPredsvm3, testingRFE$galaxysentiment)
#Accuracy     Kappa 
#0.6993802 0.3624548 

# Polynomial kernel
system.time({
  svmfit4 <- train(galaxysentiment~., data = trainingCor,
                   method = "svmPoly",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm4 <- predict(svmfit4, testingCor)
postResample(testPredsvm4, testingCor$galaxysentiment)
#Accuracy     Kappa 
#0.7184917 0.4044062 

system.time({
  svmfit5 <- train(galaxysentiment~., data = trainingNZV,
                   method = "svmPoly",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm5 <- predict(svmfit5, tesingNZV)
postResample(testPredsvm5, tesingNZV$galaxysentiment)
#Accuracy     Kappa 
#0.7259814 0.4222493 


system.time({
  svmfit6 <- train(galaxysentiment~., data = trainingRFE,
                   method = "svmPoly",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm6 <- predict(svmfit6, testingRFE)
postResample(testPredsvm6, testingRFE$galaxysentiment)
#Accuracy     Kappa 
#0.7316632 0.4488045 

# Radial Basis Function Kernel 
system.time({
  svmfit7 <- train(galaxysentiment~., data = trainingCor,
                   method = "svmRadial",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm7 <- predict(svmfit7, testingCor)
postResample(testPredsvm7, testingCor$galaxysentiment)
#Accuracy     Kappa 
#0.7267562 0.4426791 

system.time({
  svmfit8 <- train(galaxysentiment~., data = trainingNZV,
                   method = "svmRadial",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm8 <- predict(svmfit8, tesingNZV)
postResample(testPredsvm8, tesingNZV$galaxysentiment)
#Accuracy     Kappa 
#0.7466426 0.4731909 

system.time({
  svmfit9 <- train(galaxysentiment~., data = trainingRFE,
                   method = "svmRadial",
                   trControl = fitControl,
                   tunelength = 5)
})
testPredsvm9 <- predict(svmfit9, testingRFE)
postResample(testPredsvm9, testingRFE$galaxysentiment)
#Accuracy     Kappa 
#0.7463843 0.4724057 

# KNN
library(kknn)

system.time({
kknnfit1 <- train(galaxysentiment~., data = trainingRFE,
                  method = "kknn",
                  trControl = fitControl,
                  tunelength = 5)
})
testPredknn1 <- predict(kknnfit1, testingRFE)
postResample(testPredknn1, testingRFE$galaxysentiment)
#Accuracy     Kappa 
#0.7461260 0.4949305 

system.time({
  kknnfit2 <- train(galaxysentiment~., data = trainingNZV,
                    method = "kknn",
                    trControl = fitControl,
                    tunelength = 5)
})
testPredknn2 <- predict(kknnfit2, tesingNZV)
postResample(testPredknn2, tesingNZV$galaxysentiment)
#Accuracy     Kappa 
#0.7267562 0.4629368 

system.time({
  kknnfit3 <- train(galaxysentiment~., data = trainingCor,
                    method = "kknn",
                    trControl = fitControl,
                    tunelength = 5)
})
testPredknn3 <- predict(kknnfit3, testingCor)
postResample(testPredknn3, testingCor$galaxysentiment)
#Accuracy     Kappa 
#0.7295971 0.4727423 

ModelDataGalaxy <- resamples(list(C501 = c5fit1,C502 = c5fit2,C503 = c5fit3,
                            RF1 = rFfit1, RF2 = rFfit2, RF3 = rFfit3,
                            SVML1 = svmfit1, SVML2 = svmfit2, SVML3 = svmfit3,
                            SVMP4 = svmfit4, SVMP5 = svmfit5, SVMP6 = svmfit6,
                            SVMR7 = svmfit7, SVMR8 = svmfit8, SVMR9 = svmfit9,
                            KNN1 = kknnfit1, KNN2 = kknnfit2, KNN3 = kknnfit3))

summary(ModelDataGalaxy)

# Winner: c5fit3
#Accuracy     Kappa 
#0.7618802 0.5170860 

# Pre-processing Iphone ######################################################################

# Correlation Feature Selection
inTraining <- createDataPartition(iphoneCor$iphonesentiment, p = .70, list = FALSE)
trainingCor1 <- iphoneCor[inTraining,]
testingCor1 <- iphoneCor[-inTraining,]

trainingCor1$iphonesentiment <- as.factor(trainingCor1$iphonesentiment)
testingCor1$iphonesentiment <- as.factor(testingCor1$iphonesentiment)
str(trainingCor1)
str(testingCor1)

# Near Zero Variance Feature Selection
inTraining <- createDataPartition(iphoneNZV$iphonesentiment, p=.70, list = FALSE)
trainingNZV1 <- iphoneNZV[inTraining,]
tesingNZV1 <- iphoneNZV[-inTraining,]

trainingNZV1$iphonesentiment <- as.factor(trainingNZV1$iphonesentiment)
tesingNZV1$iphonesentiment <- as.factor(tesingNZV1$iphonesentiment)
str(trainingNZV1)
str(tesingNZV1)

# Recursive Feature Elimination 
inTraining <- createDataPartition(iphoneRFE$iphonesentiment, p=.70, list=FALSE)
trainingRFE1 <- iphoneRFE[inTraining,]
testingRFE1 <- iphoneRFE[-inTraining,]

trainingRFE1$iphonesentiment <- as.factor(trainingRFE1$iphonesentiment)
testingRFE1$iphonesentiment <- as.factor(testingRFE1$iphonesentiment)
str(trainingRFE1)
str(testingRFE1)

# Models Iphone ######################################################################

# C5.0 model
system.time({
  c5fit1A <- train(iphonesentiment~., data = trainingCor1,
                  method = "C5.0",
                  trControl = fitControl,
                  tunelength = 5)
})

c5fit1A
summary(c5fit1A)
c5fit1A$finalModel$tuneValue

testPredc501A <- predict(c5fit1A, testingCor1)
postResample(testPredc501A, testingCor1$iphonesentiment)
#Accuracy     Kappa 
#0.7419686 0.4961306 

system.time({
  c5fit2A <- train(iphonesentiment~., data = trainingNZV1,
                  method = "C5.0",
                  trControl = fitControl,
                  tunelength = 5)
})

c5fit2A
summary(c5fit2A)
c5fit2A$finalModel$tuneValue

testPredc502A <- predict(c5fit2A, tesingNZV1)
postResample(testPredc502A, tesingNZV1$iphonesentiment)
#Accuracy     Kappa 
#0.7658700 0.5339852 

system.time({
  c5fit3A <- train(iphonesentiment~., data = trainingRFE1,
                  method = "C5.0",
                  trControl = fitControl,
                  tunelength = 5)
})

c5fit3A
summary(c5fit3A)
c5fit3A$finalModel$tuneValue

testPredc503A <- predict(c5fit3A, testingRFE1)
postResample(testPredc503A, testingRFE1$iphonesentiment)
#Accuracy     Kappa 
#0.7733231 0.5611647 


# Random Forest Model
library(MLeval)

system.time({
  rFfit1A <- train(iphonesentiment~., data = trainingCor1,
                  method = "rf",
                  trControl = fitControl,
                  tunelength = 5)
})
rFfit1A
summary(rFfit1A)
rFfit1A$finalModel$tuneValue

testPredrf1A <- predict(rFfit1A, testingCor1)
postResample(testPredrf1A, testingCor1$iphonesentiment)
#Accuracy     Kappa 
#0.7417116 0.4983279 

system.time({
  rFfit2A <- train(iphonesentiment~., data = trainingNZV1,
                  method = "rf",
                  trControl = fitControl,
                  tunelength = 5)
})
rFfit2A
testPredrf2A <- predict(rFfit2A, tesingNZV1)
postResample(testPredrf2A, tesingNZV1$iphonesentiment)
#Accuracy     Kappa 
#0.766898 0.533573 

system.time({
  rFfit3A <- train(iphonesentiment~., data = trainingRFE1,
                  method = "rf",
                  trControl = fitControl,
                  tunelength = 5)
})
rFfit3A
testPredrf3A <- predict(rFfit3A, testingRFE1)
postResample(testPredrf3A, testingRFE1$iphonesentiment)
#Accuracy     Kappa 
#0.7725520 0.5640802 

# Support Vector Machine Model
library(e1071)

# Lineal kernel
system.time({
  svmfit1A <- train(iphonesentiment~., data = trainingCor1,
                   method = "svmLinear",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit1A
testPredsvm1A <- predict(svmfit1A, testingCor1)
postResample(testPredsvm1A, testingCor1$iphonesentiment)
#Accuracy     Kappa 
#0.7180673 0.4190738 

system.time({
  svmfit2A <- train(iphonesentiment~., data = trainingNZV1,
                   method = "svmLinear",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit2A
testPredsvm2A <- predict(svmfit2A, tesingNZV1)
postResample(testPredsvm2A, tesingNZV1$iphonesentiment)
#Accuracy     Kappa 
#0.6951940 0.3615369 

system.time({
  svmfit3A <- train(iphonesentiment~., data = trainingRFE1,
                   method = "svmLinear",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit3A
testPredsvm3A <- predict(svmfit3A, testingRFE1)
postResample(testPredsvm3A, testingRFE1$iphonesentiment)
#Accuracy     Kappa 
#0.7154973 0.4268458 

# Polynomial kernel
system.time({
  svmfit4A <- train(iphonesentiment~., data = trainingCor1,
                   method = "svmPoly",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit4A
testPredsvm4A <- predict(svmfit4A, testingCor1)
postResample(testPredsvm4A, testingCor1$iphonesentiment)
#Accuracy     Kappa 
#0.7144693 0.4072349 

system.time({
  svmfit5A <- train(iphonesentiment~., data = trainingNZV1,
                   method = "svmPoly",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit5A
testPredsvm5A <- predict(svmfit5A, tesingNZV1)
postResample(testPredsvm5A, tesingNZV1$iphonesentiment)
#Accuracy     Kappa 
#0.7311745 0.4528588 

system.time({
  svmfit6A <- train(iphonesentiment~., data = trainingRFE1,
                   method = "svmPoly",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit6A
testPredsvm6A <- predict(svmfit6A, testingRFE1)
postResample(testPredsvm6A, testingRFE1$iphonesentiment)
#Accuracy     Kappa 
#0.7393986 0.4962608 

# Radial Basis Function Kernel 
system.time({
  svmfit7A <- train(iphonesentiment~., data = trainingCor1,
                   method = "svmRadial",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit7A
testPredsvm7A <- predict(svmfit7A, testingCor1)
postResample(testPredsvm7A, testingCor1$iphonesentiment)
#Accuracy     Kappa 
#0.7262914 0.4460495

system.time({
  svmfit8A <- train(iphonesentiment~., data = trainingNZV1,
                   method = "svmRadial",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit8A
testPredsvm8A <- predict(svmfit8A, tesingNZV1)
postResample(testPredsvm8A, tesingNZV1$iphonesentiment)
#Accuracy     Kappa 
#0.7566178 0.5069567 

system.time({
  svmfit9A <- train(iphonesentiment~., data = trainingRFE1,
                   method = "svmRadial",
                   trControl = fitControl,
                   tunelength = 5)
})
svmfit9A
testPredsvm9A <- predict(svmfit9A, testingRFE1)
postResample(testPredsvm9A, testingRFE1$iphonesentiment)
#Accuracy     Kappa 
#0.7496787 0.4998515 

# KNN
library(kknn)

system.time({
  kknnfit1A <- train(iphonesentiment~., data = trainingRFE1,
                    method = "kknn",
                    trControl = fitControl,
                    tunelength = 5)
})
kknnfit1A
testPredknn1A <- predict(kknnfit1A, testingRFE1)
postResample(testPredknn1A, testingRFE1$iphonesentiment)
#Accuracy     Kappa 
#0.3382164 0.1685654 

system.time({
  kknnfit2A <- train(iphonesentiment~., data = trainingNZV1,
                    method = "kknn",
                    trControl = fitControl,
                    tunelength = 5)
})
kknnfit2A
testPredknn2A <- predict(kknnfit2A, tesingNZV1)
postResample(testPredknn2A, tesingNZV1$iphonesentiment)
#Accuracy     Kappa 
#0.2996659 0.1277521 

system.time({
  kknnfit3A <- train(iphonesentiment~., data = trainingCor1,
                    method = "kknn",
                    trControl = fitControl,
                    tunelength = 5)
})
kknnfit3A
testPredknn3A <- predict(kknnfit3A, testingCor1)
postResample(testPredknn3A, testingCor1$iphonesentiment)
#Accuracy     Kappa 
#0.2526343 0.1106657 


ModelDataIphone <- resamples(list(C501 = c5fit1A,C502 = c5fit2A,C503 = c5fit3A,
                                  RF1 = rFfit1A, RF2 = rFfit2A, RF3 = rFfit3A,
                                  SVML1 = svmfit1A, SVML2 = svmfit2A, SVML3 = svmfit3A,
                                  SVMP4 = svmfit4A, SVMP5 = svmfit5A, SVMP6 = svmfit6A,
                                  SVMR7 = svmfit7A, SVMR8 = svmfit8A, SVMR9 = svmfit9A,
                                  KNN1 = kknnfit1A, KNN2 = kknnfit2A, KNN3 = kknnfit3A))

summary(ModelDataIphone)



# Winner: c5fit3A
#Accuracy     Kappa 
#0.7733231 0.5611647 

# Feature Engineering ######################################################################

##Iphone
# create a new dataset that will be used for recoding sentiment
iphoneRC <- iphoneRFE
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, '0' = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(iphoneRC)
str(iphoneRC)
# make iphonesentiment a factor
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)

inTraining <- createDataPartition(iphoneRC$iphonesentiment,p =.70, list = FALSE)
trainingRCA <- iphoneRC[inTraining,]
testingRCA <- iphoneRC[-inTraining,]

system.time({
  c5fit3RCA <- train(iphonesentiment~., data = trainingRCA,
                   method = "C5.0",
                   trControl = fitControl,
                   tunelength = 5)
})

c5fit3RCA
summary(c5fit3RCA)
c5fit3RCA$finalModel$tuneValue

testPredc503RCA <- predict(c5fit3RCA, testingRCA)
postResample(testPredc503RCA, testingRCA$iphonesentiment)
#Accuracy     Kappa 
#0.8478149 0.6235084  

cmRCA <- confusionMatrix(testPredc503RCA,testingRCA$iphonesentiment)
cmRCA


## PCA

inTraining <- createDataPartition(iphone$iphonesentiment, p= .70, list = FALSE)
training <- iphone[inTraining,]
testing <- iphone[-inTraining,]
# data = training and testing from iphoneDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)
#PCA needed 24 components to capture 95 percent of the variance

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, training[,-59])

# add the dependent to training
train.pca$iphonesentiment <- training$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing[,-59])

# add the dependent to training
test.pca$iphonesentiment <- testing$iphonesentiment

# inspect results
str(train.pca)
str(test.pca)

train.pca$iphonesentiment <- as.factor(train.pca$iphonesentiment)
test.pca$iphonesentiment <- as.factor(test.pca$iphonesentiment)

system.time({
  c5fit3PCAA <- train(iphonesentiment~., data = train.pca,
                     method = "C5.0",
                     trControl = fitControl,
                     tunelength = 5)
})

testPredc503PCAA <- predict(c5fit3PCAA, test.pca)
postResample(testPredc503PCAA, test.pca$iphonesentiment)
#Accuracy     Kappa 
#0.7527628 0.5245679 

cmPCA <- confusionMatrix(testPredc503PCAA,test.pca$iphonesentiment)
cmPCA


##Galaxy

# create a new dataset that will be used for recoding sentiment
galaxyRC <- galaxyRFE
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
galaxyRC$galaxysentiment <- recode(galaxyRC$galaxysentiment, '0' = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(galaxyRC)
str(galaxyRC)
# make galaxysentiment a factor
galaxyRC$galaxysentiment <- as.factor(galaxyRC$galaxysentiment)

inTraining <- createDataPartition(galaxyRC$galaxysentiment,p =.70, list = FALSE)
trainingRC <- galaxyRC[inTraining,]
testingRC <- galaxyRC[-inTraining,]

system.time({
  c5fit3RC <- train(galaxysentiment~., data = trainingRC,
                     method = "C5.0",
                     trControl = fitControl,
                     tunelength = 5)
})

c5fit3RC
summary(c5fit3RC)
c5fit3RC$finalModel$tuneValue

testPredc503RC <- predict(c5fit3RC, testingRC)
postResample(testPredc503RC, testingRC$galaxysentiment)
#Accuracy     Kappa 
#0.8426763 0.5927696   

cmRC <- confusionMatrix(testPredc503RC,testingRC$galaxysentiment)
cmRC


## PCA

inTraining <- createDataPartition(galaxy$galaxysentiment, p= .70, list = FALSE)
training <- galaxy[inTraining,]
testing <- galaxy[-inTraining,]
# data = training and testing from iphoneDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)
#PCA needed 26 components to capture 95 percent of the variance

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, training[,-59])

# add the dependent to training
train.pca$galaxysentiment <- training$galaxysentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing[,-59])

# add the dependent to training
test.pca$galaxysentiment <- testing$galaxysentiment

# inspect results
str(train.pca)
str(test.pca)

train.pca$galaxysentiment <- as.factor(train.pca$galaxysentiment)
test.pca$galaxysentiment <- as.factor(test.pca$galaxysentiment)

system.time({
  c5fit3PCA <- train(galaxysentiment~., data = train.pca,
                      method = "C5.0",
                      trControl = fitControl,
                      tunelength = 5)
})

testPredc503PCA <- predict(c5fit3PCA, test.pca)
postResample(testPredc503PCA, test.pca$galaxysentiment)
#Accuracy     Kappa 
#0.7636880 0.5214581  

cmPCA <- confusionMatrix(testPredc503PCA,test.pca$galaxysentiment)
cmPCA

# Winner: Altering the dependant variable in both cases(iphone and galaxy)
#Iphone
#Accuracy     Kappa 
#0.8478149 0.6235084 
# Galaxy
#Accuracy     Kappa 
#0.8426763 0.5927696   

# Predictions ######################################################################

largeIphone <- read.csv("R Data Sets/LargeMatrix.csv")
largeGalaxy <- read.csv("R Data Sets/LargeMatrix.csv")

largeIpRFE <- largeIphone[,predictors(rfeResults)]
largeGaRFE <- largeGalaxy[,predictors(rfeResultsGalaxy)]

largeIpRFE$iphonesentiment <- 0
largeGaRFE$galaxysentiment <- 0

finalPredIphone <- predict(c5fit3RCA, largeIpRFE)
finalPredIphone

finalPredGalaxy <- predict(c5fit3RC, largeGaRFE)
finalPredGalaxy

largeIpRFE$iphonesentiment <- finalPredIphone
largeGaRFE$galaxysentiment <- finalPredGalaxy

summary(largeIpRFE$iphonesentiment)
summary(largeGaRFE$galaxysentiment)




# Results ######################################################################

# This contains the data of the sentiment scores of iphone
p1 <- data.frame("sentiment"= c("0","1","2","3","4"),
                 "frequency"= c(16650,0,770,786,4390))

library("ggplot2")
# Iphone barplot
# This code creates a barplot with the numbers showing on top of each bar
## Make the frequencies numbers (rather than factors)
p1$frequency <- as.numeric(as.character(p1$frequency))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(p1$frequency))
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p1$frequency, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Frequency of sentiment scores of iphone",
              ylab = "Frequency",
              border = "white",
              col = "lightskyblue2")
## Add text at top of bars
text(x = xx, y = p1$frequency, label = p1$frequency, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p1$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

p2 <- data.frame("sentiment"= c('0','1','2','3','4'), 
                 "frequency"= c(16289,0,767,1001,4539))

# Galaxy barplot
# This code creates a barplot with the numbers showing on top of each bar
## Make the frequencies numbers (rather than factors)
p2$frequency <- as.numeric(as.character(p2$frequency))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(p2$frequency))
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p2$frequency, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Frequency of sentiment scores of galaxy",
              ylab = "Frequency",
              border = "white",
              col = "plum3")
## Add text at top of bars
text(x = xx, y = p2$frequency, label = p2$frequency, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p2$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)


# this is iphone
p3 <- data.frame("sentiment"= c('0','1','2','3','4'),
                 "percentage"= c(73.7,0,3.4,3.5,19.4))

p3$percentage <- as.numeric(as.character(p3$percentage))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 100)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p3$percentage, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Percentage of sentiment scores of iphone",
              ylab = "Percentage",
              border = "white",
              col = "lightskyblue2")
## Add text at top of bars
text(x = xx, y = p3$percentage, label = p3$percentage, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p3$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

# this is galaxy
p3 <- data.frame("sentiment"= c('0','1','2','3','4'),
                 "percentage"= c(72.1,0,3.4,4.4,20.1))

p3$percentage <- as.numeric(as.character(p3$percentage))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 100)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p3$percentage, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Percentage of sentiment scores of galaxy",
              ylab = "Percentage",
              border = "white",
              col = "plum3")
## Add text at top of bars
text(x = xx, y = p3$percentage, label = p3$percentage, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p3$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

## This is without the unclear category
# this is iphone
p3 <- data.frame("sentiment"= c('1','2','3','4'),
                 "percentage"= c(0,13,13.3,73.7))

p3$percentage <- as.numeric(as.character(p3$percentage))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 100)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p3$percentage, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Percentage of clear sentiment scores of iphone",
              ylab = "Percentage",
              border = "white",
              col = "lightskyblue2")
## Add text at top of bars
text(x = xx, y = p3$percentage, label = p3$percentage, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p3$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

# this is galaxy
p3 <- data.frame("sentiment"= c('1','2','3','4'),
                 "percentage"= c(0,12.2,15.8,72))

p3$percentage <- as.numeric(as.character(p3$percentage))
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 100)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(p3$percentage, xaxt = 'n', xlab = 'Sentiment', width = 0.85, ylim = ylim,
              main = "Percentage of clear sentiment scores of galaxy",
              ylab = "Percentage",
              border = "white",
              col = "plum3")
## Add text at top of bars
text(x = xx, y = p3$percentage, label = p3$percentage, pos = 3, cex = 0.8)#, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=p3$sentiment, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

# Summary ######################################################################

#IPHONE
metricsA <- data.frame("Model"= c("C5.0_COR","C5.0_NZV","C5.0_RFE",
                                 "RF_COR","RF_NZV","RF_RFE",
                                 "SVM_L_COR","SVM_L_NZV","SVM_L_RFE",
                                 "SVM_P_COR","SVM_P_NZV","SVM_P_RFE",
                                 "SVM_R_COR","SVM_R_NZV","SVM_R_RFE",
                                 "KKNN_COR","KKNN_NZV","KKNN_RFE"),
                      "Accuracy"= c(0.7419686,0.7658700,0.7733231,
                                    0.7417116,0.766898,0.7725520,
                                    0.7180673,0.6951940,0.7154973,
                                    0.7144693,0.7311745,0.7393986,
                                    0.7262914,0.7566178,0.7496787,
                                    0.3382164,0.2996659,0.2526343),
                      "Kappa" = c(0.4961306,0.5339852,0.5611647,
                                  0.4983279,0.533573,0.5640802,
                                  0.4190738,0.3615369,0.4268458,
                                  0.4072349,0.4528588,0.4962608,
                                  0.4460495,0.5069567,0.4998515,
                                  0.1685654,0.1277521,0.1106657))

library(reshape2)
df2 <- melt(metricsA, id.vars='Model')

library(ggplot2)
ggplot(df2, aes(x=Model, y= value, fill=variable )) + 
  geom_bar(stat='identity', position='dodge')+
  coord_flip()+
  theme_classic()+
  geom_text(aes(label = round(value,2)),position = position_dodge(0.70))+
  ggtitle("Accuracy and Kappa of the Iphone Models")

#GALAXY
metrics <- data.frame("Model"= c("C5.0_COR","C5.0_NZV","C5.0_RFE",
                                  "RF_COR","RF_NZV","RF_RFE",
                                  "SVM_L_COR","SVM_L_NZV","SVM_L_RFE",
                                  "SVM_P_COR","SVM_P_NZV","SVM_P_RFE",
                                  "SVM_R_COR","SVM_R_NZV","SVM_R_RFE",
                                  "KKNN_COR","KKNN_NZV","KKNN_RFE"),
                       "Accuracy"= c(0.7412190,0.7518079,0.7618802,
                                     0.7401860,0.7574897,0.7585227,
                                     0.7027376,0.6776860,0.6993802,
                                     0.7184917,0.7259814,0.7316632,
                                     0.7267562,0.7466426,0.7463843,
                                     0.7461260,0.7267562,0.7295971),
                       "Kappa" = c(0.4825711,0.4944048,0.5170860,
                                   0.4821303,0.5023972,0.5162388,
                                   0.3600613,0.3060065,0.3624548,
                                   0.4044062,0.4222493,0.4488045,
                                   0.4426791,0.4731909,0.4724057,
                                   0.4949305,0.4629368,0.4727423))
 
library(reshape2)
df2 <- melt(metrics, id.vars='Model')

library(ggplot2)
ggplot(df2, aes(x=Model, y= value, fill=variable )) + 
  geom_bar(stat='identity', position='dodge')+
  coord_flip()+
  theme_classic()+
  geom_text(aes(label = round(value,2)),position = position_dodge(0.70))+
  ggtitle("Accuracy and Kappa of the Galaxy Models")
