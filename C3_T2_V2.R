# Descripción: Second try of C3_T2 with the caret classification models
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-01
##########################D#####################################################

# read data
library(readr)
CompleteResponses <- read_csv("R Tutorial Data Sets/SurveyData/CompleteResponses.csv")
SurveyIncomplete <- read_csv("R Tutorial Data Sets/SurveyData/SurveyIncomplete.csv")


# drop features that are highly correlated
correlationMatrix <- cor(CompleteResponses[,1:7])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5, 
                                    names = TRUE, verbose = TRUE)
print(highlyCorrelated)
# didn't drap any

# change the brand to factor
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)

# check brand data
summary(CompleteResponses$brand)
summary(SurveyIncomplete$brand)

library(caret)
library(gbm)

set.seed(99)
# split data
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

# cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# Stochastic Gradient Boosting first iteration
gbmFit1 <- train(brand~., data = training, 
                method = "gbm", 
                trControl=fitControl,
                tuneLength = 5)
gbmFit1

plot(varImp(gbmFit1))

gbmFit1$finalModel$tuneValue
summary(gbmFit1)
plot(gbmFit1)

set.seed(88)
# Random Forest first iteration
system.time({
rfFit1 <- train(brand~., data = training, 
                 method = "rf", 
                 trControl=fitControl,
                 tuneLength = 5)
})# 150 seconds

rfFit1

varImp(rfFit1)

rfFit1$finalModel$tuneValue
summary(rfFit1$results)
plot(rfFit1)

library(C50)
set.seed(78)
# C5.0 first iteration
system.time({
  c50Fit1 <- train(brand~., data = training, 
                  method = "C5.0", 
                  trControl=fitControl,
                  tuneLength = 5)
})# 150 seconds

c50Fit1

varImp(c50Fit1)

c50Fit1$finalModel$tuneValue
summary(c50Fit1$results)
plot(c50Fit1)

set.seed(123)
# C5.0 second iteration
system.time({
  c50Fit2 <- train(brand~., data = training, 
                   method = "C5.0", 
                   trControl=fitControl,
                   tuneLength = 5)
})# 150 seconds

c50Fit2

varImp(c50Fit2)


c50Fit2$finalModel$tuneValue
summary(c50Fit2)
plot(c50Fit2)

plot(varImp(c50Fit2))
plot(varImp(c50Fit1))

# Random Forest prediction on testing set
testPredrf1 <- predict(rfFit1, testing)
postResample(testPredrf1, testing$brand)
# Random Forest confusion matirx
cmrf1 <- confusionMatrix(testPredrf1, testing$brand)

# Random Forest prediction on corrupted data
finalPredrf1 <- predict(rfFit1, SurveyIncomplete)
summary(finalPredrf1)

# C5.0 prediction on testing set
testPredc501 <- predict(c50Fit1, testing)
postResample(testPredc501, testing$brand)
# C5.0 confusion matirx
cmc501 <- confusionMatrix(testPredrf1, testing$brand)

# C5.0 prediction on corrupted data
finalPredc501 <- predict(c50Fit1, SurveyIncomplete)
summary(finalPredc501)

# Stochastic Gradient Boosting prediction on testing set
testPredgbm1 <- predict(gbmFit1, testing)
postResample(testPredgbm1, testing$brand)
# Stochastic Gradient Boosting confusion matirx
cmgbm1 <- confusionMatrix(testPredgbm1, testing$brand)

# Stochastic Gradient Boosting prediction on corrupted data
finalPredgbm1 <- predict(gbmFit1, SurveyIncomplete)
summary(finalPredgbm1)


# Data frame that has the accuracy
ModelType <- c( "Stochastic Gradient Boosting","Random forest",
               "C50")

TrainAccuracy <- c(max(gbmFit1$results$Accuracy), max(rfFit1$results$Accuracy),
                   max(c50Fit1$results$Accuracy))

Train_missclass_Error <- 1 - TrainAccuracy


TestingAccuracy <- c(cmgbm1$overall[1], cmrf1$overall[1], cmc501$overall[1])

Testing_missclass_Error <- 1 - TestingAccuracy

metrics <- data.frame(ModelType, TrainAccuracy, 
                      Train_missclass_Error, TestingAccuracy, 
                      Testing_missclass_Error)

metrics

# GBM shows the best accuracy and Kappa

FilledSurvey <- SurveyIncomplete

FilledSurvey$brand  <- finalPredgbm1

summary(FilledSurvey$brand)

# Trying to add the predicted results and the complete data
dataPreferences <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("brand")
colnames(dataPreferences) <- x

dataPreferences1 <- data.frame(c(FilledSurvey$brand))
colnames(dataPreferences1) <- x
dataPreferences2 <- data.frame(c(CompleteResponses$brand))
colnames(dataPreferences2) <- x
dataPreferences <- rbind(dataPreferences1,dataPreferences2)

str(dataPreferences)

hist(dataPreferences$brand)

table(dataPreferences$brand)

# checking how many I have
brand_counts <- table(dataPreferences$brand)
brand_counts / sum(brand_counts) *100

# first attempt to plot
bar_plt <- ggplot(dataPreferences, aes(x = brand)) 

# second attempt to plot
bar_plt <- bar_plt + geom_bar()
summary(bar_plt) 

# Show plot, but it is ugly
bar_plt

# Change names of the factors in brand
ords <- c("Acer", "Sony")
dataPreferences$brand <- as.factor(dataPreferences$brand)
levels(dataPreferences$brand) <- c("Acer", "Sony")

# Barplot of all the data
myplt <- ggplot(dataPreferences, aes(x = brand)) + 
  geom_bar(fill = "orange", width = 0.7) + 
  scale_x_discrete(limits = ords) +
  coord_flip() +
  xlab("Brand Preference") + ylab("Number of Observations") +
  ggtitle("Total brand preferences")+
  theme_classic()

myplt

# Barplot of the predicted values
myplt1 <- ggplot(FilledSurvey, aes(x = brand)) + 
  geom_bar(fill = "#FF6666", width = 0.7) + 
  scale_x_discrete(limits = ords) +
  coord_flip() +
  xlab("Brand Preference") + ylab("Number of Observations") +
  ggtitle("Generated brand preferences")+
  theme_classic()

myplt1

FilledSurvey$brand <- as.factor(FilledSurvey$brand)
levels(FilledSurvey$brand) <- c("Acer", "Sony")

# exported all data to excel just in case
library("writexl")
write_xlsx(dataPreferences,"Brand_Preferences.xlsx")
