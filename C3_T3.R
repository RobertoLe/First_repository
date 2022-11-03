# Descripción: Multiple regression of C3T3
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-03
##########################D#####################################################
library(caret)
library(readr)
existingProducts <- read_csv("R Data Sets/productattributes/existingproductattributes2017.csv")
newProducts <- read_csv("R Data Sets/productattributes/newproductattributes2017.csv")

head(existingProducts)
names(existingProducts)
summary(existingProducts)
str(existingProducts)

head(newProducts)
names(newProducts)
summary(newProducts)
str(newProducts)

# Dummies ######################################################################

# create dummies for existing products
newDataFrame <- dummyVars(" ~ .", data = existingProducts)
readyExist <- data.frame(predict(newDataFrame, newdata = existingProducts))

# create dummies for new products
newDataFrame1 <- dummyVars(" ~ .", data = newProducts)
readyNew <- data.frame(predict(newDataFrame, newdata = newProducts))

# Missing data ######################################################################

# check for missing data in columns in existing products
missingcols <- sapply(readyExist, function(x) {
  any(is.na(x))
})  
# tells me how many columns has missing data
sum(missingcols)

# checking for missing data in the new products
missingcols1 <- sapply(readyNew, function(x) {
  any(is.na(x))
})  
# tells me how many columns has missing data
sum(missingcols1)

# clearly the volume is missing because it is all 0s, but that we predict
summary(readyNew$Volume)

# it is a data frame that contains the rows with missing data of existing prod
new_DF <- readyExist[rowSums(is.na(readyExist)) > 0,]
new_DF

# checking the volume 
summary(readyExist$Volume)
hist(readyExist$Volume)

# checking the variable types and they are all numeric
str(readyExist)
str(readyNew)

# Drop the attribute with missing data that is the Best Seller Rank
readyExist$BestSellersRank <- NULL
readyNew$BestSellersRank <- NULL

# Drop the Product number because it is just the id and not relevant for prediction
readyExist$ProductNum <- NULL
readyNew$ProductNum <- NULL

# Correlations ######################################################################

corrData <- cor(readyExist)
corrData

# Split the data into two so I can see the correlations
core1 <- readyExist[,1:13]
core1 
core1['Volume'] <- readyExist$Volume

core2 <- readyExist[,14:27]
View(core2)

corrData1 <- cor(core1)
corrData1

corrData2 <- cor(core2)
corrData2

library(corrplot)
corrplot(corrData1)
corrplot(corrData2)

# The variables that have a strong correlation with the dependent variable are:
#         x5StarReviews: 1.00000000
#         x4StarReviews: 0.8790063940
#         x3StarReviews: 0.76337319
#         x2StarReviews: 0.48727933 # this has a moderate correlation
# PositiveServiceReview: 0.62226022
#ProductTypeGameConsole: 0.38829824 # this has a weak correlation

# Split ######################################################################

set.seed(999)

# split data into 75% training and 25% testing
inTraining <- createDataPartition(readyExist$Volume, p = .75, list = FALSE)
training <- readyExist[inTraining,]
testing <- readyExist[-inTraining,]

# Random Forest ######################################################################

library(randomForest)
# randomm forest
system.time({rfm1 <- randomForest(Volume ~ ., data = training, mtry = 4,
                     importance = TRUE)
})

rfm1 

#  Mean of squared residuals: 550197.8
#            % Var explained: 46.87

library(dplyr)

i_scores <- varImp(rfm1, conditional=TRUE)
#Gathering rownames in 'var'  and converting it to the factor
#to provide 'fill' parameter for the bar chart. 
i_scores <- i_scores %>% tibble::rownames_to_column("var") 
i_scores$var<- i_scores$var %>% as.factor()
#Plotting the bar and polar charts for comparing variables
i_bar <- ggplot(data = i_scores) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=Overall, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL) +
  ggtitle("Random Forest Variable Importance")

# i_bar + coord_polar() + theme_minimal()
i_bar + coord_flip() + theme_minimal()


prf1 <- predict(rfm1, testing)
postResample(prf1, testing$Volume)

# RMSE              Rsquared          MAE 
# 1804.8486870      0.7051622         491.7111560 

# second model of rf
system.time({rfm2 <- randomForest(Volume ~ x5StarReviews + x4StarReviews +
                                  x3StarReviews + x2StarReviews +
                                  x1StarReviews + PositiveServiceReview, 
                                  data = training, mtry = 4,
                                  importance = TRUE)
})

rfm2

#  Mean of squared residuals: 526009.7
#            % Var explained: 49.2

prf2 <- predict(rfm2, testing)
postResample(prf2, testing$Volume)

# RMSE              Rsquared          MAE 
# 1466.2004124      0.7710457         426.3461404 

# rfm2 is better

i_scores <- varImp(rfm2, conditional=TRUE)
#Gathering rownames in 'var'  and converting it to the factor
#to provide 'fill' parameter for the bar chart. 
i_scores <- i_scores %>% tibble::rownames_to_column("var") 
i_scores$var<- i_scores$var %>% as.factor()
#Plotting the bar and polar charts for comparing variables
i_bar <- ggplot(data = i_scores) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=Overall, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL) +
  ggtitle("Random Forest 2 Variable Importance")

# i_bar + coord_polar() + theme_minimal()
i_bar + coord_flip() + theme_minimal()

# Support Vector Machine ######################################################################

library(e1071)

set.seed(1492)

svm1 <- svm(Volume~., data=training)

svm1

psvm1 <- predict(svm1, testing)
postResample(psvm1,testing$Volume)

# RMSE              Rsquared          MAE 
# 2383.9411284      0.1734086         764.1393350 

x <- 1:length(testing$Volume)
plot(x, testing$Volume, pch=18, col="red")
lines(x, psvm1, lwd="1", col="blue")



svm2 <- svm(Volume ~ x5StarReviews + x4StarReviews +
                       x3StarReviews + x2StarReviews +
                       x1StarReviews + PositiveServiceReview, 
                     data = training)

svm2

psvm2 <- predict(svm2, testing)
postResample(psvm2,testing$Volume)

# RMSE              Rsquared          MAE 
# 2235.0525453      0.3942749         601.6722967

x <- 1:length(testing$Volume)
plot(x, testing$Volume, pch=18, col="red")
lines(x, psvm2, lwd="1", col="blue")

# A lot better, but still lacking

# Gradient Boosting ######################################################################
library(gbm)

gbm1 <- gbm(training$Volume ~.,
                data = training,
                distribution = "gaussian",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)

gbm1

summary(gbm1)

pgbm1 <- predict(gbm1, testing)
postResample(pgbm1,testing$Volume)

# RMSE              Rsquared          MAE 
# 2197.5745103      0.3300658         673.8513096

gbm2 <- gbm(training$Volume ~x5StarReviews + x4StarReviews +
              x3StarReviews + x2StarReviews +
              x1StarReviews + PositiveServiceReview,
            data = training,
            distribution = "gaussian",
            cv.folds = 10,
            shrinkage = .01,
            n.minobsinnode = 10,
            n.trees = 500)

gbm2

summary(gbm2)

pgbm2 <- predict(gbm2, testing)
postResample(pgbm2,testing$Volume)

# RMSE              Rsquared          MAE 
# 2167.2850344      0.3530976         613.9373128

# Make predictions ######################################################################

# Use rfm2 because it has the best metrics

finalPredrfm2 <- predict(rfm2, readyNew)
summary(finalPredrfm2)
finalPredrfm2

readyNew$Volume <- finalPredrfm2

View(readyNew)

plot(readyNew$Volume)

NewPPredictions <- readyNew

# write.csv(NewPPredictions, file="C2_T3_Predictions.csv", row.names = TRUE)

PC <- filter(readyNew,ProductTypePC == 1)
Laptops <- filter(readyNew,ProductTypeLaptop == 1)
Netbooks <- filter(readyNew,ProductTypeNetbook == 1)
Smartphones <- filter(readyNew,ProductTypeSmartphone == 1)

Electronics <- PC

Electronics <- rbind(Electronics, Laptops)
Electronics <- rbind(Electronics, Netbooks, Smartphones)

# Focus on PC, Laptops, Netbooks and Smartphones
# write.csv(Electronics, file="C2_T3_Electronics_Predictions.csv", row.names = TRUE)

names(readyNew)

sum(PC$Volume)
sum(Laptops$Volume)
sum(Netbooks$Volume)
sum(Smartphones$Volume)


