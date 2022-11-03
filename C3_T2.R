# Descripción: Classification models and predictions of C3_T2
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-07-30
##########################D#####################################################
library(fastDummies)
library(mlbench)
library(caret)
library(readr)
library('randomForest')

CompleteResponses <- read_csv("R Tutorial Data Sets/SurveyData/CompleteResponses.csv")

summary(CompleteResponses)
summary(CompleteResponses$elevel)

# Pre-processing ######################################################################

sum(is.na(CompleteResponses)) # no missing values

CompleteResponses$elevel <- as.factor(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
CompleteResponses$age <- as.integer(CompleteResponses$age)

str(CompleteResponses)

summary(CompleteResponses)

# Dummies ######################################################################


#dataDUM <- CompleteResponses

#dataDUM <- dummy_cols(dataDUM, select_columns = 
#                      c('elevel','car','zipcode','brand'),
#                     remove_selected_columns = TRUE)
#names(dataDUM)
#str(dataDUM)



#correlationMatrix <- cor(dataDUM[,1:39])
#print(correlationMatrix)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5, 
#                                    names = TRUE, verbose = TRUE)
#print(highlyCorrelated)

# removed the column because it is highly correlated and because I just need one
# response variable.
#dataDUM$brand_0 <- NULL



# Split ######################################################################
set.seed(999)

inTrain <- createDataPartition(y = dataDUM$brand_1,
                               ## the outcome data are needed
                               p = .75,
                               ## The percentage of data in the
                               ## training set
                               list = FALSE)

trainSet <- dataDUM[inTrain,]
testSet <- dataDUM[-inTrain,]


# Feature Selection ######################################################################


# set predictors         
predictors <- names(trainSet)[names(trainSet) != "brand_1"]
predictors







# RandomForest Install ######################################################################

# I went to the old archives of the random forest to install it
# urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-14.tar.gz"

# install.packages(urlPackage, repos=NULL, type="source") 

#  Caret Tutorial ######################################################################

# this took forever and didn't work
# install.packages("caret", dependencies = c("Depends", "Suggests"))

# this worked to install caret
# install.packages('caret', dependencies = TRUE)


# Here I load the libraries and split the data into a training and testing set.
library(caret)
library(mlbench)
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,
                                  ## the outcome data are needed
                                    p = .75,
                                  ## The percentage of data in the
                                    ## training set
                                    list = FALSE)
## The format of the results
  
  ## The output is a set of integers for the rows of Sonar
  ## that belong in the training set.
  str(inTrain)

  training <- Sonar[ inTrain,]
  testing <- Sonar[-inTrain,]
  nrow(training) 
  
  # This is a partial least squares discriminant analysis (PLSDA) model
  # This helps you select components that should be retained
  
  plsFit <- train(Class ~ .,
                   data = training,
                   method = "pls",
                   ## Center and scale the predictors for the training
                     ## set and all future samples.
                     preProc = c("center", "scale"))
  nrow(testing)
  
  # Expand the set of PLS models that the function evaluates. 
  # By default, the function will tune over three values of each 
  # tuning parameter.
  
  plsFit <- train(Class ~ .,
                   data = training,
                   method = "pls",
                   tuneLength = 15,
                   preProc = c("center", "scale"))
  
 # The type of re-sampling used. The simple bootstrap is used by default. 
 # We will have the function use three repeats of 10–fold cross–validation.
  
  ctrl <- trainControl(method = "repeatedcv",
                        repeats = 3)
   plsFit <- train(Class ~ .,
                     data = training,
                     method = "pls",
                     tuneLength = 15,
                     trControl = ctrl,
                     preProc = c("center", "scale"))
   
  # The methods for measuring performance. If unspecified, overall accuracy 
  # and the Kappa statistic are computed. 
  # For regression models, root mean squared error and R2 are computed.
  # Here, the function will be altered to estimate the area under 
  # the ROC curve, the sensitivity and specificity.
  
   ctrl <- trainControl(method = "repeatedcv",
                         repeats = 3,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)
    plsFit <- train(Class ~ .,
                      data = training,
                      method = "pls",
                      tuneLength = 15,
                      trControl = ctrl,
                      metric = "ROC",
                      preProc = c("center", "scale"))
    plsFit
    
    
    plsClasses <- predict(plsFit, newdata = testing)
     str(plsClasses)
     
     plsProbs <- predict(plsFit, newdata = testing, type = "prob")
      head(plsProbs)
      
      plot(plsFit)

      confusionMatrix(data = plsClasses, testing$Class)     
      
      ## To illustrate, a custom grid is used
       rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
       set.seed(123)
       rdaFit <- train(Class ~ .,
                         data = training,
                         method = "rda",
                         tuneGrid = rdaGrid,
                         trControl = ctrl,
                         metric = "ROC")
       rdaFit
       
       rdaClasses <- predict(rdaFit, newdata = testing)
        confusionMatrix(rdaClasses, testing$Class)
       
         resamps <- resamples(list(pls = plsFit, rda = rdaFit))
         summary(resamps)
       
         diffs <- diff(resamps)
          summary(diffs)
          
          xyplot(resamps, what = "BlandAltman")   
          
           
          
          
          
      