#####################################################################################################################################################
##################################### Support Vector Machines - Assignment  #########################################################################
#####################################################################################################################################################
# Submitted by : Vidhi Thakkar

####################################################################################################################################################
# -------------------------------------- 1. Business Understanding --------------------------------------------------------------------------------

# A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
# Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices.
# The goal is to develop a SVM model which should correctly identify the handwritten digits based on the pixel values given as features.

# a) Remove the previous data (if any)
rm(list=ls())

# b) Install necessary packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("stringr")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("caTools")
install.packages("kernlab")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("gmodels")


# c) Load the Packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(e1071)
library(caret)
library(caTools)
library(kernlab)
library(ggplot2)              
library(gridExtra)
library(gmodels)

# d) Set working directory
setwd("C:/Users/thakk/Desktop/SVM Assignment")

###############################################################################################################################################
# ------------------------------------- 2. Data Understanding ---------------------------------------------------------------------------------

# We use the MNIST data which is a large database of handwritten digits where we have pixel values of each digit along with its label. 
# The MNIST database of handwritten digits has a training set of 60,000 observations and a test set of 10,000 observations. 
# There are 784 columns which consist of 28*28 matrix describing the scanned image of the digits


# a) Load the file
mnist_train  <- read.csv("mnist_train.csv", stringsAsFactors = FALSE, header = F)
mnist_test   <- read.csv("mnist_test.csv", stringsAsFactors = FALSE, header = F)

# b) Let's check the structure of these data sets
str(mnist_train)   # 60,000 obs. of  785 variables, all dependant variables are integers
str(mnist_test)    # 10,000 obs. of  785 variables, all dependant variables are integers

# c) Let's look at the summary of some dependent variables
summary(mnist_test[ , 2:100]) 
summary(mnist_train[ , 2:100]) 
# Some columns contain only zeros, some in the range of 100 and maximum pixel values are 255
# Hence the data needs to be scaled

#################################################################################################################
# ------------------------------------- 3. Data Cleaning & Preparation --------------------------------------------

# a) Missing column name 
colnames(mnist_train)[1]  <- "Label"
colnames(mnist_test)[1]   <- "Label"

# View(mnist_train)
# View(mnist_test)

# b) Look for duplicate rows
sum(duplicated(mnist_test)) # no duplicate rows
sum(duplicated(mnist_train)) # no duplicate rows

# c) Check for Missing/Blank values
sapply(list(mnist_train, mnist_test), function(x) length(which(x == "")))
# There are no Missing values

# d) Look for NA values in three files: general_data, manager_survey_data, employee_survey_data
sapply(list(mnist_train, mnist_test), function(x) length(which(is.na(x))))
#  There are no NA's 

# e) Checking the first and last row in both the data sets
head(mnist_test, n=1) 
head(mnist_train,n=1)

tail(mnist_test, n=1) 
tail(mnist_train,n=1)

# f) Convert Digit (Independent variable) into factor
mnist_train$Label <- factor(mnist_train$Label)
summary(mnist_train$Label)

mnist_test$Label <- factor(mnist_test$Label)
summary(mnist_test$Label)

# g) Sampling training dataset
dim(mnist_train) # Understanding the dimensions

# Since 60k obervations will take a lot of computation time 
# We would subset the data to 5000 training observations using sample.split function
# Hence, SplitRatio = 5,000/60,000 = 0.083

set.seed(100)
sample_indices = sample.split(mnist_train$Label, SplitRatio = 0.083)
train = mnist_train[sample_indices,]  # 4982 obs of 785 variables
summary(train$Label)

# h) Scaling the data 

# Identify the Max value of pixels
max(train[ ,2:ncol(train)]) 

# Scaling the data using max value
train[ , 2:ncol(train)] <- train[ , 2:ncol(train)]/255
test <- cbind(Label = mnist_test[ ,1], mnist_test[ , 2:ncol(mnist_test)]/255)

# i) Check for NaN values in both the train & test data set
sum(apply(train,2,is.nan)) #None
sum(apply(test,2,is.nan))  #None

#################################################################################################################################
# ---------------------------------------- 4. Exploratory Data Analysis --------------------------------------------

# We can see that inspite of sampling the train data set to almost 5000 obs. frequencies of the digits has been retained. 
# Similar frequency is also observed in the test dataset
round(prop.table(table(mnist_train$Label)),3)
round(prop.table(table(train$Label)),3)
round(prop.table(table(test$Label)),3)
# Hence the class balance is maintained as expected


# Comparing the same using bar graph
plot1 <- ggplot(mnist_train, aes(Label)) +  geom_bar()  
plot2 <- ggplot(train, aes(Label)) +  geom_bar() 
plot3 <- ggplot(test, aes(Label)) +  geom_bar() 

grid.arrange(plot1, plot2, plot3, nrow = 3)

# Digit density
density_plot <- ggplot(mnist_train,aes(Label,fill=Label))+geom_density()
density_plot
# Digit '0' is the most intense of all

#################################################################################################################################
# ---------------------------------------- 5. Model Building & Evaluation --------------------------------------------

#--------------------------------------------- a) Linear Kernel ---------------------------------------------------------#

## 1. Linear kernel using default parameters (C = 1)

Model_linear_1 <- ksvm(Label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 1)
Eval_linear_1 <- predict(Model_linear_1, test, type = "response")

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear_1, test$Label) 

# Observations:

# Accuracy    : 90.8%
# Sensitivity > 81%
# Specificity > 98%

## 2.  Linear kernel using C = 10

Model_linear_2 <- ksvm(Label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 10)
Eval_linear_2 <- predict(Model_linear_2, test, type = "response")

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear_2, test$Label) 

# Observations:

# Accuracy    : 90.7%
# Sensitivity > 81%
# Specificity > 98%
# Model performance has slightly decreased

# --------------------------------------------------------------------------------------------------------------------------------
# 3. Using Cross-validation to Optimise C

# a) Create a trainControl() Scheme with method and num folds
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(100)


# b) Create a grid of hyperparameters to be tuned
# make a grid of c values to be tuned
grid_linear <- expand.grid(C= c(0.001, 0.1 ,1 ,10 ,100)) 

# c) Train the model across the grid and the k-folds
fit_linear <- train(Label ~ ., data = train, metric = metric, method = "svmLinear",
                    tuneGrid = grid_linear, preProcess = NULL,
                    trControl = trainControl, scale=FALSE)

# Printing results of 5 cross validation
print(fit_linear) 

# Plotting the results
plot(fit_linear)

# Observations:
# Best accuracy of 91.8% at C = 0.1
# Higher values of C are overfitting and lower values are giving simple models

# ------------------------------------------------------------------------------------------------------------
# Evaluating on the test data
model_optimal <- ksvm(Label~., data=train,scaled = FALSE, kernel = "vanilladot", C = 0.1 )

# Predicting the model results
eval_linear <- predict(fit_linear, test)

# Confusion Matrix - finding accuracy,sensitivity and specificity
confusionMatrix(eval_linear, test$Label)

# Observations:
# Overall accuracy of 92%, slightly imporved
# Specificities quite high > 98%
# Sensitivities > 86%, improved as compared to model at C=1


#--------------------------------------------- b) Radial Kernel --------------------------------------------------------------#

## 1. Radial kernel using default parameters

#Using RBF Kernel
Model_RBF_1 <- ksvm(Label~ ., data = train, scaled = FALSE, kernel = "rbfdot", C = 1, kpar = "automatic")

# Predict the model results
Eval_RBF_1 <- predict(Model_RBF_1, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF_1,test$Label)

# Observations:

# Accuracy   :  94.8%
# Specificity > 99%
# Sensitivity > 91%
# Increase in overall accuracy and sensitivty from linear kernel
# Data seems to have non linearity to it



#--------------------------------------------- c) Hyperparameter tuning and Cross Validation --------------------------------#

# Making grid of "sigma" and C values
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
# metric <- "Accuracy", tuneGrid = Grid of Parameters,
# trcontrol = trainControl,  #trainControl <- trainControl(method="cv", number=5)


# Performing 5 fold cross validation
fit.svm <- train(Label~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl, scale = FALSE)

# Printing cross validation results
print(fit.svm)

# We can look at the best tuned parameters
fit.svm$bestTune

#Best tuned at sigma = 0.01 & C = 3, Accuracy: (~)96%
# Higher sigma values are overfitting and lower sigma values are not capturing non linearity adequately
# Accuracy increases with C until 5 and then decreases again, can be further optimised
# Higher C values are overfitting and lower C values have too much bias

#Plotting model results
plot(fit.svm)

#-------------------------------------------------------------------------------------------------------------
# Validating the results on test data 
evaluate_non_linear <- predict(fit.svm,test)
confusionMatrix(evaluate_non_linear,test$Label)

# Accuracy    : 95.5%
# Sensitivity > 91% (high)
# Specificity > 99%

#####################################################################################################################################
# --------------------------------------------- 6. Conclusion ----------------------------------------------------------------------#
# Final model
final_model = fit.svm

## SVM using RBF kernel (C = 3 , sigma = 0.01) achieved highest accuracy in predicting digits

# reduced training data set of around 5000 instances (extracted using sample.split) has been used
# distribution of the dependent variable (digtits) has been preserved while sampling
# Model performance on validation data set of 10000 instances

# Accuracy = (~)96%
# Sensitivites > 91%
# Specificities > 99%
