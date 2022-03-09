
## FIT3152 Assignment 2
## Last Modified date: 21th May 2021
## Student ID: 28280016

# Load necessary libraries
library(tree)
library(e1071)
library(ROCR)
library(randomForest)
library(rpart)
library(adabag)
library(ggplot2)
#library(neuralnet)
library(dplyr)
library(pROC)

# Set up data root path
setwd("~/Desktop/FIT3152/Assignment 2")

rm(list = ls())
WAUS <- read.csv("CloudPredict2021.csv")
# nrow(WAUS)

# Q.1 Data exploration
str(WAUS)
summary(WAUS)
cloudy_freq_table <- as.data.frame(xtabs(~CloudTomorrow,WAUS))
cbind(cloudy_freq_table, Percent = prop.table(cloudy_freq_table$Freq)*100)

# Check if there are any missing and NAs value
any(is.na(WAUS))    # The answer is Yes

count_NAs <- apply(WAUS, MARGIN = 2, function(col) sum(is.na(col)))

# Remove the NAs rows first
data <- WAUS[complete.cases(WAUS),] # Remove missing data for mean,std description
min(data$Year)
max(data$Year)

# Calculate the std for each column
std_col <- apply(data[,c(5:9,11,14:21)],2,sd)
mean_col <- apply(data[,c(5:9,11,14:21)],2,mean)
summary(data)

## Normalize the data / Regularisation
normalisation = function(col){
  max_value = max(col)
  min_value = min(col)
  result = (col - min_value)/(max_value - min_value)
  return (result)
}


# Remove few columns
WAUS[,c("WindGustDir","WindDir9am","WindDir3pm")] = NULL

# Convert categorical data columns into numeric data type
WAUS$RainToday <- as.factor(WAUS$RainToday)

# observe the effect of independent variables (inputs) on the dependent variable (output)

## Pre - processing deal with NAs/missing data.
WAUS <- WAUS[complete.cases(WAUS),] # Remove missing data from the beginning

# Q 2 Document pre-processing required parts 
# Random places 
L <- as.data.frame(c(1:49))
set.seed(28280016) # My student ID:28280016
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows


## Q.3 Partition on to training and test data
set.seed(28280016) #Student ID as random seed 
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS)) 
WAUS_training = WAUS[train.row,]
WAUS_testing = WAUS[-train.row,]

nrow(WAUS_training)
nrow(WAUS_testing)

# Extract Day, Month, Year of the observation
WAUS_training <- WAUS_training[,c(4:20)]
WAUS_testing <- WAUS_testing[,c(4:20)]

# Resolve the argument of length 0 error
WAUS_training$CloudTomorrow <- as.factor(WAUS_training$CloudTomorrow)
WAUS_testing$CloudTomorrow <- as.factor(WAUS_testing$CloudTomorrow)

## Q.4 Implement a classification model via different techniques
## Decision Tree
WAUS.tree <- tree(CloudTomorrow~.,data = WAUS_training)
summary(WAUS.tree)
# Running some plot 
plot(WAUS.tree)
text(WAUS.tree,pretty = 0)

## Naïve Bayes
WAUS.bayes <- naiveBayes(CloudTomorrow~.,data = WAUS_training)
summary(WAUS.bayes)

## Bagging 
WAUS.bag <- bagging(CloudTomorrow~.,data = WAUS_training,mfinal = 5)
summary(WAUS.bag)

## Boosting 
WAUS.Boost <- boosting(CloudTomorrow~., data = WAUS_training, mfinal = 5)
summary(WAUS.Boost)

## Random Forests
WAUS.rf <- randomForest(CloudTomorrow~., data = WAUS_training)
summary(WAUS.rf)


# Q.5 using test data, classify each of the test cases.
# Q.6.Using test data, calculate confidence of predicting 'cloudy tomorrow'
# Construct ROC for each classifier.
# to compute accuracy
# Step 1: Create confusion matrix for each method 
# Step 2 : (TP + TN)/(TP+TN+FP+FN) = Accuracy

# Decision Tree testing
WAUS.predtree <- predict(WAUS.tree,WAUS_testing,type = "class")
tree_table <- table(Predicted_Class = WAUS.predtree, Actual_Class = WAUS_testing$CloudTomorrow)
cat("\n#Decision Tree Confusion Matrix\n")
tree_table
tree_accuracy <- (sum(diag(tree_table))/sum(tree_table))*100

detach(package:neuralnet)   # To solve some crash errors.

# Do predicitions as classes and draw a table
WAUS.pred.tree <- predict(WAUS.tree,WAUS_testing,type = "vector")
# Computing a simple ROC curve
# labels are actual values, predictors are probability of class
WAUSpred_tree <- prediction(WAUS.pred.tree[,2],WAUS_testing$CloudTomorrow)
WAUSperf_tree <- performance(WAUSpred_tree,"tpr","fpr")

# calculate auc for trees
auc_tree <- performance(WAUSpred_tree,"auc")
print(as.numeric(auc_tree@y.values))

plot(WAUSperf_tree,col="purple")
abline(0,1)

## Naïve Bayes testing
WAUS.predbayes = predict(WAUS.bayes,WAUS_testing)
naiveBayes_table = table(Predicted_Class = WAUS.predbayes, Actual_Class = WAUS_testing$CloudTomorrow)
cat("\n#Naïve Bayes Confusion Matrix\n")
naiveBayes_table
naiveBayes_accuracy <- (sum(diag(naiveBayes_table))/sum(naiveBayes_table))*100

# Outputs as confidence levels
WAUSpred.bayes <- predict(WAUS.bayes,WAUS_testing,type = "raw")
WAUSpred <- prediction(WAUSpred.bayes[,2], WAUS_testing$CloudTomorrow)
WAUSperf_naive <- performance(WAUSpred,"tpr","fpr")
plot(WAUSperf_naive,add = TRUE,col = 'blueviolet')

## Bagging testing
WAUSpred.bag <- predict.bagging(WAUS.bag,WAUS_testing)
WAUSpred.bag$confusion
bagging_accuracy <- sum((diag(WAUSpred.bag$confusion)))/sum(WAUSpred.bag$confusion)*100

WAUSBagpred <- prediction(WAUSpred.bag$prob[,2],WAUS_testing$CloudTomorrow)
WAUSBagperf <- performance(WAUSBagpred,"tpr","fpr")
                           
# calculate auc for Bagging
auc_bagging <- performance(WAUSBagpred,"auc")
print(as.numeric(auc_bagging@y.values))
plot(WAUSBagperf,add=TRUE,col='blue')
cat("\n#Bagging Confusion\n")
print(WAUSpred.bag$confusion)

## Boosting testing
WAUSpred.boost <- predict.boosting(WAUS.Boost,newdata = WAUS_testing)
WAUSpred.boost$confusion
boosting_accuracy <- sum((diag(WAUSpred.boost$confusion)))/sum(WAUSpred.boost$confusion)*100
WAUSBoostpred <- prediction(WAUSpred.boost$prob[,2],WAUS_testing$CloudTomorrow)

auc_boosting <- performance(WAUSBoostpred,"auc")
print(as.numeric(auc_boosting@y.values))

WAUSBoostperf <- performance(WAUSBoostpred,"tpr","fpr")
plot(WAUSBoostperf,add = TRUE,col = 'red')
cat("\n#Boosting Confusion\n")
print(WAUSpred.boost$confusion)

## Random Forest
WAUSpredrf <- predict(WAUS.rf, WAUS_testing)
WAUSRandF <- table(Predicted_Class = WAUSpredrf, Acutual_Class = WAUS_testing$CloudTomorrow) # Confusion Matrix
cat("\n#Random Forest Confusion Matrix\n")
print(WAUSRandF)
randomForest_accuracy <- sum(diag(WAUSRandF))/sum(WAUSRandF)*100

WAUSpred.rf <- predict(WAUS.rf,WAUS_testing,type = "prob")

#WAUSpred.rf
WAUSRFpred <- prediction(WAUSpred.rf[,2],WAUS_testing$CloudTomorrow)

auc_randomForest <- performance(WAUSBagpred,"auc")
print(as.numeric(auc_randomForest@y.values))
WAUSRFperf <- performance(WAUSRFpred,"tpr","fpr")
plot(WAUSRFperf,add = TRUE,col = "darkgreen")

legend("topleft",legend=c("Decision Trees","Naïve Bayes","Bagging", "Boosting","Random Forest"),
       col=c("purple","blueviolet","blue", "red","darkgreen"), lty=1:2, cex=0.8)

# Add a straight line onto diagram
abline(0,1)

# Q.7 Comparisons between task 5 and task 6 for all classifiers.
# Combine accuracy results from previous tasks
accuracy_list <- c(tree_accuracy,naiveBayes_accuracy,bagging_accuracy,boosting_accuracy,randomForest_accuracy)
summary_models <- rbind(Accuray = accuracy_list)
colnames(summary_models) <- c("Decision Trees","Naïve Bayes","Bagging", "Boosting","Random Forest")
summary_models


# Q.8. Examine each models, determine most important variables for prediction
summary(WAUS.tree)
plot(WAUS.tree)
text(WAUS.tree,pretty = 0)

sort(WAUS.bag$importance,decreasing=TRUE)

sort(WAUS.Boost$importance,decreasing =TRUE)

#sort(WAUS.rf$importance,decreasing = TRUE)
WAUS.rf$importance[order(-WAUS.rf$importance),]

## Based on these check, WindSpeed9am, RainToday,WindGustSpeed,WindSpeed3pm,Temp3pm are top 5 least important attributes
# remove these attributes from data-set
#WAUS_training[,c("RainToday","WindSpeed9am","WindGustSpeed","Temp3pm","WindSpeed3pm")] = NULL

#WAUS_testing[,c("RainToday","WindSpeed9am","WindGustSpeed","Temp3pm","WindSpeed3pm")] <- NULL

# Q.9.Create a simpler classifier
set.seed(28280016)
WAUS_simple <- WAUS[sample(nrow(WAUS), 10, replace = FALSE),] # sample 10 rows
simpler_data <- WAUS_simple[,c(5,9,15,16,14,20)]

train_simple.row = sample(1:nrow(simpler_data), 0.7*nrow(simpler_data)) 
WAUS_simple_training = simpler_data[train_simple.row,]
WAUS_simple_testing = simpler_data[-train_simple.row,]


# Q.10 improve the performance 
# K-fold cross validation on decision tree
WAUS_new.tree <- cv.tree(WAUS.tree,FUN = prune.misclass)
# Punning trees
WAUS_prunned.tree <- prune.misclass(WAUS.tree,best = 2)
summary(WAUS_prunned.tree)
plot(WAUS_prunned.tree)
text(WAUS_prunned.tree,pretty = 0)

# Conduct prediction
WAUS_prunned.predtree <- predict(WAUS_prunned.tree,WAUS_testing,type = "class")
treeprunned_table <- table(Predicted_Class = WAUS_prunned.predtree, Actual_Class = WAUS_testing$CloudTomorrow)
cat("\n#Decision Tree after Prunning Confusion Matrix\n")
treeprunned_table
treeprunned_accuracy <- (sum(diag(treeprunned_table))/sum(treeprunned_table))*100


# Q. 10  Create the best tree-based classifier
# Based on accuracy table, Boosting and Random Forest are chosen as the highest accuracy.
set.seed(28280016)
data_best_training <- WAUS_training[,c(6,13,17,11,2,12)]
data_best_testing <- WAUS_testing[,c(6,13,17,11,2,12)]

WAUS_best.Boost <- boosting(CloudTomorrow~., data = data_best_training, mfinal = 10)
# Run the boosting model
## Boosting testing
WAUSpred_best.boost <- predict.boosting(WAUS_best.Boost,newdata = data_best_testing)
WAUSpred_best.boost$confusion
boosting_best_accuracy <- sum((diag(WAUSpred_best.boost$confusion)))/sum(WAUSpred_best.boost$confusion)*100
WAUSBoostpred_best <- prediction(WAUSpred_best.boost$prob[,2],data_best_testing$CloudTomorrow)
auc_boosting_best <- performance(WAUSBoostpred_best,"auc")
print(as.numeric(auc_boosting_best@y.values))
WAUSBoostperf_best <- performance(WAUSBoostpred_best,"tpr","fpr")

# Run cross validation for boosting
WAUS.boostingcv <- boosting.cv(CloudTomorrow~.,data = data_best_training,boos = TRUE,mfinal = 100,coeflearn = "Breiman",control=rpart.control(cp=0.01))
WAUS.boostingcv$confusion
WAUS.boostingcv$error


# Q.11 Build ANN
WAUS_new <- WAUS
WAUS_new$CloudTomorrow <- as.factor(WAUS_new$CloudTomorrow)

# Select some important variables manually
WAUS_new <- WAUS_new[,c(5,8,9,13,14,15,16,17,20)]

# Normalise data
WAUS_new_norm <- as.data.frame(lapply(WAUS_new[1:7], normalisation))
WAUS_new_norm <-cbind(WAUS_new_norm,WAUS_new$CloudTomorrow)
colnames(WAUS_new_norm)[8] <-"CloudTomorrow"

# Split data into train and test
set.seed(28280016)
train.row <-sample(1:nrow(WAUS_new_norm),0.7*nrow(WAUS_new_norm))
WAUS_new.train <- WAUS_new_norm[train.row,]
WAUS_new.test <- WAUS_new_norm[-train.row,]

# Build up an ANN
library(neuralnet)
WAUS.nn <- neuralnet(CloudTomorrow~.,WAUS_new.train,hidden = 1)
# Visualize Artificial Neural Network
plot(WAUS.nn)
WAUS.nn$result.matrix
WAUS.nn$net.result

WAUS_nn.predict <- compute(WAUS.nn,WAUS_new.test)
WAUS_nn.predr <- round(WAUS_nn.predict$net.result,0)
WAUS_nn.predrdf <- as.data.frame(as.table(WAUS_nn.predr))
WAUS_nn.predrdf <- WAUS_nn.predrdf[!WAUS_nn.predrdf$Freq == 0,]
WAUS_nn.predrdf$Freq = NULL
colnames(WAUS_nn.predrdf) <- c("Obs","CloudTomorrow")
WAUS_nn.predrdf <- WAUS_nn.predrdf[order(WAUS_nn.predrdf$Obs),]

# Create the confusion matrix and calculate the accuracy
WAUS_nn_cm <- table(observed = WAUS_new.test$CloudTomorrow,predicted = WAUS_nn.predrdf$CloudTomorrow)
WAUS_nn_cm
WAUS_nn_accuracy <- round(sum(diag(WAUS_nn_cm))/sum(WAUS_nn_cm)*100,3)
WAUS_nn_accuracy

