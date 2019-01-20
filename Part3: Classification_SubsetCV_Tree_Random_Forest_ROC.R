########################################
## Andy Dingler
## PREDICT 422
## Charity Project - Part 2 (Regression Problem)
########################################


########################################
## Exercise 1 - Imprt Data
########################################

# read the data into R
classData = read.csv("trainSample.csv", na.strings=c("NA"," "))
source("Dingler_DataPreparation.R")
classData <- processPart(classData)

########################################
## Exercise 2 - Data Preparation
########################################

# re-categorizing variables
# Convert categorical variables to factors
classData$DONR = as.factor(classData$DONR)
classData$HOME = as.factor(classData$HOME)
classData$HINC = as.factor(classData$HINC)

# removed the observations that have neighborhood values of zero
classData_filtered <- classData[classData$MEDAGE > 0, ]

# addressing missing values
table(classData_filtered$HOME, useNA = "ifany")
table(classData_filtered$HINC, useNA = "ifany")
table(classData_filtered$GENDER, useNA = "ifany")

# The unknown gender categorical option 
# replace the A and C genders to unkown
classData_filtered$GENDER <- replace(classData_filtered$GENDER, classData_filtered$GENDER == 'A', NA)
classData_filtered$GENDER <- replace(classData_filtered$GENDER, classData_filtered$GENDER == 'C', NA)
classData_filtered$GENDER <- replace(classData_filtered$GENDER, classData_filtered$GENDER == 'U', NA)
classData_filtered$GENDER <- factor(classData_filtered$GENDER, levels = c("F", "J", "M"))
# levels(charityData_donrs_filtered$GENDER)

table(classData_filtered$DONR)
library(mice)
charityData_donrs_mice <- mice(classData_filtered, m = 1, method = 'cart', printFlag = FALSE, maxit = 5)
classData_filtered_imputed <- complete(charityData_donrs_mice) 


# transforming variables

# Note: You may want to separate RFA Values (R = recency, F = frequency, A = amount)
# into separate R, F, and A variables. I wrote a function (separateRFA) to perform 
# these steps.
separateRFA = function(xData,varName)
{
        bytes = c("R","F","A")
        newVarNames = paste(varName,bytes, sep="_")
        
        for (ii in 1:length(bytes)) # Loop over 1 to 3 (corresponding to R, F, and A)
        {
                # Find the unique values for current byte
                byteVals = unique(substr(levels(xData[,varName]),ii,ii))
                
                for (jj in 1:length(byteVals)) # Loop over unique byte values
                {
                        rowIdx = substr(xData[,varName],ii,ii) == byteVals[jj]
                        xData[rowIdx,newVarNames[ii]] = byteVals[jj]
                }
                
                xData[,newVarNames[ii]] = factor(xData[,newVarNames[ii]])
        }
        
        return(xData)
}

# Apply separateRFA to the variables RFA_96
classData_filtered_imputed = separateRFA(classData_filtered_imputed,"RFA_96")
# remove old variable RFA_96
old_vars <- c("RFA_96", "DAMT", "ID")
classData <- classData[ , !(names(classData) %in% old_vars)]

# B and C can be merged into factor value of D
classData_filtered_imputed$RFA_96_A <- replace(classData_filtered_imputed$RFA_96_A, classData_filtered_imputed$RFA_96_A == 'B', 'D')
classData_filtered_imputed$RFA_96_A <- replace(classData_filtered_imputed$RFA_96_A, classData_filtered_imputed$RFA_96_A == 'C', 'D')
classData_filtered_imputed$RFA_96_A <- factor(classData_filtered_imputed$RFA_96_A)

## make all factor variables valid R names
feature.names=names(classData_filtered_imputed)

for (f in feature.names) {
        if (class(classData_filtered_imputed[[f]])=="factor") {
                levels <- unique(c(classData_filtered_imputed[[f]]))
                classData_filtered_imputed[[f]] <- factor(classData_filtered_imputed[[f]],
                                             labels=make.names(levels))
        }
}


########################################
## Exercise 3 - Dataset Partitioning
########################################

# Hold-out Test Set
set.seed(8)
train <- sample(nrow(classData), nrow(classData) * 0.75, replace=FALSE)
charity_train <- classData[train,]
charity_test <- classData[-train,]

# number of observations in Test
nrow(charity_test)
table(charity_test$DONR) / length(charity_test$DONR)
# number of observations in Train
nrow(charity_train)
table(charity_train$DONR) / length(charity_train$DONR)

########################################
## Exercise 4 - Model Fitting
########################################
# Load packages required for this code.
library(pROC)
library(lift)
library(MASS)
library(rpart)

## Part A - Simple Logistic Regression
modelA1 = glm(DONR ~ LASTGIFT, data=charity_train, family=binomial)
summary(modelA1)
par(mfrow=c(2,2))
plot(modelA1)
par(mfrow=c(1,1))

trnProbsA1 = predict(modelA1,type="response")
hist(trnProbsA1,col="gray")   # Note that scores are distributed around 0.05.

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA1 = roc(response=charity_train$DONR, predictor=trnProbsA1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA1,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA1$auc,digits=3),sep=""))
par(pty="m")

simple_train_auc <- rocA1$auc
# Classification: Determine "optimal" threshold.

# Note: Since the ROC curve is plotted with Specificity on the horizontal axis
# (instead of FP, where FP = 1-Specificity) and the horizontal axis goes
# from 1 down to 0, I will be using the coordinates (1,1) in the distance 
# formula.
# changed to # sqrt((0.68*(rocA1$specificities-1))^2 + (15.62*(rocA1$sensitivities-1)^2))
dist01 = sqrt( (15.62*(rocA1$sensitivities-1)^2) - (0.68*(rocA1$specificities-1))^2) 
optIdxA1 = which.min(dist01)  # index corresponding to minimum distance
threshA1 = rocA1$thresholds[optIdxA1]  # threshold corresponding to min. distance
points(rocA1$specificities[optIdxA1],rocA1$sensitivities[optIdxA1],col="red",pch=7)

# confusion matrix
train_preds <- replace(trnProbsA1, trnProbsA1 > threshA1, 1)
train_preds <- replace(train_preds, train_preds < threshA1, 0)
confMattrix_a1_train = table(charity_train$DONR, train_preds, dnn=c("Target","Predicted"))
confMattrix_a1_train [1,] <- confMattrix_a1_train [1,] / sum(confMattrix_a1_train [1,])
confMattrix_a1_train [2,] <- confMattrix_a1_train [2,] / sum(confMattrix_a1_train [2,])
confMattrix_a1_train


## Part B - Multiple logistic regression with subset selection

library(caret)
cv.ctrl <- trainControl(method = "repeatedcv",
                        number = 5, 
                        repeats = 5, 
                        ## repeated ten times
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel = TRUE,
                        savePredictions = "final")


set.seed(35)
modelB1 <- train(DONR ~ . -RAMNTALL - MEDAGE - GENDER - MEDPPH - MEDINC - MEDEDUC - NGIFTALL - MAXRAMNT - LASTGIFT - HOME - AGE -RFA_96_R,
                          data = charity_train,
                          method = "glm",
                          metric = "ROC",
                          trControl = cv.ctrl)
varImp(modelB1)
summary(modelB1)

trnProbsB1 = predict(modelB1,type="prob")$X2
hist(trnProbsB1,col="gray")

# Classification: ROC Curve for Model B1 - Use methods from pROC package.
rocB1 = roc(response=charity_train$DONR, predictor=trnProbsB1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocB1,col="blue",
     main=paste("ROC curve for Model B1\nAUC = ",round(rocB1$auc,digits=3),sep=""))
par(pty="m")

mult_train_auc <- rocB1$auc

# Classification: Determine "optimal" threshold.
dist01 =  sqrt((15.62*(rocB1$sensitivities-1)^2) - (0.68*(rocB1$specificities-1))^2)
optIdxB1 = which.min(dist01)  # index corresponding to minimum distance
threshB1 = rocB1$thresholds[optIdxB1]  # threshold corresponding to min. distance
points(rocB1$specificities[optIdxB1],rocB1$sensitivities[optIdxB1],col="red",pch=7)

# confusion matrix
train_preds_B1 <- replace(trnProbsB1, trnProbsB1 > threshB1, 1)
train_preds_B1 <- replace(train_preds_B1, train_preds_B1 < threshB1, 0)
confMattrix_b1_train = table(charity_train$DONR, train_preds_B1, dnn=c("Target","Predicted"))
confMattrix_b1_train [1,] <- confMattrix_b1_train [1,] / sum(confMattrix_b1_train [1,])
confMattrix_b1_train [2,] <- confMattrix_b1_train [2,] / sum(confMattrix_b1_train [2,])
confMattrix_b1_train


## Part C - Tree-Based Models
fullTree = rpart(DONR ~  .,
                 data=classData,
                 method="class",
                 parms=list(split="gini", loss=matrix(c(0,15.62,0.68,0),nrow=2,ncol=2)))
#summary(fullTree)
plot(fullTree)
text(fullTree)

# Prune the tree
printcp(fullTree)
cpBest = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]),"CP"]
modelC1 = prune(fullTree,cp=cpBest) # In this case, the optimal tree is the unpruned tree
#summary(modelC1)
plot(modelC1)
text(modelC1,pretty=0)

# Ranking: Generate lift chart on training subset and measure top-decile lift.
trnProbsC1 = predict(modelC1,newdata=charity_train, type="prob")[,2]

hist(trnProbsC1,col="gray")   # Note that scores are distributed around 0.05.

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocC1 = roc(response=charity_train$DONR, predictor=trnProbsC1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocC1,col="blue",
     main=paste("ROC curve for Model C1\nAUC = ",round(rocC1$auc,digits=3),sep=""))
par(pty="m")

tree_train_auc <- rocC1$auc

# Classification: Determine "optimal" threshold.
dist01 = sqrt( (15.62*(rocC1$sensitivities-1)^2) - (0.68*(rocC1$specificities-1))^2) 
optIdxA1 = which.min(dist01)  # index corresponding to minimum distance
threshC1 = rocC1$thresholds[optIdxA1]  # threshold corresponding to min. distance
points(rocC1$specificities[optIdxA1],rocC1$sensitivities[optIdxA1],col="red",pch=7)


# confusion matrix
train_preds_C1 <- replace(trnProbsC1, trnProbsC1 > threshC1, 1)
train_preds_C1 <- replace(train_preds_C1, train_preds_C1 < threshC1, 0)
confMattrix_c1_train = table(charity_train$DONR, train_preds_C1, dnn=c("Target","Predicted"))
confMattrix_c1_train [1,] <- confMattrix_c1_train [1,] / sum(confMattrix_c1_train [1,])
confMattrix_c1_train [2,] <- confMattrix_c1_train [2,] / sum(confMattrix_c1_train [2,])
confMattrix_c1_train

## Part D - Model of my choice. #### Random forest classified

library(randomForest)
set.seed(711)
modelD1 <- randomForest(DONR ~ .,
                        data = charity_train, 
                        importance = TRUE, 
                        ntree = 500,
                        mtry = 3)

varImpPlot(modelD1)

trnProbsD1 = predict(modelD1, type="prob")[,2]
hist(trnProbsD1,col="gray")

# Classification: ROC Curve for Model B1 - Use methods from pROC package.
rocD1 = roc(response=charity_train$DONR, predictor=trnProbsD1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocD1,col="blue",
     main=paste("ROC curve for Model D1\nAUC = ",round(rocD1$auc,digits=3),sep=""))
par(pty="m")

rf_train_auc <- rocD1$auc

# Classification: Determine "optimal" threshold.
dist01 =  sqrt((15.62*(rocD1$sensitivities-1)^2) - (0.68*(rocD1$specificities-1))^2)
optIdxD1 = which.min(dist01)  # index corresponding to minimum distance
threshD1 = rocD1$thresholds[optIdxD1]  # threshold corresponding to min. distance
points(rocD1$specificities[optIdxD1],rocD1$sensitivities[optIdxD1],col="red",pch=7)

# confusion matrix
train_preds_D1 <- replace(trnProbsD1, trnProbsD1 > threshD1, 1)
train_preds_D1 <- replace(train_preds_D1, train_preds_D1 < threshD1, 0)
confMattrix_d1_train = table(charity_train$DONR, train_preds_D1, dnn=c("Target","Predicted"))
confMattrix_d1_train [1,] <- confMattrix_d1_train [1,] / sum(confMattrix_d1_train [1,])
confMattrix_d1_train [2,] <- confMattrix_d1_train [2,] / sum(confMattrix_d1_train [2,])
confMattrix_d1_train



########################################
## Exercise 5
## Model Validation
########################################

# B
# For the Training Set MSE, predict DONR for all of the individuals in the Training Set, 
# and calculate the MSE from the Training Set predictions

# Simple Logistic Classification
trnProbsA1 = predict(modelA1,type="response")
# confusion matrix
train_preds <- replace(trnProbsA1, trnProbsA1 > threshA1, 1)
train_preds <- replace(train_preds, train_preds < threshA1, 0)

# Multi Logistic Classification
trnProbsB1 = predict(modelB1,type="prob")$X2
# confusion matrix
train_preds_B1 <- replace(trnProbsB1, trnProbsB1 > threshB1, 1)
train_preds_B1 <- replace(train_preds_B1, train_preds_B1 < threshB1, 0)

# Tree model
trnProbsC1 = predict(modelC1,newdata=charity_train, type="prob")[,2]
# confusion matrix
train_preds_C1 <- replace(trnProbsC1, trnProbsC1 > threshC1, 1)
train_preds_C1 <- replace(train_preds_C1, train_preds_C1 < threshC1, 0)

# Random Forest
trnProbsD1 = predict(modelD1, type="prob")[,2]
# confusion matrix
train_preds_D1 <- replace(trnProbsD1, trnProbsD1 > threshD1, 1)
train_preds_D1 <- replace(train_preds_D1, train_preds_D1 < threshD1, 0)

train_y <- ifelse(charity_train$DONR == "X2", 1, 0)


# Calculate MSE for training set
simple_logistic_train <- mean((train_preds - train_y)^2)
simple_logistic_train

multi_logistic_train <- mean((train_preds_B1 - train_y)^2)
multi_logistic_train

tree_train <- mean((train_preds_C1 - train_y)^2)
tree_train

RF_train <- mean((train_preds_D1 - train_y)^2)
RF_train


# C
# For the Training Set MSE, predict DONR for all of the individuals in the Training Set, 
# and calculate the MSE from the Training Set predictions
test_y <- ifelse(charity_test$DONR == "X2", 1, 0)

# Simple Logistic Classification
trnProbsA1_test <- predict(modelA1, newdata = charity_test, type="response")
test_preds <- replace(trnProbsA1_test, trnProbsA1_test > threshA1, 1)
test_preds <- replace(test_preds, test_preds < threshA1, 0)

# Multi Logistic Classification
trnProbsB1_test <- predict(modelB1, newdata = charity_test,type="prob")$X2
test_B1_preds <- replace(trnProbsB1_test, trnProbsB1_test > threshA1, 1)
test_B1_preds <- replace(test_B1_preds, test_B1_preds < threshA1, 0)

# Tree model
trnProbsC1_test = predict(modelC1,newdata=charity_test, type="prob")[,2]
test_preds_C1 <- replace(trnProbsC1_test, trnProbsC1_test > threshC1, 1)
test_preds_C1 <- replace(test_preds_C1, test_preds_C1 < threshC1, 0)

# Random Forest
trnProbsD1_test = predict(modelD1,newdata=charity_test, type="prob")[,2]
test_preds_D1 <- replace(trnProbsD1_test, trnProbsD1_test > threshC1, 1)
test_preds_D1 <- replace(test_preds_D1, test_preds_D1 < threshC1, 0)


# Calculate MSE for test set
simple_logistic_test <- mean((test_preds - test_y)^2)
simple_logistic_test

multi_logistic_test <- mean((test_B1_preds - test_y)^2)
multi_logistic_test

tree_test <- mean((test_preds_C1 - test_y)^2)
tree_test

RF_test <- mean((test_preds_D1 - test_y)^2)
RF_test

# D
# present the Test Set lift chart for each model that you build.


# Simple confusion matrix
confMattrix_a1 = table(test_y, test_preds, dnn=c("Target","Predicted"))
confMattrix_a1 [1,] <- confMattrix_a1 [1,] / sum(confMattrix_a1 [1,])
confMattrix_a1 [2,] <- confMattrix_a1 [2,] / sum(confMattrix_a1 [2,])
confMattrix_a1
rocA1_test = roc(response=test_y, predictor=trnProbsA1_test)
simple_test_auc <- rocA1_test$auc

# Multi confusion matrix
confMattrix_b1 = table(test_y, test_B1_preds, dnn=c("Target","Predicted"))
confMattrix_b1 [1,] <- confMattrix_b1 [1,] / sum(confMattrix_b1 [1,])
confMattrix_b1 [2,] <- confMattrix_b1 [2,] / sum(confMattrix_b1 [2,])
confMattrix_b1
rocB1_test = roc(response=test_y, predictor=trnProbsB1_test)
mult_test_auc <- rocB1_test$auc

# tree confusion matrix
confMattrix_c1 = table(test_y, test_preds_C1, dnn=c("Target","Predicted"))
confMattrix_c1 [1,] <- confMattrix_c1 [1,] / sum(confMattrix_c1 [1,])
confMattrix_c1 [2,] <- confMattrix_c1 [2,] / sum(confMattrix_c1 [2,])
confMattrix_c1
rocC1_test = roc(response=test_y, predictor=trnProbsC1_test)
tree_test_auc <- rocC1_test$auc

# Random Forest confusion matrix
confMattrix_d1 = table(test_y, test_preds_D1, dnn=c("Target","Predicted"))
confMattrix_d1 [1,] <- confMattrix_d1 [1,] / sum(confMattrix_d1 [1,])
confMattrix_d1 [2,] <- confMattrix_d1 [2,] / sum(confMattrix_d1 [2,])
confMattrix_d1
rocD1_test = roc(response=test_y, predictor=trnProbsD1_test)
rf_test_auc <- rocD1_test$auc

# D produce lift tables for each model

# simple logistic
plotLift(trnProbsA1_test, test_y)
TopDecileLift(trnProbsA1_test,test_y)

# multiple logistic
plotLift(trnProbsB1_test, test_y)
TopDecileLift(trnProbsB1_test,test_y)

# tree
plotLift(trnProbsC1_test, test_y)
TopDecileLift(trnProbsC1_test,test_y)

# random forest
plotLift(trnProbsD1_test, test_y)
TopDecileLift(trnProbsD1_test,test_y)

# A
# Build a table
# three columns - Model Name, Training Set MSE, Test Set MSE
Model_names <- c('Simple Logistic', 'Multiple Logistic', 'Tree', 'Random Forest')

results_table <- as.data.frame(Model_names)
results_table$training_MSE <- c(simple_logistic_train, multi_logistic_train, tree_train, RF_train)
results_table$Test_MSE <- c(simple_logistic_test, multi_logistic_test, tree_test, RF_test)
results_table$train_auc <- c(simple_train_auc, mult_train_auc, tree_train_auc, rf_train_auc)
results_table$test_auc <- c(simple_test_auc, mult_test_auc, tree_test_auc, rf_test_auc)
results_table$Donor_acc_train <- c(confMattrix_a1_train [2,2], confMattrix_b1_train [2,2], confMattrix_c1_train [2,2], confMattrix_d1_train [2,2])
results_table$Donor_acc_test <- c(confMattrix_a1 [2,2], confMattrix_b1 [2,2], confMattrix_c1 [2,2], confMattrix_d1 [2,2])
results_table$Non_Donor_acc_train <- c(confMattrix_a1_train [1,1], confMattrix_b1_train [1,1], confMattrix_c1_train [1,1], confMattrix_d1_train [1,1])
results_table$Non_Donor_acc_test <- c(confMattrix_a1 [1,1], confMattrix_b1 [1,1], confMattrix_c1 [1,1], confMattrix_d1 [1,1])
results_table


