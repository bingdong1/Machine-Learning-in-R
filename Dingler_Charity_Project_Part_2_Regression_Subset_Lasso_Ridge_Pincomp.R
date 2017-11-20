########################################
## Andy Dingler
## PREDICT 422
## Charity Project - Part 2 (Regression Problem)
########################################

########################################
## Exercise 1 - Imprt Data
########################################

# read the data into R
charityData = read.csv("trainSample.csv", na.strings=c("NA"," "))

# subset the data to only observations where DONR = 1
charityData_donrs <- subset(charityData, charityData$DONR == 1)

source("Dingler_DataPreparation.R")
charityData_donrs_imputed2 <- processPart(charityData_donrs)


########################################
## Exercise 2 - Data Preparation
########################################

# re-categorizing variables
# Convert categorical variables to factors
charityData_donrs$DONR = as.factor(charityData_donrs$DONR)
charityData_donrs$HOME = as.factor(charityData_donrs$HOME)
charityData_donrs$HINC = as.factor(charityData_donrs$HINC)

# removed the observations that have neighborhood values of zero
charityData_donrs_filtered <- charityData_donrs[charityData_donrs$MEDAGE > 0, ]

# addressing missing values
table(charityData_donrs_filtered$HOME, useNA = "ifany")
table(charityData_donrs_filtered$HINC, useNA = "ifany")
table(charityData_donrs_filtered$GENDER, useNA = "ifany")

# The unknown gender categorical option 
# replace the A and C genders to unkown
charityData_donrs_filtered$GENDER <- replace(charityData_donrs_filtered$GENDER, charityData_donrs_filtered$GENDER == 'A', NA)
charityData_donrs_filtered$GENDER <- replace(charityData_donrs_filtered$GENDER, charityData_donrs_filtered$GENDER == 'C', NA)
charityData_donrs_filtered$GENDER <- replace(charityData_donrs_filtered$GENDER, charityData_donrs_filtered$GENDER == 'U', NA)
charityData_donrs_filtered$GENDER <- factor(charityData_donrs_filtered$GENDER, levels = c("F", "J", "M"))
# levels(charityData_donrs_filtered$GENDER)

# remove variables from data set?
plot(charityData_donrs_filtered$GENDER, charityData_donrs_filtered$DAMT, xlab="Gender", ylab="Donation ($)")
gender_outlierTest <- charityData_donrs_filtered[charityData_donrs_filtered$DAMT < 120, ]
plot(gender_outlierTest$GENDER, gender_outlierTest$DAMT, xlab="Gender", ylab="Donation ($)")

library(mice)
charityData_donrs_mice <- mice(charityData_donrs_filtered, m = 1, method = 'cart', printFlag = FALSE, maxit = 5)
charityData_donrs_imputed <- complete(charityData_donrs_mice) 

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
charityData_donrs_imputed = separateRFA(charityData_donrs_imputed,"RFA_96")
# remove old variable RFA_96
old_vars <- c("RFA_96", "DONR", "ID")
charityData_donrs_imputed2 <- charityData_donrs_imputed2[ , !(names(charityData_donrs_imputed2) %in% old_vars)]

plot(charityData_donrs_imputed2$RFA_96_A, charityData_donrs_imputed2$DAMT, xlab="RFA Values", ylab="Donation ($)")
# B and C can be merged into factor value of D
charityData_donrs_imputed2$RFA_96_A <- replace(charityData_donrs_imputed2$RFA_96_A, charityData_donrs_imputed2$RFA_96_A == 'B', 'D')
charityData_donrs_imputed2$RFA_96_A <- replace(charityData_donrs_imputed2$RFA_96_A, charityData_donrs_imputed2$RFA_96_A == 'C', 'D')
charityData_donrs_imputed2$RFA_96_A <- factor(charityData_donrs_imputed2$RFA_96_A)
plot(charityData_donrs_imputed2$RFA_96_A, charityData_donrs_imputed2$DAMT, xlab="Gender", ylab="Donation ($)")

########################################
## Exercise 3 - Dataset Partitioning
########################################

str(charityData_donrs_imputed2)

# Hold-out Test Set
set.seed(8)
train <- sample(nrow(charityData_donrs_imputed2), nrow(charityData_donrs_imputed2) * 0.75)
charity_train <- charityData_donrs_imputed2[train,]
charity_test <- charityData_donrs_imputed2[-train,]

# number of observations in Test
nrow(charity_test)

# number of observations in Train
nrow(charity_train)

par(mfrow = c(2,1))
# Test Distribution
hist(charity_test$DAMT, breaks = 20, main = "Test DAMT Distribution")

# Train Distribution
hist(charity_train$DAMT, breaks = 20, main = "Train DAMT Distribution")
par(mfrow = c(1,1))


########################################
## Exercise 4 - Model Fitting
########################################

# Simple linear regression -- single predictor variable
charity_lm <- lm(DAMT ~ LASTGIFT, data = charity_train)
par(mfrow=c(2,2))
plot(charity_lm)
par(mfrow=c(1,1))

# Multiple linear regression with subset selection -- ISLR section 6.1
library(leaps)
length(charity_train)
regfit.fwd <- regsubsets(DAMT~., data = charityData_donrs_imputed2,
                         nvmax = 19, method = "exhaustive")

val.errors <- rep(NA, 19)
# make test model matrix, with Salary being the response
x.test <- model.matrix(DAMT~., data = charity_test)
# now make predictions for each model
for(i in 1:19) {
        coefi = coef(regfit.fwd, id = i)
        pred = x.test[ , names(coefi)] %*% coefi
        val.errors[i] = mean( (charity_test$DAMT-pred)^2 )
} 

plot(val.errors, ylab="MSE",
     ylim=c(0, 100), pch=19, type="b")
points(sqrt(regfit.fwd$rss[-1]/180), col="blue", pch=19, type="b")
legend("topright", legend=c("Training", "Validation"),
       col=c("blue", "black"), pch=19)

# I want the 3 variable model
coefi <- coef(regfit.fwd, id = 3)

# Shrinkage models -- ISLR section 6.2
library(glmnet)
charity_train_y <- charity_train$DAMT
charity_train_x <- model.matrix(DAMT ~., data = charity_train)[, -1]
charity_test_y <- charity_test$DAMT 
charity_test_x <- model.matrix(DAMT ~., data = charity_test)[, -1]

# Lasso model
lasso.cv <- cv.glmnet(charity_train_x, charity_train_y, alpha = 1)
plot(lasso.cv)
best_lambda_lasso <- lasso.cv$lambda.min

# Fit a ridge regression model; lambda chosen by CV
ridge.cv <- cv.glmnet(charity_train_x, charity_train_y, alpha = 0)
plot(ridge.cv)
best_lambda_ridge <- ridge.cv$lambda.min

# Principal Compnents Regression -- ISLR section 6.3
# Fit a PCR model with M chosen by CV
library(pls)
charity_pcr <- pcr(DAMT ~ ., data = charity_train, scale = TRUE, validation = "CV")
validationplot(charity_pcr, val.type = "MSEP")

#### F ####
# Fit a PLS model with M chosen by CV
charity_plsr <- plsr(DAMT ~ ., data = charity_train, scale = TRUE, validation = "CV")
validationplot(charity_plsr, val.type = "MSEP")


########################################
## Exercise 5 - Model Validation
########################################

# B
# Predict DAMT for all of the individuals in the training set

pred_charirty_train <- predict(charity_lm, newdata = charity_train)
train_matrix <- model.matrix(DAMT~., data = charity_train)
reg_pred_train <- train_matrix[ , names(coefi)] %*% coefi
pred_lasso_train <- predict(lasso.cv, s = best_lambda_lasso, newx = charity_train_x)
ridge_pred_train <- predict(ridge.cv, s = best_lambda_ridge, newx = charity_train_x)
pcr_pred_train <- predict(charity_pcr, newdata = charity_train, ncomp = 29)
plsr_pred_train <- predict(charity_plsr, newdata = charity_train, ncomp = 6)

# Calculate MSE for training set
lm_residuals_train <- mean((pred_charirty_train - charity_train_y)^2)
lm_residuals_train

reg_residuals_train <- mean((reg_pred_train - charity_train_y)^2)
reg_residuals_train

lasso_residuals_train <- mean((pred_lasso_train - charity_train_y)^2)
lasso_residuals_train

ridge_residuals_train <- mean((ridge_pred_train - charity_train_y)^2)
ridge_residuals_train

pcr_residuals_train <- mean((pcr_pred_train - charity_train_y)^2)
pcr_residuals_train

pls_residuals_train <- mean((plsr_pred_train - charity_train_y)^2)
pls_residuals_train

# Predict DAMT for all of the individuals in the test set
pred_charirty <- predict(charity_lm, newdata = charity_test)
test_matrix <- model.matrix(DAMT~., data = charity_test)
reg_pred <- test_matrix[ , names(coefi)] %*% coefi
pred_lasso <- predict(lasso.cv, s = best_lambda_lasso, newx = charity_test_x)
ridge_pred <- predict(ridge.cv, s = best_lambda_ridge, newx = charity_test_x)
pcr_pred <- predict(charity_pcr, newdata = charity_test, ncomp = 29)
plsr_pred <- predict(charity_plsr, newdata = charity_test, ncomp = 6)

# C
# Calculate MSE for test set

lm_residuals <- mean((pred_charirty - charity_test$DAMT)^2)
lm_residuals

reg_residuals <- mean((reg_pred - charity_test$DAMT)^2)
reg_residuals

lasso_residuals <- mean((pred_lasso - charity_test$DAMT)^2)
lasso_residuals

ridge_residuals <- mean((ridge_pred - charity_test$DAMT)^2)
ridge_residuals

pcr_residuals <- mean((pcr_pred - charity_test$DAMT)^2)
pcr_residuals

pls_residuals <- mean((plsr_pred - charity_test$DAMT)^2)
pls_residuals

# A
# Build a table
# three columns - Model Name, Training Set MSE, Test Set MSE
Model_names <- c('Simple Linear', 'Multiple BestSubset', 'Lasso', 'Ridge', 'PCR', 'PLSR')
training_MSE <- c(lm_residuals_train, reg_residuals_train, lasso_residuals_train, ridge_residuals_train, pcr_residuals_train, pls_residuals_train)
Test_MSE <- c(lm_residuals, reg_residuals, lasso_residuals, ridge_residuals, pcr_residuals, pls_residuals)
lm_coef <- length(coef(charity_lm))
regsubset_coef <- length(coefi)
lasso_coef <- length(coef(lasso.cv))
ridge_coef <- length(coef(ridge.cv))
pcr_coef <- length(coef(charity_pcr))
plsr_coef <- length(coef(charity_plsr))
Num_coef <- c(lm_coef, regsubset_coef, lasso_coef, ridge_coef, pcr_coef, plsr_coef)

# Comment on the predictive accuracy from the models
# Is there much difference between the test errrors
r_square_denom <- mean((mean(charity_test$DAMT) - charity_test$DAMT)^2)
lm_r2 <- 1 - lm_residuals / r_square_denom
reg_r2 <- 1 - reg_residuals / r_square_denom
lasso_r2 <- 1 - lasso_residuals / r_square_denom
ridge_r2 <- 1 - ridge_residuals / r_square_denom
pcr_r2 <- 1 - pcr_residuals / r_square_denom
pls_r2 <- 1 - pls_residuals / r_square_denom
rsquare <- c(lm_r2, reg_r2, lasso_r2, ridge_r2, pcr_r2, pls_r2)

results_table <- as.data.frame(Model_names)
results_table$training_MSE <- training_MSE
results_table$Test_MSE <- Test_MSE
results_table$rsquare <- rsquare
results_table$Num_coef <- Num_coef
results_table

########################################
## Exercise 6 - Model Selection
########################################

# Explain which model is the best performing model
# If two models have similar Test Set MSE values, then the model with fewer predictors should be selected.
# Best subset selection performed the best

