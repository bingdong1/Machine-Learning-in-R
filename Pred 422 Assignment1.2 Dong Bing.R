library(corrgram)
library(corrplot)
library(ISLR)



#8. (a)
college <- read.csv(file="U:/Midterm/College.csv", header=TRUE, sep=",", na.strings="?")

#8. (b)
rownames(college )=college [,1] 
fix(college)

college =college [,-1] 
fix(college)

# 8. (c)
# i.

summary <- summary(college[,-1])
attach(college)

#write.csv(summary, file = "summary.csv", row.names=FALSE)

# ii.
pairs(college[,1:10])
# iii.
plot(Private, Outstate)
# iv.
Elite = rep("No", nrow(college))
Elite[Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(Elite)
plot(Elite, Outstate)
# v.
par(mfrow=c(2,2))
hist(Accept)
hist(Enroll, col=2)
hist(Grad.Rate, col=3, breaks=10)
hist(Expend, breaks=100)
# vi.
par(mfrow=c(1,1))
plot(Private, college$Grad.Rate)
# Mean for graduation rate is slightly highter for private college, although the hinge for eac box is close

plot(Outstate, Grad.Rate)
# Out of state student pays Higher tuition, it looks its correlates to higher graduation rate as well.

plot(Private, Room.Board)
# As expected, the cost of room and board for private school is higher than public

plot(Accept/Apps, Elite)
# Colleges with low acceptance rate tend to be elite colleges

plot(PhD , Grad.Rate)
# At first glanace it looks like school with lower Phd ratio has lower graduation rate but this trend stops at 
#certain threshold, it doesn't make much difference for graduation rate once the phd ratio exceed 50%.

corrgram(college[,2:18])


# correlations

correlations <- cor(college[,2:18])
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")


# 9.
Auto <- read.csv(file="U:/Midterm/Auto.data.csv", header=TRUE, sep=",", na.strings="?")
Auto = na.omit(Auto)
dim(Auto)
fix(Auto)

#(a)
summary(Auto)
str(Auto)

# quantitative: mpg, cylinders, displacement, horsepower, weight,
# acceleration, year
# qualitative: name, origin

#(b)
# apply the range function to the first seven columns of Auto
sapply(Auto[, 1:7], range)

#(c)
sapply(Auto[, 1:7], mean)

sapply(Auto[, 1:7], sd)

#(d)

newAuto = Auto[-(10:85),]

sapply(newAuto[, 1:7], range)

sapply(newAuto[, 1:7], mean)

sapply(newAuto[, 1:7], sd)

#(e)

correlations <- cor(Auto[,1:8])
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

pairs(Auto)
plot(Auto$mpg, Auto$horsepower)
# Higher horse power correlates with lower mpg.
plot(Auto$mpg, Auto$year)
# Cars become more efficient as time progress
plot(Auto$mpg, Auto$cylinders)
# Hihger cylinders yields lower mpg. 

#(f)

library("Hmisc")
res2 <- rcorr(as.matrix(Auto[,1:8]))
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

fix(Auto)

# boruta variable selection
#install.packages("Boruta")
library(Boruta)

drop_id <- c("row.names","mpg")
boruta_features = Auto[,!(names(Auto) %in% drop_id)]

bor.results <- Boruta(boruta_features, Auto$mpg,
                      maxRuns=200)
plot(bor.results)
names(bor.results)
bor.results$finalDecision
CONFIRMED_ATTR <- getSelectedAttributes(bor.results)
CONFIRMED_ATTR_fix <- getSelectedAttributes(TentativeRoughFix(bor.results))
Boruta_output <- attStats(bor.results)


#machine learning
auto_train <- Auto[1:300,1:8]
auto_test <- Auto[301:392,1:8]

fix(Auto)
library(caret)

cols.dont.want <- c("mpg")
auto_test <- auto_test[, ! names(auto_test) %in% cols.dont.want, drop = F]

#model_drop <- c("mpg")
model <- auto_train

#Giadient Boosting
fitControl <- trainControl(
  method = "repeatedcv",
  number = 2, 
  ## repeated 5 times
  repeats = 1, 
  allowParallel = TRUE)

fit_gbm <- function(data2, test) {
  
  #data2 <- train_data
  gbmGrid <-  expand.grid(interaction.depth = 25, #c(5:30), # 12 old; 25 was selected as optimal
                          n.trees = 1000,# c(5000, 10000), #20000, 30000, 40000, 50000), 
                          shrinkage = 0.01, #c(0.01, 0.005),
                          n.minobsinnode = 7) #c(1, 3, 5, 7, 10))
  
  gbmFit <- train(mpg ~ ., data = data2,
                  method = "gbm",
                  trControl = fitControl,
                  preProc = c("center", "scale"),
                  ## Now specify the exact models 
                  ## to evaluate:
                  tuneGrid = gbmGrid,
                  maximize=FALSE)
  
  return(gbmFit)
}

gbmFit <- fit_gbm(model, auto_test) 
mean(gbmFit$resample$RMSE) 
summary(gbmFit)

gbm_pred <- predict(gbmFit, auto_test)

fix(Auto)

#linear regression
fit_lm <- function(data2, test) {
  
  set.seed(711)
  lmFit <- train(mpg ~ ., data = data2,
                 method = "lmStepAIC")
  
  return(lmFit)
}

lm_fit <- fit_lm(model, auto_test)
summary(lm_fit)   
mean(lm_fit$resample$RMSE) 
lm_pred <- predict(lm_fit, auto_test) 

#xboost model
fit_xgb <- function(data2, test) {
  #data2 <- train_data
  xgb_GRID <-  expand.grid(nrounds= 1500,  # 1300 -> 1500, 100
                           max_depth= 3,# c(1:5), 
                           eta= 0.05, #c(1:10)/100 , # learning rate
                           gamma= 0, #c(1:10)/100 ,
                           colsample_bytree= 0.205, #(10:30)/100, # 0.21
                           min_child_weight = 9, #c(1:10),
                           subsample = 1)
  
  
  xgboost <- train(mpg ~ ., data = data2,
                   method = "xgbTree",
                   trControl = fitControl,
                   verbose = FALSE,
                   preProc = c("center", "scale"),
                   ## Now specify the exact models 
                   ## to evaluate:
                   tuneGrid = xgb_GRID,
                   maximize=FALSE)
  
  #trellis.par.set(caretTheme()) # RMSE: 0.1228894  Rsquared: 0.7880107
  #plot(xgboost)
  
  #xgb_pred <- predict(xgboost, test)
  #xgb_pred2 <- (exp(xgb_pred) * test_model$GrLivArea) + test_model$MiscVal
  
  return(xgboost)
}

xgb_fit <- fit_xgb(model,auto_test ) 
mean(xgb_fit$resample$RMSE) 
xgb_pred <- predict(xgb_fit, auto_test)

#Random Forest
fit_rf <- function(data2, test) {
  
  #data2 <- train_data
  rf.grid <- data.frame(.mtry = 53)#c(1:55))#,
  #.maxdepth = c(10:15)) # mtry = 55
  
  rf.tune.1 <- train(mpg ~ ., data = data2,
                     method = "rf", # rfRules
                     metric = "RMSE",
                     trControl = fitControl,
                     tuneGrid = rf.grid,
                     maximize=FALSE)
  
  #trellis.par.set(caretTheme())
  #plot(fobaFit2)
  
  #rf_pred <- predict(rf.tune.1, test)
  #rf_pred2 <- (exp(rf_pred) * test_model$GrLivArea) + test_model$MiscVal
  
  return(rf.tune.1)
}

rf_fit <- fit_rf(model, auto_test)
mean(rf_fit$resample$RMSE) 
rf_pred <- predict(rf_fit, auto_test)

#rpart
fit_rpart <- function(data2, test) {
  #data2 <- train_data
  rpart.grid <- data.frame(maxdepth = 14)#c(10:15)) 
  
  rpartFit <- train(mpg ~ ., data = data2,
                    method = "rpart2",
                    metric = "RMSE",
                    trControl = fitControl,
                    tuneGrid = rpart.grid,
                    maximize=FALSE)
  
  #rpart_pred <- predict(rpartFit, test)
  #rpart_pred2 <- exp(rpart_pred) * test_model$GrLivArea + test_model$MiscVal
  
  return(rpartFit)
}

rpart_fit <- fit_rpart(model, auto_test) 
mean(rpart_fit$resample$RMSE) 
rpart_pred <- predict(rpart_fit, auto_test)


#Compare different models
mean(gbmFit$resample$RMSE) 
mean(lm_fit$resample$RMSE) 
mean(xgb_fit$resample$RMSE) 
mean(rf_fit$resample$RMSE) 
mean(rpart_fit$resample$RMSE) 

#ensemble model

model_preds_actual = cbind(gbm_pred, rpart_pred, xgb_pred, rf_pred, lm_pred)  
cor(model_preds_actual, model_preds_actual)

GBM_train_preds <- fitted.values(gbmFit)
XGB_train_preds <- fitted.values(xgb_fit)
RF_train_preds <- fitted.values(rf_fit)
LM_train_preds <- fitted.values(lm_fit)
rpart_train_preds <- fitted.values(rpart_fit)

insample_preds = data.frame(GBM_train_preds, rpart_train_preds, 
                            XGB_train_preds, RF_train_preds, LM_train_preds) 

model_preds_df <- as.data.frame(insample_preds)
model_preds_act <- as.data.frame(model_preds_actual)
mpg <- model$mpg
model_preds_df$mpg <- mpg

#### XGB Stacking model ####
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 2, #5
  ## repeated ten times
  repeats = 1, # 5
  allowParallel = TRUE)
#search = "random")

xgb_GRID <-  expand.grid(nrounds= 1500,  # 1300 -> 1500, 100
                         max_depth = 1, #c(1:6),
                         eta= 0.05, #c(1:10)/100 , # learning rate
                         gamma= 0, #c(1:10)/100 ,
                         colsample_bytree= 0.205, #(5:30)/100, # 0.21
                         min_child_weight = 9, #c(1:10),
                        subsample = 1)

set.seed(1)
xgboost_ensemble <- train(mpg ~ ., data = model_preds_df,
                          method = "xgbTree",
                          trControl = fitControl,
                          verbose = FALSE,
                          preProc = c("center", "scale"),
                          ## Now specify the exact models 
                          ## to evaluate:
                          tuneGrid = xgb_GRID,
                          maximize=FALSE)

mean(xgboost_ensemble$resample$RMSE)

xensemble_pred <- predict(xgboost_ensemble,model_preds_act)
fitted.values(xgboost_ensemble)
xensemble_results <- postResample(pred = xensemble_pred, obs = mpg)
#xensemble_results


