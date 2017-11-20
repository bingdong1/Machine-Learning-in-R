#rm(list = ls())

library(mice)
library(caret)
library(leaps)
library(pls)
library(randomForest)

#1 Read in Data

charityData <- read.csv(file="U:/Midterm/trainSample.csv", header=TRUE, sep=",", na.strings=NA)

regrData<- charityData[charityData$DONR == 1, ]

fix(regrData)


#2 (a) Impute for Missing Values

#Graphical illustration of NA

colSums(sapply(regrData, is.na))

#impute missing value

imp.regrData <- mice(regrData, m = 1, method='cart')

regrData <- complete(imp.regrData) 

colSums(sapply(regrData, is.na))

#2 (b) Transformed variables

#Separate RFA
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
regrData = separateRFA(regrData,"RFA_96")

# Convert categorical variables to factors
regrData$DONR = as.factor(regrData$DONR)
regrData$HOME = as.factor(regrData$HOME)
regrData$HINC = as.factor(regrData$HINC)

#2 (c) re-categorization of categorical variables

# Recategorize Gender
summary(regrData$GENDER)

regrData$GENDER <- ifelse(regrData$GENDER == 'A', 'A',
                                ifelse(regrData$GENDER == "C", 'A',
                                       ifelse(regrData$GENDER == 'J', 'A', 
                                              ifelse(regrData$GENDER == 'U', 'A', 
                                                     ifelse(regrData$GENDER == 'F', 'F',
                                                            ifelse(regrData$GENDER == 'M', 'M', NA))))))
#change to factors
regrData$GENDER = as.factor(regrData$GENDER)

#Check
summary(regrData$GENDER)

# Recategorize Recency
summary(regrData$RFA_96_R)

regrData$RFA_96_R <- ifelse(regrData$RFA_96_R == 'A', 'A',
                           ifelse(regrData$RFA_96_R == "S", 'S',
                                  ifelse(regrData$RFA_96_R == 'F', 'FNL', 
                                         ifelse(regrData$RFA_96_R == 'L', 'FNL', 
                                                ifelse(regrData$RFA_96_R == 'N', 'FNL',NA )))))
#change to factors
regrData$RFA_96_R = as.factor(regrData$RFA_96_R)

#check
summary(regrData$RFA_96_R)

#no need to recategorize frequency

## Recategorize amount

summary(regrData$RFA_96_A)

regrData$RFA_96_A <- ifelse(regrData$RFA_96_A == 'B', 'D',
                            ifelse(regrData$RFA_96_A == "C", 'D',
                                   ifelse(regrData$RFA_96_A == 'D', 'D', 
                                          ifelse(regrData$RFA_96_A == 'E', 'E', 
                                                 ifelse(regrData$RFA_96_A == 'F', 'F',
                                                      ifelse(regrData$RFA_96_A == 'G', 'G',NA ))))))
#change to factors
regrData$RFA_96_A = as.factor(regrData$RFA_96_A)

#check
summary(regrData$RFA_96_A)

#2 (d). Remove Variable

#Drop no longer needed variables
drop <- c('ID', 'RFA_96', 'DONR') 

regrData = regrData[,!(names(regrData) %in% drop)]


#Remove outliers

#library(data.table)
#outlierReplace = function(dataframe, cols, rows, newValue = NA) {
#  if (any(rows)) {
#    set(dataframe, rows, cols, newValue)
#  }
#}

#qplot(data = regrData, x = DAMT) + ylab("Number of Schools")

#outlierReplace(regrData, "DAMT", which(regrData$DAMT >100), 100)

#qplot(data = regrData, x = DAMT) + ylab("Number of Schools")

#3 (A) Hold-Out Test Set

#test set
testindex <- createDataPartition(y=regrData$DAMT, p=0.25, list=FALSE)    

regrData_test <- regrData[ testindex,]

dim(regrData_test)

summary(regrData_test)

hist(regrData_test$DAMT,col="blue",breaks=20,xlab="DAMT",main="")

#train set

regrData_train  <- regrData[-testindex,]

dim(regrData_train)

summary(regrData_train)

hist(regrData_train$DAMT,col="blue",breaks=20,xlab="DAMT",main="")

#4 (A) Simple linear regression
fix(regrData_train)

regfit.full=regsubsets(DAMT~.,regrData_train,nvmax = 20)

mod.summary = summary(regfit.full)

mod.summary

lm.fit =lm(DAMT ~ LASTGIFT ,data=regrData_train )

summary (lm.fit)

plot(lm.fit)


#train MSE

mean((regrData_train$DAMT - predict.lm(lm.fit, regrData_train)) ^ 2)

#78.16131

# another way to calculate train MSE
train.mat=model.matrix(DAMT~.,regrData_train)

coefi=coef(regfit.full,id=1)
pred=train.mat[,names(coefi)]%*%coefi
val.errors=mean((regrData_train$DAMT-pred)^2)

val.errors

#78.16131

#test MSA

mean((regrData_test$DAMT - predict.lm(lm.fit, regrData_test)) ^ 2)

#42.76315

# another way to calculate test MSE
test.mat=model.matrix(DAMT~.,regrData_test)

  coefi=coef(regfit.full,id=1)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors=mean((regrData_test$DAMT-pred)^2)

val.errors

#76.02581

# (B) Multiple linear regression

# Find the model size for best cp, BIC and adjr2which.min(mod.summary$cp)

which.min(mod.summary$bic) #6

which.max(mod.summary$adjr2) #17

which.min(mod.summary$cp) #13

coef(regfit.full,6)

#select 6 variables
#train MSE

#Winning Model
#modelA1 = lm(DAMT ~ RAMNTALL + NGIFTALL + LASTGIFT + RFA_96_A,data=regrData_train)

train.mat=model.matrix(DAMT~.,regrData_train)

coefi=coef(regfit.full,id=6)
pred=train.mat[,names(coefi)]%*%coefi
val.errors=mean((regrData_train$DAMT-pred)^2)

val.errors

#71.8768

#test MSE
test.mat=model.matrix(DAMT~.,regrData_test)

coefi=coef(regfit.full,id=8)
pred=test.mat[,names(coefi)]%*%coefi
val.errors=mean((regrData_test$DAMT-pred)^2)

val.errors

#41.69347

#names(mod.summary)

#cp plot
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(13, mod.summary$cp[13], pch = 4, col = "red", lwd = 7)

#bic plot
plot(mod.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(6, mod.summary$bic[6], pch = 4, col = "red", lwd = 7)

#Adjusted R2 plot
plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, 
     type = "l")
points(17, mod.summary$adjr2[17], pch = 4, col = "red", lwd = 7)

#use forward and backward stepwise

mod.fwd = regsubsets(DAMT~., data = regrData_train, nvmax = 20, 
                     method = "forward")
mod.bwd = regsubsets(DAMT~., data = regrData_train, nvmax = 20, 
                     method = "backward")

fwd.summary = summary(mod.fwd)

bwd.summary = summary(mod.bwd)

which.min(fwd.summary$cp) #13

which.min(bwd.summary$cp) #13

which.min(fwd.summary$bic) #6

which.min(bwd.summary$bic) #7

which.max(fwd.summary$adjr2) #17

which.max(bwd.summary$adjr2) #17

#select 13 variables
#train MSE

train.mat=model.matrix(DAMT~.,regrData_train)

coefi=coef(mod.fwd,id= 6)
pred=train.mat[,names(coefi)]%*%coefi
val.errors=mean((regrData_train$DAMT-pred)^2)

val.errors

#71.13249

#test MSE
test.mat=model.matrix(DAMT~.,regrData_test)

coefi=coef(mod.fwd,id=13)
pred=test.mat[,names(coefi)]%*%coefi
val.errors=mean((regrData_test$DAMT-pred)^2)

val.errors

#41.94462

# Principal Components Regressions

set.seed(2)

pcr.fit=pcr(DAMT~., data=regrData_train,scale=TRUE,validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")
validationplot(pcr.fit, val.type = "R2")

predplot(pcr.fit)

#train error
pcr.pred=predict(pcr.fit,regrData_train,ncomp=5)

mean((regrData_train$DAMT - data.frame(pcr.pred)) ^ 2)

#82.79289

#test error

pcr.pred=predict(pcr.fit,regrData_test,ncomp=5)

mean((regrData_test$DAMT - data.frame(pcr.pred)) ^ 2)

#51.37933

#random forest

mtry <- tuneRF(regrData_train[-1],regrData_train$DAMT, ntreeTry=200,
               
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

#print(mtry)

print(best.m)

rf <- randomForest(DAMT~.,regrData_train,ntree = 500,mtry=best.m, importance =T,
                   do.trace=TRUE)






importance(rf)
varImpPlot(rf)

p <- predict(rf,regrData_train)

mean((regrData_train$DAMT - p) ^ 2)

#18.84306

p <- predict(rf,regrData_test)

mean((regrData_test$DAMT - p) ^ 2)

#48.08568

#GBM model

set.seed(825)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 5, # how many times you want to repeat CV
  allowParallel = TRUE)

gbmGrid <-  expand.grid(interaction.depth = 5, #c(3:5)
                        n.trees =  1500, # You will want to play with this and slowly increase the number of trees
                        #c(500, 700, 900, 1100, 1300),  #seq(from = 1500, to = 4000, by = 100)
                        shrinkage = 0.01, # This is the learning rate; usually smaller is better
                        n.minobsinnode = 10)

  gbmFit <- train(DAMT ~ ., data = regrData_train,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = FALSE,
                  tuneGrid = gbmGrid)
  
  
  pred_DAMT= predict(gbmFit, regrData_train, n.trees = 1500)
  
  mean((regrData_train$DAMT - data.frame(pred_DAMT)) ^ 2)
  
  #31.56214
  
  pred_DAMT= predict(gbmFit, regrData_test, n.trees = 1500)
  
  mean((regrData_test$DAMT - data.frame(pred_DAMT)) ^ 2)
  
  #70.07593

#GBM model 2
  DAMT_GBM = gbm(DAMT~., data = regrData_train, distribution = "gaussian", n.trees = 900,
                 shrinkage = 0.01, interaction.depth = 3)
  
  pred_DAMT= predict(DAMT_GBM, regrData_train, n.trees = 900)
  
  mean((regrData_train$DAMT - data.frame(pred_DAMT)) ^ 2)
  
  #41.39343
  
  pred_DAMT= predict(DAMT_GBM, regrData_test, n.trees = 900)
  
  mean((regrData_test$DAMT - data.frame(pred_DAMT)) ^ 2)
  
  #69.01307
  
  
  
  

