library(mice)
library(caret)
library(leaps)
library(pls)
library(pscl)
library(MASS)
library(boot)
library(pROC)
library(lift)
library(rpart)
library(randomForest)

modelPath = file.path("U:/Midterm/")
outPath = file.path("U:/Midterm/")
codePath = file.path("U:/Midterm/")

#load functions
source(file.path(codePath,"DataPreparation DB.R"))

# Use the load command to load objects from your .RData files.
load(file.path(modelPath,"modelPart2.RData"))
load(file.path(modelPath,"modelPart3.RData"))

########################################
## Exercise 1
## Read Data from CSV File
########################################

valData <- read.csv(file="U:/Midterm/valSample.csv", header=TRUE, sep=",", na.strings=NA)

valData$DONR = as.factor(valData$DONR)
valData$HOME = as.factor(valData$HOME)
valData$HINC = as.factor(valData$HINC)

########################################
## Exercise 2
## Predictions on Validation Set
########################################

## Part A - Apply the Part 2 data processing steps to valData
valDataPart2 = processPart2(valData)

levels(valDataPart2$RFA_96_A) = c(levels(valDataPart2$RFA_96_A), "B")

## Part B - Predict DAMT for valData using your chosen model from Part 2

class(modelPart2)

valData$DAMT.Pred =  predict(modelPart2,valDataPart2)

# Check the predictions as a sanity check

hist(valData$DAMT.Pred,xlab="DAMT",main="Validation Set",col="gray",breaks=50)
par(pty="s")
plot(valData$DAMT,valData$DAMT.Pred,xlab="Target DAMT",ylab="Predicted DAMT",
     main="Validation Set")
abline(0,1,col="red")
par(pty="m")


## Part C - Apply the Part 3 data processing steps to valData
valDataPart3 = processPart3(valData)
levels(valDataPart3$RFA_96_A) = c(levels(valDataPart3$RFA_96_A), "B")

## Part D - Predict DONR and PDONR for valData using your chosen model from Part 3
# Recall DONR = 0 or 1 is the predicted class and PDONR in [0,1] is the predicted 
# probability.

assignClass = function(probVals,threshVal)
{
  predVals = rep(0,length(probVals))
  predVals[probVals > threshVal] = 1
  predVals = factor(predVals)
  
  return(predVals)
}

# Further note that for a logistic regression model, the probabilities (PDONR) come 
# from predict.glm and the classifications (DONR) come from applying the optimal threshold.

valData$PDONR.Pred = predict(modelPart3,newdata=valDataPart3,type="response")
valData$DONR.Pred = assignClass(valData$PDONR.Pred,optThreshPart3)

# Check the predictions as a sanity check
table(valData$DONR,valData$DONR.Pred,dnn=c("Target","Predicted"))
hist(valData$PDONR.Pred,xlab="P(DONR=1)",main="Validation Set",col="gray",breaks=50,
     xlim=c(0,1))
plot(valData$DONR,valData$PDONR.Pred,xlab="Target DONR Value",
     ylab="Predicted P(DONR=1)",main="Validation Set")

########################################
## Exercise 3
## Mailing List Selection
########################################

# "Source" the file RankedDonorOutput.R in order to put the function 

source(file.path(codePath,"RankedDonorOutput.R"))

# Rank donors by PDONR.Pred
numBins = 10
out1 = outputForRankedDonors(numBins,rankVar="PDONR.Pred",dataToRank=valData)
print(out1$Donor.Table)
print(out1$Mailing.Table)

# Rank donors by EXAMT.Pred (expected donation amount)
# EXAMT.Pred = PDONR.Pred * DAMT.Pred 
# (likelihood of donation * predicted donation amount)
valData$EXAMT.Pred = valData$PDONR.Pred * valData$DAMT.Pred
out2 = outputForRankedDonors(numBins,rankVar="EXAMT.Pred",dataToRank=valData)
print(out2$Donor.Table)
print(out2$Mailing.Table)

# Rank donors by DAMT.Pred (predicted donation amount)
out3 = outputForRankedDonors(numBins,rankVar="DAMT.Pred",dataToRank=valData)
print(out3$Donor.Table)
print(out3$Mailing.Table)

fn1 = ecdf(out1$breakVals)
fn2 = ecdf(out2$breakVals)
fn3 = ecdf(out3$breakVals)
yLimits = c(-500+1000*floor(min(c(
  out1$Mailing.Table$Total.Profit,
  out2$Mailing.Table$Total.Profit,
  out3$Mailing.Table$Total.Profit
))/1000),
500+1000*ceiling(max(c(
  out1$Mailing.Table$Total.Profit,
  out2$Mailing.Table$Total.Profit,
  out3$Mailing.Table$Total.Profit
))/1000))
plot(fn1(out1$breakVals)[-1],out1$Mailing.Table$Total.Profit,type='b',col="blue",
     xlab="% Mailed",ylab="Profit ($)",main="Profit Profiles",xlim=c(0,1),ylim=yLimits)
lines(fn2(out2$breakVals)[-1],out2$Mailing.Table$Total.Profit,col="red")
points(fn2(out2$breakVals)[-1],out2$Mailing.Table$Total.Profit,col="red",pch=16)
lines(fn3(out3$breakVals)[-1],out3$Mailing.Table$Total.Profit,col="green")
points(fn3(out3$breakVals)[-1],out3$Mailing.Table$Total.Profit,col="green",pch=16)
abline(h=0,lty=2)
legend(x="bottomleft",legend=c("PDONR","EXAMT","DAMT","Break Even"),
       col=c("blue","red","green","black"),
       lty=c(1,1,1,2),pch=c(1,16,16,NA))

cutOff = out2$breakVals[numBins+1-1]
valMailList = data.frame(ID=valData$ID[valData$EXAMT.Pred >= cutOff])
length(valMailList$ID)

########################################
## Exercise 4
## Predictions on Test Set
########################################

## Part A - Repeat Exercise 1 on projectDataTEST.csv
testData <- read.csv(file="U:/Midterm/testSample.csv", header=TRUE, sep=",", na.strings=NA)

testData$HOME = as.factor(testData$HOME)
testData$HINC = as.factor(testData$HINC)

## Part B - Repeat Exercise 2 on projectDataTEST.csv

# Note: The model.matrix method will not allow us to use a dataframe with "missing" 
# columns. Therefore, we add dummy DAMT and DONR columns to testData.
testData$DAMT = -1
testData$DONR = -1

## Apply the Part 2 data processing steps to testData
testDataPart2 = processPart2(testData)
levels(testDataPart2$RFA_96_A) = c(levels(testDataPart2$RFA_96_A), "B")

## Predict DAMT for testData using your chosen model from Part 2
# Note that the model I am using is a glmnet model.

testData$DAMT.Pred =  predict(modelPart2,testDataPart2)
fix(testData)

# Check the predictions as a sanity check
summary(testData$DAMT.Pred)

## Apply the Part 3 data processing steps to valData
testDataPart3 = processPart3(testData)
levels(testDataPart3$RFA_96_A) = c(levels(testDataPart3$RFA_96_A), "B")

## Predict DONR and PDONR for valData using your chosen model from Part 3
# Note that the model I am using is a glm model.

testData$PDONR.Pred = predict(modelPart3,newdata=testDataPart3,type="response")
testData$DONR.Pred = assignClass(testData$PDONR.Pred,optThreshPart3)

# Check the predictions as a sanity check
table(testData$DONR.Pred)
summary(testData$PDONR.Pred)


# Rank donors by EXAMT.Pred (expected donation amount)
# EXAMT.Pred = PDONR.Pred * DAMT.Pred 
# (likelihood of donation * predicted donation amount)
testData$EXAMT.Pred = testData$PDONR.Pred * testData$DAMT.Pred

## Part C - Write Test Set Predictions to CSV File
# Name the columns in the CSV file ID, DONR, PDONR, DAMT
testPredOut = data.frame(ID = testData$ID,
                         DONR = testData$DONR.Pred,
                         PDONR = testData$PDONR.Pred,
                         DAMT = testData$DAMT.Pred)



write.csv(testPredOut,file=file.path(outPath,"projectPredictionsTEST.csv"),
          row.names=FALSE)

## Part D - Apply Mailing List Strategy to Test Data
# Use cutoff selected above.
testMailList = data.frame(ID=testData$ID[testData$EXAMT.Pred >= cutOff])
length(testMailList$ID)

## Part E - Write Test Set Mailing List to CSV File
write.csv(testMailList,file=file.path(outPath,"projectListTEST.csv"),row.names=FALSE)


