########################################
## PREDICT 422
## Charity Project - Part 4 (The Mailing List Problem)
##
## SampleCodePart4.R
########################################

# RECOMMENDATIONS: Before working with SampleCodePart4.R, perform the following steps.
#   1. Alter the functions processPart1 and processPart2 in DataPreparation.R to match
#      the data preparation steps you applied in Exercise 2 of Parts 2 and 3 of the 
#      project.
#   2. Execute the examples in SaveYourModels.R. Start with a new R session (nothing 
#      in memory) for working with SampleCodePart4.R.

# Load packages required for this code.
library(glmnet)

########################################
## Exercise 1
## Read Data from CSV File
########################################

# This path is specified wrt a Mac, the Windows path will start differently
inPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                   "Project - Charity Mailing","Project Data Files")

valData = read.csv(file.path(inPath,"valSample.csv"),na.strings=c("NA"," "))

# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
valData$DONR = as.factor(valData$DONR)
valData$HOME = as.factor(valData$HOME)
valData$HINC = as.factor(valData$HINC)

########################################
## Exercise 2
## Predictions on Validation Set
########################################

# Make sure you have altered the functions processPart2 and processPart3 in 
# DataPreparation.R to match the data processing steps you took in Parts 2 and 3
# of the project.

# Make sure you have saved your models from Part 2 and Part 3 using the examples given
# in SaveYourModels.R before you begin this exercise.

# "Source" the file DataPrepration.R in order to put the data processing functions
# into memory. Define the file path to be the location of the file DataPreparation.R.
codePath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                     "Project - Charity Mailing","R Code")
source(file.path(codePath,"DataPreparation.R"))

## Part A - Apply the Part 2 data processing steps to valData
valDataPart2 = processPart2(valData)

# Note that RFA_96_A for valData does not include the level "B". I had to do some 
# investigating to track down an error that originated from this fact. Therefore, we
# will add the level so that we don't have problems with making predictions.
levels(valDataPart2$RFA_96_A) = c(levels(valDataPart2$RFA_96_A), "B")

## Part B - Predict DAMT for valData using your chosen model from Part 2
modelPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                      "Project - Charity Mailing","Project Data Files")
load(file.path(modelPath,"modelPart2.RData"))

# Note that the model I am using is a glmnet model. I must call the predict function
# using the syntax for a glmnet model. Search for help on predict.glmnet for details.
# You can type class(modelPart2) on the command line (after you have loaded the model)
# to determine the class of the model.
x = model.matrix(DAMT ~ .-ID,data=valDataPart2)[,-1]
valData$DAMT.Pred = as.numeric(predict(modelPart2,newx=x,type="response"))

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
load(file.path(modelPath,"modelPart3.RData"))

# Note that the model I am using is a glm model. I must call the predict function
# using the syntax for a glm model. Search for help on predict.glm for details.
# You can type class(modelPart3) on the command line (after you have loaded the model)
# to determine the class of the model.
assignClass = function(probVals,threshVal)
{
  predVals = rep(0,length(probVals))
  predVals[probVals > threshVal] = 1
  predVals = factor(predVals)
  
  return(predVals)
}

# Further note that for a logistic regression model, the probabilities (PDONR) come 
# from predict.glm and the classifications (DONR) come from applying the optimal threshold.
# Each predict method should have some means of obtaining the probabilities. You
# will have to check the documentation for the type of model you are using to 
# determine the appropriate syntax.
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
# outputForRankedDonors into memory. It is assumed here that all of your R code
# for this part of the project is located in one file directory.
source(file.path(codePath,"RankedDonorOutput.R"))

## Evaluate various mailing list strategies using function outputForRankedDonors

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

# Plot profit profiles
# Note, the following code is minorly complicated by the fact that there may be
# fewer than numBins unique PDONR values. I have worked out the complications, but
# it makes the x-axis for the plot a bit less intuitive.

# Calculate percentiles of breakVals for each profile using the empircal CDF function.
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

# Currently, I am seeing that the highest profit is obtained by mailing the top 2 bins
# ranked by DAMT.Pred. Note that the donation and profit values are measured from the 
# known target data. However, the potential for profit is driven by the RANKING of 
# individuals within the dataset. The ranking is determined by the choice of 
# metric (e.g. EXAMT) and by your models' predicted values. A different choice of 
# models might re-order the individuals so that more individuals worth mailing work 
# their way towards the top and a higher profit is obtained from mailing fewer 
# individuals.
#
# For this coding example, suppose selecting the top 2 bins determined by DAMT.Pred 
# is deemed to be the  optimal selection criterion (results using your models may 
# vary). Apply cutoff to valData.
cutOff = out3$breakVals[numBins+1-2]
valMailList = data.frame(ID=valData$ID[valData$DAMT.Pred >= cutOff])
length(valMailList$ID)

########################################
## Exercise 4
## Predictions on Test Set
########################################

## Part A - Repeat Exercise 1 on projectDataTEST.csv
testData = read.csv(file.path(inPath,"projectDataTEST.csv"),na.strings=c("NA"," "))

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
x = model.matrix(DAMT ~ .-ID,data=testDataPart2)[,-1]
testData$DAMT.Pred = as.numeric(predict(modelPart2,newx=x,type="response"))

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

## Part C - Write Test Set Predictions to CSV File
# Name the columns in the CSV file ID, DONR, PDONR, DAMT
testPredOut = data.frame(ID = testData$ID,
                         DONR = testData$DONR.Pred,
                         PDONR = testData$PDONR.Pred,
                         DAMT = testData$DAMT.Pred)

outPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                    "Project - Charity Mailing","Project Data Files")

write.csv(testPredOut,file=file.path(outPath,"projectPredictionsTEST.csv"),
          row.names=FALSE)

## Part D - Apply Mailing List Strategy to Test Data
# Use cutoff selected above.
testMailList = data.frame(ID=testData$ID[testData$DAMT.Pred >= cutOff])
length(testMailList$ID)

## Part E - Write Test Set Mailing List to CSV File
write.csv(testMailList,file=file.path(outPath,"projectListTEST.csv"),row.names=FALSE)
