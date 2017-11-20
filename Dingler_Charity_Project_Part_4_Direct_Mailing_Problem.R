########################################
## PREDICT 422
## Dingler Charity Project - Part 4 (The Mailing List Problem)

########################################

# Load packages required for this code.

########################################
## Exercise 1
## Read Data from CSV File
########################################

valData = read.csv("valSample.csv", na.strings=c("NA"," "))

########################################
## Exercise 2
## Predictions on Validation Set
########################################

## Part A - Apply the Part 2 data processing steps to valData
source("Dingler_DataPreparation.R")
valData <- processPart(valData)

## Part B - Predict DAMT for valData using your chosen model from Part 2
load("modelPart2.RData")

predict.regsubsets = function (object ,newdata ,id ,...){
        form=as.formula(object$call [[2]])
        mat=model.matrix(form,newdata)
        coefi=coef(object ,id=id)
        xvars=names(coefi)
        mat[,xvars]%*%coefi
}

valData$DAMT.Pred = predict.regsubsets(modelPart2, valData, id = 3)

# Check the predictions as a sanity check
hist(valData$DAMT.Pred,xlab="DAMT",main="Validation Set",col="gray",breaks=50)
par(pty="s")
plot(valData$DAMT,valData$DAMT.Pred,xlab="Target DAMT",ylab="Predicted DAMT",
     main="Validation Set")
abline(0,1,col="red")
par(pty="m")

## Part C - Apply the Part 3 data processing steps to valData
# This was the same as part 2

## Part D - Predict DONR and PDONR for valData using your chosen model from Part 3
# Recall DONR = 0 or 1 is the predicted class and PDONR in [0,1] is the predicted 
# probability.
load("modelPart3.RData")

assignClass = function(probVals,threshVal)
{
  predVals = rep(0,length(probVals))
  predVals[probVals > threshVal] = 1
  predVals = factor(predVals)
  
  return(predVals)
}

valData$PDONR.Pred = predict(modelPart3, newdata=valData, type="prob")[,2]
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
source("RankedDonorOutput.R")

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

# I am seeing that the highest profit is obtained by mailing the top 2 bins
# ranked by DAMT.Pred. 

# selecting the top 2 bins determined by DAMT.Pred 
# is deemed to be the  optimal selection criterion 
cutOff = out2$breakVals[numBins+1-2]
valMailList = data.frame(ID=valData$ID[valData$DAMT.Pred >= cutOff])
length(valMailList$ID)

########################################
## Exercise 4
## Predictions on Test Set
########################################

## Part A - Repeat Exercise 1 on projectDataTEST.csv
testData = read.csv("testSample.csv",na.strings=c("NA"," "))

## Part B - Repeat Exercise 2 on projectDataTEST.csv

# Note: The model.matrix method will not allow us to use a dataframe with "missing" 
# columns. Therefore, we add dummy DAMT and DONR columns to testData.
testData$DAMT = -1
testData$DONR = -1
  
## Apply the Part 2 data processing steps to testData
testData <- processPart(testData)

## Predict DAMT for testData using your chosen model from Part 2
testData$DAMT.Pred = predict.regsubsets(modelPart2, testData, id = 3)

# Check the predictions as a sanity check
summary(testData$DAMT.Pred)

## Apply the Part 3 data processing steps to valData
# This was the same as part 2

## Predict DONR and PDONR for valData using your chosen model from Part 3
# Note that the model I am using is a glm model.
testData$PDONR.Pred = predict(modelPart3, newdata=testData, type="prob")[,2]
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

write.csv(testPredOut,file="projectPredictionsTEST.csv",
          row.names=FALSE)

## Part D - Apply Mailing List Strategy to Test Data
# Use cutoff selected above.
testMailList = data.frame(ID=testData$ID[testData$DAMT.Pred >= cutOff])
length(testMailList$ID)

## Part E - Write Test Set Mailing List to CSV File
write.csv(testMailList,file="projectListTEST.csv",row.names=FALSE)
