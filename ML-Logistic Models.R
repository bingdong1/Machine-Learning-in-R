#rm(list = ls())
#.libPaths("\\\\file-chi-2/dbing$/R/win-library/3.3")

library(mice)
library(Amelia)
library(caret)
library(leaps)
library(pls)
library(doParallel)
library(foreach)
library(ggplot2)
library(pscl)
library(MASS)
library(boot)
library(pROC)
library(lift)
library(rpart)
library(randomForest)
library(rattle)
library(rpart.plot)

memory.size(max=T)
c1=makeCluster(3)
registerDoParallel(c1)

#test
foreach(i=1:3) %dopar% i^100

#1 Read in Data

classData <- read.csv(file="U:/Midterm/trainSample.csv", header=TRUE, sep=",", na.strings=NA)

#2 Data Preparation

# (1) checking missing data

colSums(sapply(classData1, is.na))

#impute missing value use MICE

#Remove variables
exclude <- c('ID', 'DONR', 'DAMT')
include <- setdiff(names(classData), exclude)

classData_MICE <- classData[include]

#Missing value imputation
imp.classData <- mice(classData_MICE, m = 1, method='cart')

classData_MICE <- complete(imp.classData) 

#Check missing data after impuatation
colSums(sapply(classData_MICE, is.na))

#combine data
classData_final <- data.frame(classData[exclude],classData_MICE)

#rm(classData_MICE)

# (2) set up function

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
classData_final = separateRFA(classData_final,"RFA_96")

# Recategorization

classData_final$GENDER <- ifelse(classData_final$GENDER == 'A', 'A',
                          ifelse(classData_final$GENDER == "C", 'A',
                                 ifelse(classData_final$GENDER == 'J', 'A', 
                                        ifelse(classData_final$GENDER == 'U', 'A', 
                                               ifelse(classData_final$GENDER == 'F', 'F',
                                                      ifelse(classData_final$GENDER == 'M', 'M', NA))))))


classData_final$RFA_96_R <- ifelse(classData_final$RFA_96_R == 'A', 'A',
                            ifelse(classData_final$RFA_96_R == "S", 'S',
                                   ifelse(classData_final$RFA_96_R == 'F', 'FNL', 
                                          ifelse(classData_final$RFA_96_R == 'L', 'FNL', 
                                                 ifelse(classData_final$RFA_96_R == 'N', 'FNL',NA )))))


classData_final$RFA_96_A <- ifelse(classData_final$RFA_96_A == 'B', 'D',
                            ifelse(classData_final$RFA_96_A == "C", 'D',
                                   ifelse(classData_final$RFA_96_A == 'D', 'D', 
                                          ifelse(classData_final$RFA_96_A == 'E', 'E', 
                                                 ifelse(classData_final$RFA_96_A == 'F', 'F',
                                                        ifelse(classData_final$RFA_96_A == 'G', 'G',NA ))))))



# Convert categorical variables to factors

classData_final$DONR = as.factor(classData_final$DONR)
classData_final$HOME = as.factor(classData_final$HOME)
classData_final$HINC = as.factor(classData_final$HINC)
classData_final$RFA_96_A = as.factor(classData_final$RFA_96_A)
classData_final$RFA_96_R = as.factor(classData_final$RFA_96_R)
classData_final$GENDER = as.factor(classData_final$GENDER)

#Drop unnecessary variables

drop <- c('ID', 'RFA_96', 'DAMT') 

classData_final = classData_final[,!(names(classData_final) %in% drop)]


#3 Dataset Partitioning

#test set
testindex <- createDataPartition(y=classData_final$DONR, p=0.25, list=FALSE)    

test_data <- classData_final[ testindex,]

barplot(table(test_data$DONR),xlab="DONR")

summary(train_data$MEDHVAL)

dim(test_data)

#910/16809
#[1] 0.05413766


#train set

train_data  <- classData_final[-testindex,]

barplot(table(train_data$DONR),xlab="DONR")

dim(train_data)

summary(train_data)

#2727/50425
#[1] 0.05408032


#4 Model Fitting

#stepwise variable selection

#forward stepwise regression Criterion
reg0=glm(DONR~1,data=train_data,family=binomial(link="logit"))
reg1=glm(DONR~.,data=train_data,family=binomial(link="logit"))

# aic Criterion
step(reg0,scope=formula(reg1),
        direction="forward",k=2)   

#most important variable
#RFA_96_A

#glm(formula = DONR ~ RFA_96_A + TDON + MEDHVAL + RFA_96_F + NUMPROM + 
#      HINC + NUMPRM12, family = binomial(link = "logit"), data = train_data)

#Aic 21110

# BIC Criterion

step(reg0,scope=formula(reg1),
        direction="forward",k=log(nrow(train_data))) 

#most important variable
#RFA_96_A

#glm(formula = DONR ~ RFA_96_A + TDON + MEDHVAL + NGIFTALL + RFA_96_F, 
#family = binomial(link = "logit"), data = train_data)

#Aic 21140

# chisq Test
step(reg0,
     scope = list(upper=reg1),
     direction="both",
     test="Chisq",
     data=train_data)

#most important variable
#RFA_96_A

#glm(formula = DONR ~ RFA_96_A + TDON + MEDHVAL + RFA_96_F + NUMPROM + 
#HINC + NUMPRM12, family = binomial(link = "logit"), data = train_data)

#AIC: 21110

#Simple logistic regression

glm.fits=glm(DONR~ RFA_96_A,train_data,family=binomial)
glm.probs=predict(glm.fits,type='response')

#cv.err <- cv.glm(train_data, glm.fits, cost, K=11)$delta

#glm.pred <- ifelse(glm.probs > 0.06134882,1,0)
#table(train_data$DONR,glm.pred)

hist(glm.probs,col="gray")   # Note that scores are distributed around 0.05.
hist(glm.probs,col="gray",xlim=c(0,1))  

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA1 = roc(response=train_data$DONR,predictor=glm.probs)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA1,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA1$auc,digits=3),sep=""))
#0.583
par(pty="m")

dist01 = sqrt((rocA1$specificities-1)^2 + (rocA1$sensitivities-1)^2)
optIdxA1 = which.min(dist01)  # index corresponding to minimum distance
#generate the threshold
threshA1 = rocA1$thresholds[optIdxA1]  # threshold corresponding to min. distance
#0.05343944
points(rocA1$specificities[optIdxA1],rocA1$sensitivities[optIdxA1],col="red",pch=7)

# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(glm.probs,train_data$DONR)
TopDecileLift(glm.probs,train_data$DONR)
#1.65

SimpleLM <- ifelse(glm.probs>threshA1, 1, 0) # mail to everyone above the cutoff

table(train_data$DONR,SimpleLM)

#table(SimpleLM, train_data$DONR)

#SimpleLM
#    0     1
#0 35012 15413
#1  1560  1167

#TRUE POSITIVE RATE
1167/(1560+1167)
#0.4279428

#FALSE POSITIVE RATE
15413/(15413+35012)
#0.3056619

#accuracy rate
mean(SimpleLM==train_data$DONR)
#0.6806705

#misClasificError
mean(SimpleLM != train_data$DONR)
#0.3193295

#Simple Logistic Regression Test Data

glm.probs_test=predict(glm.fits,newdata = test_data, type='response')

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA1_test = roc(response=test_data$DONR,predictor=glm.probs_test)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA1_test,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA1_test$auc,digits=3),sep=""))
#0.558
par(pty="m")

dist01_test = sqrt((rocA1_test$specificities-1)^2 + (rocA1_test$sensitivities-1)^2)
optIdxA1_test = which.min(dist01_test)  # index corresponding to minimum distance
#generate the threshold
threshA1_test = rocA1_test$thresholds[optIdxA1_test]  # threshold corresponding to min. distance
#0.05343944
points(rocA1_test$specificities[optIdxA1_test],rocA1_test$sensitivities[optIdxA1_test],col="red",pch=7)

# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(glm.probs_test,test_data$DONR)
TopDecileLift(glm.probs_test,test_data$DONR)
#1.56

SimpleLM_test <- ifelse(glm.probs_test>threshA1_test, 1, 0) # mail to everyone above the cutoff

table(test_data$DONR,SimpleLM_test)

#SimpleLM_test
#   0     1
#0 11666  5143
#1   543   367

#TRUE POSITIVE RATE
367/(367+543)
#0.4032967

#FALSE POSITIVE RATE
5143/(5143+11666)
#0.305967

#accuracy rate
mean(SimpleLM_test==test_data$DONR)
#0.6791015

#misClasificError
mean(SimpleLM_test != test_data$DONR)
#0.3208985

#PROFITBILITY 

#profit.log1 <- cumsum(15.62*train_data$DONR[order(glm.probs, decreasing=T)]-0.99)
#plot(profit.log1) # see how profits change as more mailings are made
#n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
#c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit

#cutoff.log1 <- sort(glm.probs, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
#chat.valid.log1 <- ifelse(glm.probs>cutoff.log1, 1, 0) # mail to everyone above the cutoff
#table(chat.valid.log1, train_data$DONR) # classification table
#confusionMatrix(chat.valid.log1,train_data$DONR)

#chat.valid.log1     0     1
#                0 46530  2346
#                1  3895   381
                
# check n.mail.valid = 3895+381 = 4276
#15.62*381-0.99*4276 = 1717.98

#multiple logistic regression

glm.fits2=glm(DONR~ RFA_96_A + TDON + MEDHVAL + RFA_96_F + NUMPROM + 
               HINC + NUMPRM12,train_data,family=binomial(link = "logit"))

glm.probs2=predict(glm.fits2,type='response')

hist(glm.probs2,col="gray")   # Note that scores are distributed around 0.05.
hist(glm.probs2,col="gray",xlim=c(0,1))  

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA2 = roc(response=train_data$DONR,predictor=glm.probs2)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA2,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA2$auc,digits=3),sep=""))
#0.617
par(pty="m")

dist02 = sqrt((rocA2$specificities-1)^2 + (rocA2$sensitivities-1)^2)
optIdxA2 = which.min(dist02)  # index corresponding to minimum distance
#generate the threshold
threshA2 = rocA2$thresholds[optIdxA2]  # threshold corresponding to min. distance
#[1] 0.05169483
points(rocA2$specificities[optIdxA2],rocA2$sensitivities[optIdxA2],col="red",pch=7)

# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(glm.probs2,train_data$DONR)
TopDecileLift(glm.probs2,train_data$DONR)
#1.825

MulitLM <- ifelse(glm.probs2>threshA2, 1, 0) # mail to everyone above the cutoff

table(train_data$DONR,MulitLM)

#MulitLM
#    0     1
#0 30914 19511
#1  1194  1533

#TRUE POSITIVE RATE
1533/(1533+1194)
#0.5621562

#FALSE POSITIVE RATE
19511/(19511+30914)
#0.3869311

#accuracy rate
mean(MulitLM==train_data$DONR)
#0.6104568

#misClasificError
mean(MulitLM != train_data$DONR)
#0.3895432

#Mulitple Logistic Regression Test Data

glm.probs2_test=predict(glm.fits2,newdata = test_data, type='response')

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA2_test = roc(response=test_data$DONR,predictor=glm.probs2_test)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA2_test,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA2_test$auc,digits=3),sep=""))
#0.598
par(pty="m")

dist02_test = sqrt((rocA2_test$specificities-1)^2 + (rocA2_test$sensitivities-1)^2)
optIdxA2_test = which.min(dist02_test)  # index corresponding to minimum distance
#generate the threshold
threshA2_test = rocA2_test$thresholds[optIdxA2_test]  # threshold corresponding to min. distance
#0.05113597
points(rocA2_test$specificities[optIdxA2_test],rocA2_test$sensitivities[optIdxA2_test],col="red",pch=7)

# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(glm.probs2_test,test_data$DONR)
TopDecileLift(glm.probs2_test,test_data$DONR)
#1.868

MultiLM_test <- ifelse(glm.probs2_test>threshA2_test, 1, 0) # mail to everyone above the cutoff

table(test_data$DONR,MultiLM_test)

#MultiLM_test
#  0     1
#0 10050  6759
#1   412   498

#TRUE POSITIVE RATE
498/(498+412)
#0.5472527

#FALSE POSITIVE RATE
6759/(6759+10050)
#0.402106

#accuracy rate
mean(MultiLM_test==test_data$DONR)
#0.5952932

#misClasificError
mean(MultiLM_test != test_data$DONR)
#0.4047068

#profitability
#glm.pred <- ifelse(glm.probs > 0.06134882,1,0)
#table(train_data$DONR,glm.pred)

#profit.log1 <- cumsum(15.62*train_data$DONR[order(glm.probs, decreasing=T)]-0.99)
#plot(profit.log1) # see how profits change as more mailings are made

#n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits

#c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit

#cutoff.log1 <- sort(glm.probs, decreasing=T)[n.mail.valid] # set cutoff based on n.mail.valid
#chat.valid.log1 <- ifelse(glm.probs>cutoff.log1, 1, 0) # mail to everyone above the cutoff
#table(chat.valid.log1, train_data$DONR) # classification table
#confusionMatrix(chat.valid.log1,train_data$DONR)

#chat.valid.log1     0     1
#                0 35887  1502
#                1 14538  1225

# check n.mail.valid = 14538+1225 = 15762
#15.62*1225-0.99*15763 = 1717.98


#tree

fullTree = rpart(DONR ~  RFA_96_A + TDON + MEDHVAL + RFA_96_F + NUMPROM + HINC + NUMPRM12,
                 data=train_data,method="class",
                 parms=list(split="gini",loss=matrix(c(0,15.62,0.68,0),nrow=2,ncol=2)))

summary(fullTree)
plot(fullTree)
text(fullTree,pretty=0)

#Create fancy plot
fancyRpartPlot(fullTree, sub = "")

# Prune the tree
printcp(fullTree)
cpBest = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]),"CP"]
modelC1 = prune(fullTree,cp=cpBest) # In this case, the optimal tree is the unpruned tree
summary(modelC1)
plot(modelC1)
text(modelC1,pretty=0)

fancyRpartPlot(modelC1, sub = "")

trnProbsC1 = predict(modelC1,newdata=train_data,type="prob")[,2]
plotLift(trnProbsC1,train_data$DONR)
TopDecileLift(trnProbsC1,train_data$DONR)
#1.306

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA3 = roc(response=train_data$DONR,predictor=trnProbsC1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA3,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA3$auc,digits=3),sep=""))
par(pty="m")

dist03 = sqrt((rocA3$specificities-1)^2 + (rocA3$sensitivities-1)^2)
optIdxA3 = which.min(dist03)  # index corresponding to minimum distance
#generate the threshold
threshA3 = rocA3$thresholds[optIdxA3]  # threshold corresponding to min. distance
#[1] 0.06082376
points(rocA3$specificities[optIdxA3],rocA3$sensitivities[optIdxA3],col="red",pch=7)

tree <- ifelse(trnProbsC1>threshA3, 1, 0) # mail to everyone above the cutoff

table(train_data$DONR,tree)

#tree
#     0     1
# 0 35012 15413
# 1 1560  1167

#TRUE POSITIVE RATE
1167/(1167+1560)
#0.4279428

#FALSE POSITIVE RATE
15413/(15413+35012)
#0.3056619

#accuracy rate
mean(tree==train_data$DONR)
#0.6806705

#misClasificError
mean(tree != train_data$DONR)
#0.3193295

#tree test data

trnProbsC1_test = predict(modelC1,newdata=test_data,type="prob")[,2]
plotLift(trnProbsC1_test,test_data$DONR)
TopDecileLift(trnProbsC1_test,test_data$DONR)
#1.516

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA3_test = roc(response=test_data$DONR,predictor=trnProbsC1_test)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA3_test,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA3_test$auc,digits=3),sep=""))
#0.571
par(pty="m")

dist03_test = sqrt((rocA3_test$specificities-1)^2 + (rocA3_test$sensitivities-1)^2)
optIdxA3_test = which.min(dist03_test)  # index corresponding to minimum distance
#generate the threshold
threshA3_test = rocA3_test$thresholds[optIdxA3_test]  # threshold corresponding to min. distance
#[1] 0.06000295
points(rocA3_test$specificities[optIdxA3_test],rocA3_test$sensitivities[optIdxA3_test],col="red",pch=7)

tree_test <- ifelse(trnProbsC1_test>threshA3_test, 1, 0) # mail to everyone above the cutoff

table(test_data$DONR,tree_test)

#tree_test
#    0     1
#0 11584  5225
#1   518   392

#TRUE POSITIVE RATE
392/(392+518)
#0.4307692

#FALSE POSITIVE RATE
5525/(5525+11548)
#0.3236104

#accuracy rate
mean(tree_test==test_data$DONR)
#0.6758846

#misClasificError
mean(tree_test != test_data$DONR)
#0.3241154

#random forest

bag.donr = randomForest(DONR ~ RFA_96_A + TDON + MEDHVAL + RFA_96_F + NUMPROM + HINC + NUMPRM12, data = train_data, mtry = 5, ntree = 500, 
                            importance = T)

bag.pred1 = predict(bag.donr,newdata=train_data,type="prob")[,2]
plotLift(bag.pred1,train_data$DONR)
TopDecileLift(bag.pred1,train_data$DONR)

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA4 = roc(response=train_data$DONR,predictor=bag.pred1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA4,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA4$auc,digits=3),sep=""))
par(pty="m")

dist04 = sqrt((rocA4$specificities-1)^2 + (rocA4$sensitivities-1)^2)
optIdxA4 = which.min(dist04)  # index corresponding to minimum distance
#generate the threshold
threshA4 = rocA4$thresholds[optIdxA4]  # threshold corresponding to min. distance
#[1] 0.234
points(rocA4$specificities[optIdxA4],rocA4$sensitivities[optIdxA4],col="red",pch=7)

randomforest <- ifelse(bag.pred1>threshA4, 1, 0) # mail to everyone above the cutoff

table(train_data$DONR,randomforest)

#randomforest
#   0     1
#0 50415    10
#1     0  2727

#TRUE POSITIVE RATE
2727/(2727+0)
#1

#FALSE POSITIVE RATE
10/(10+50415)
#0.0001983143

#accuracy rate
mean(randomforest==train_data$DONR)
#0.9998119

#misClasificError
mean(randomforest != train_data$DONR)
#0.0001881397

#Random Forest test data

bag.pred2 = predict(bag.donr,newdata=test_data,type="prob")[,2]

plotLift(bag.pred2,test_data$DONR)
TopDecileLift(bag.pred2,test_data$DONR)
#1.352

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA4b = roc(response=test_data$DONR,predictor=bag.pred2)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA4b,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA4b$auc,digits=3),sep=""))
par(pty="m")

dist04b = sqrt((rocA4b$specificities-1)^2 + (rocA4b$sensitivities-1)^2)
optIdxA4b = which.min(dist04b)  # index corresponding to minimum distance
#generate the threshold
threshA4b = rocA4b$thresholds[optIdxA4b]  # threshold corresponding to min. distance
#[1] 0.045
points(rocA4b$specificities[optIdxA4b],rocA4b$sensitivities[optIdxA4b],col="red",pch=7)

randomforest2 <- ifelse(bag.pred2>threshA4b, 1, 0) # mail to everyone above the cutoff

table(test_data$DONR,randomforest2)

randomforest2
#0    1
#0 8735 8074
#1  407  503

#TRUE POSITIVE RATE
503/(503+407)
#0.5527473

#FALSE POSITIVE RATE
8074/(8074+8735)
#0.4803379

#accuracy rate
mean(randomforest2==test_data$DONR)
#0.5213613

#misClasificError
mean(randomforest2 != test_data$DONR)
#0.4786387

#LDA

modelRR = lda(DONR ~ RFA_96_A + TDON + MEDHVAL + RFA_96_F + NUMPROM 
              + HINC + NUMPRM12, data = train_data)

modelRR

predRR = predict(modelRR,data = train_data)
trnProbsRR = predRR$posterior[,2]   # column 2 corresponds to Pr(DONR = 1)

# Similar to modelA1, we explore the probabilities and build a ROC curve 
# for modelB1.
hist(trnProbsRR,col="gray")   # Note that scores are distributed around 0.05.
hist(trnProbsRR,col="gray",xlim=c(0,1))   # Rescale to make obvious.

# Classification: ROC Curve for Model B1 - Use methods from pROC package.
rocRR = roc(response=train_data$DONR,predictor=trnProbsRR)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocRR,col="blue",
     main=paste("ROC curve for Model B1\nAUC = ",round(rocRR$auc,digits=3),sep=""))
par(pty="m")

# Classification: Determine "optimal" threshold.
distRR = sqrt((rocRR$specificities-1)^2 + (rocRR$sensitivities-1)^2)
optIdxRR = which.min(distRR)  # index corresponding to minimum distance
threshRR = rocRR$thresholds[optIdxRR]  # threshold corresponding to min. distance
#0.04883544
points(rocRR$specificities[optIdxRR],rocRR$sensitivities[optIdxRR],col="red",pch=7)


# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(trnProbsRR,train_data$DONR)
TopDecileLift(threshRR,train_data$DONR)
#1.04

RR <- ifelse(trnProbsRR>threshRR, 1, 0) # mail to everyone above the cutoff

table(train_data$DONR,RR)

#RR
#   0     1
#0 29565 20860
#1  1123  1604

#TRUE POSITIVE RATE
1604/(1604+1123)
#0.5881922

#FALSE POSITIVE RATE
20860/(20860+29565)
#0.4136837

#accuracy rate
mean(RR==train_data$DONR)
#0.5864126

#misClasificError
mean(RR != train_data$DONR)
#0.4135874

#LDA test data

predRR2 = predict(modelRR,test_data)
trnProbsRR2 = predRR2$posterior[,2]   # column 2 corresponds to Pr(DONR = 1)

# Similar to modelA1, we explore the probabilities and build a ROC curve 
# for modelB1.
hist(trnProbsRR2,col="gray")   # Note that scores are distributed around 0.05.
hist(trnProbsRR2,col="gray",xlim=c(0,1))   # Rescale to make obvious.

# Classification: ROC Curve for Model B1 - Use methods from pROC package.
rocRR2 = roc(response=test_data$DONR,predictor=trnProbsRR2)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocRR2,col="blue",
     main=paste("ROC curve for Model B1\nAUC = ",round(rocRR2$auc,digits=3),sep=""))
par(pty="m")

# Classification: Determine "optimal" threshold.
distRR2 = sqrt((rocRR2$specificities-1)^2 + (rocRR2$sensitivities-1)^2)
optIdxRR2 = which.min(distRR2)  # index corresponding to minimum distance
threshRR2 = rocRR2$thresholds[optIdxRR2]  # threshold corresponding to min. distance
#0.04835085
points(rocRR2$specificities[optIdxRR2],rocRR2$sensitivities[optIdxRR2],col="red",pch=7)


# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(trnProbsRR2,test_data$DONR)
TopDecileLift(threshRR2,test_data$DONR)
#0.945

RR2 <- ifelse(trnProbsRR2>threshRR2, 1, 0) # mail to everyone above the cutoff

table(test_data$DONR,RR2)

#RR2
#  0    1
#0 7128 7128
#1  373  537

#TRUE POSITIVE RATE
537/(537+373)
#0.5881922

#FALSE POSITIVE RATE
7128/(7128+7128)
#0.5

#accuracy rate
mean(RR2==test_data$DONR)
#0.5766691

#misClasificError
mean(RR2 != test_data$DONR)
#0.4233309
