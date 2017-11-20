########################################
## PREDICT 422
## Charity Project - Part 1 
## Dong Bing
########################################

########################################
## Exercise 1
## Read Data from CSV File
########################################

library(mice)
library(VIM)
library(Hmisc)
#library(corrgram)
#library(Boruta)
library(data.table)
library(testthat)
library(gridExtra)
library(Corrplot)


#Read in Data
charityData <- read.csv(file="U:/Midterm/trainSample.csv", header=TRUE, sep=",", na.strings=NA)

#Data Review
fix(charityData)

# Convert categorical variables to factors

charityData$DONR = as.factor(charityData$DONR)
charityData$HOME = as.factor(charityData$HOME)
charityData$HINC = as.factor(charityData$HINC)

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
charityData = separateRFA(charityData,"RFA_96")
fix(charityData)

# Check the results
table(charityData$RFA_96,charityData$RFA_96_R)
table(charityData$RFA_96,charityData$RFA_96_F)
table(charityData$RFA_96,charityData$RFA_96_A)


#Change factors to binary dummy variables

levels(charityData$GENDER)

charityData$GENDER1 <- ifelse(charityData$GENDER == 'A', 1,
                                  ifelse(charityData$GENDER == "C", 2,
                                         ifelse(charityData$GENDER == 'F', 3, 
                                                ifelse(charityData$GENDER == 'J', 4, 
                                                       ifelse(charityData$GENDER == 'M', 5,
                                                              ifelse(charityData$GENDER == 'U', 6, NA))))))


levels(charityData$RFA_96_R)

charityData$RFA_96_R1 <- ifelse(charityData$RFA_96_R == 'A', 1,
                              ifelse(charityData$RFA_96_R == "F", 2,
                                     ifelse(charityData$RFA_96_R == 'L', 3, 
                                            ifelse(charityData$RFA_96_R == 'N', 4, 
                                                   ifelse(charityData$RFA_96_R == 'S', 5, NA)))))

levels(charityData$RFA_96_A)

charityData$RFA_96_A1 <- ifelse(charityData$RFA_96_A == 'B', 1,
                                ifelse(charityData$RFA_96_A == "C", 2,
                                       ifelse(charityData$RFA_96_A == 'D', 3, 
                                              ifelse(charityData$RFA_96_A == 'E', 4, 
                                                     ifelse(charityData$RFA_96_A == 'F', 5,
                                                          ifelse(charityData$RFA_96_A == 'G', 6, NA))))))

#Change variable to factors
charityData$GENDER1 = as.factor(charityData$GENDER1)
charityData$RFA_96_R1 = as.factor(charityData$RFA_96_R1)
charityData$RFA_96_A1 = as.factor(charityData$RFA_96_A1)
charityData$RFA_96_F = as.numeric(charityData$RFA_96_F)


#Drop no longer needed variables
drop <- c('GENDER', 'RFA_96', 'RFA_96_R', 'RFA_96_A') 

charityData = charityData[,!(names(charityData) %in% drop)]


########################################
## Exercise 2
## Data Quality Check
########################################

nrow(charityData)     #70871
dim(charityData)      # dimensions of data 70871,22
names(charityData)    # variable names
str(charityData)      # one form of summary of data
summary(charityData)  # another form of summary
head(charityData)
class(charityData)

describe(charityData)

#response rate
response_pct <- round(100 * prop.table(table(charityData$DONR)),digits = 1)

#pie chart for response rate
mylabels <- paste("Response Rate", names(response_pct), "\n",response_pct, "%",sep="") 
pie(response_pct, labels = mylabels)

#Create subset for positive response

charityDataPos<- charityData[charityData$DAMT > 0, ]
summary(charityDataPos$DONR)

#Create bar chart for donor amount
donor_amt <- (charityDataPos$DAMT)
donor_amt <- round(donor_amt)
barplot(table(donor_amt), las=2)


## Check for Missing Values

#Graphical illustration of NA

missmap(charityData, main="Missing Map")

md.pattern(charityData)

mice_plot <- aggr(charityData, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(charityData), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

#find column with NA
which(sapply(charityData,anyNA))

colSums(sapply(charityData, is.na))

#impute missing value

imp.charityData <- mice(charityData, m = 1, method='cart')

miceOutput <- complete(imp.charityData) 

#Recheck
colSums(sapply(miceOutput, is.na))

# Missing values identified in HOME, GENDER, and HINC
# Get counts of missing values for each variable
table(charityData$HOME,useNA="ifany")
table(charityData$HINC,useNA="ifany")
table(charityData$GENDER,useNA="ifany")

#Create list for numeric and factor variables. 
cat_var <- names(charityData)[which(sapply(charityData, is.factor))]
numeric_var <- names(charityData)[which(sapply(charityData, is.numeric))]

#Summarize variable
summary(charityData[,numeric_var])
summary(charityData[,cat_var])

########################################
## Exercise 3
## Exploratory Data Analysis
########################################

## Part A - General EDA

# Histogram of the response variable DAMT

# (first with 0s included, second with 0s dropped)
hist(charityData$DAMT,col="blue",breaks=20,xlab="DAMT",main="")
hist(charityData$DAMT[charityData$DAMT > 0],col="blue",breaks=20,xlab="DAMT > 0",main="")

#determine numeric variables
num.mumber <- which(sapply(charityData, is.numeric))

#Set layout format
layout(matrix(c(1,2),1,2))

#create function for histogram
myHist <- function(x) {
  hist(charityData[,x], main = NULL, xlab = x)
}

#Create histogram for all variables
sapply(names(num.mumber),myHist)

# Counts of the response variable DONR
table(charityData$DONR)
barplot(table(charityData$DONR),xlab="DONR")

## Part B - Regression Problem EDA

#create dataset for those donate

charityDataPos<- charityData[charityData$DAMT > 0, ]
summary(charityDataPos$DONR)

# correlations plot, 

drop_id <- c("ID", 'DONR','HOME','HINC','GENDER1','RFA_96_F','RFA_96_R1','RFA_96_A1')
charityDataPos2 = charityDataPos[,!(names(charityDataPos) %in% drop_id)]

correlations <- cor(charityDataPos2)
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

#Boruta Selection
# model cols
#drop_id <- c("ID", 'DAMT')
#boruta_features = charityData[,!(names(charityData) %in% drop_id)]
#bor.results <- Boruta(boruta_features, charityData$DONR,
#                      maxRuns=10)
#plot(bor.results)
#names(bor.results)
#bor.results$finalDecision
#CONFIRMED_ATTR <- getSelectedAttributes(bor.results)
#CONFIRMED_ATTR_fix <- getSelectedAttributes(TentativeRoughFix(bor.results))
# Boruta_output <- attStats(bor.results)

# Boxplot of DAMT amount by categories for GENDER
plot(charityDataPos$GENDER1,charityDataPos$DAMT,xlab="Gender",ylab="Donation ($)")

plot(charityDataPos$HOME,charityDataPos$DAMT,xlab="HOME",ylab="Donation ($)")

plot(charityDataPos$HINC,charityDataPos$DAMT,xlab="HINC",ylab="Donation ($)")

# Plot DAMT against a quantitative predictor variable Age
plot(charityDataPos$AGE,charityDataPos$DAMT,xlab="Age",ylab="Donation ($)")
lm_age = lm(DAMT ~ AGE, data=charityDataPos)
abline(lm_age,col="red")

# Create bins for boxplot to exam the same relationship
AGE2 <- cut(charityDataPos$AGE, right = F, breaks=seq(0,100, by=5))

boxplot(charityDataPos$DAMT ~ AGE2, ylim=c(0,40), las=3)

# Plot DAMT against a quantitative predictor variable LASTGIFT
plot(charityDataPos$LASTGIFT,charityDataPos$DAMT,xlab="LastGift",ylab="Donation ($)")
lm_LASTGIFT = lm(DAMT ~ LASTGIFT, data=charityDataPos)
abline(lm_LASTGIFT,col="red")

# Plot DAMT against a quantitative predictor variable MAXRAMNT
plot(charityDataPos$MAXRAMNT,charityDataPos$DAMT,xlab="RAMNTALL",ylab="Donation ($)")
lm_MAXRAMNT = lm(DAMT ~ MAXRAMNT, data=charityDataPos)
abline(lm_MAXRAMNT,col="red")

# Plot DAMT against a quantitative predictor variable NGIFTALL
plot(charityDataPos$NGIFTALL,charityDataPos$DAMT,xlab="NGIFTALL",ylab="Donation ($)")
lm_NGIFTALL = lm(DAMT ~ NGIFTALL, data=charityDataPos)
abline(lm_NGIFTALL,col="red")

## Part C - Classification Problem EDA

# Boxplot of AGE by DONR status
plot(charityData$DONR,charityData$AGE,xlab="DONR",ylab="AGE")

plot(charityData$DONR,charityData$HOME,xlab="DONR",ylab="HOME")

plot(charityData$DONR,charityData$HINC,xlab="DONR",ylab="HINC")

plot(charityData$DONR,charityData$MEDAGE,xlab="DONR",ylab="MEDAGE")


barplot(table(charityData$GENDER[charityData$DONR == 1]),
        xlab="GENDER",main="Barplot of GENDER for DONR = 1")


barplot(table(charityData$GENDER[charityData$DONR == 0]),
        xlab="GENDER",main="Barplot of GENDER for DONR = 0")


plot(charityData$DONR,charityData$GENDER,xlab="DONR",ylab="GENDER",main="Mosaic Plot")

plot(charityData$GENDER,charityData$DONR,xlab="GENDER",ylab="DONR",main="Mosaic Plot")



#Chi-Test Association for factor variables

myChiTest <- function(x) {

  t1 <- table(charityData[,x],charityData$DONR)  
  
  plot(t1, main=x, las=1)
  
  print(x)
  
  print(chisq.test(t1))
  
  }

myChiTest("RFA_96_F")

sapply(cat_var, myChiTest)


########################################
## Exercise 4
## Principal Component Analysis
########################################

# See ISLR Section 10.4 for guidance on performing PCA in R.

#load library
library(dummies)

#Drop no longer needed variables
drop <- c('ID', 'DONR', 'DAMT','HOME','HINC','GENDER1','RFA_96_F','RFA_96_R1','RFA_96_A1') 

charityDataPCA = charityData[,!(names(charityData) %in% drop)]

#create a dummy data frame
#new_charityDataPCA <- dummy.data.frame(charityDataPCA, names = c("HOME","HINC",
#                                                     "GENDER1","RFA_96_R1",
#                                                     "RFA_96_A1","RFA_96_F"))

#fix(charityDataPCA)
#check the data set
#str(charityDataPCA)

#principal component analysis
prin_comp <- prcomp(charityDataPCA, scale. = T)

names(prin_comp)

summary(prin_comp)

prin_comp$x[,1:2]
plot(prin_comp$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

biplot(prin_comp, scale = 0)

biplot(prin_comp, choices = 1:2, scale = 1, pc.biplot = FALSE)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
 prop_varex <- pr_var/sum(pr_var)

 #scree plot
plot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")







