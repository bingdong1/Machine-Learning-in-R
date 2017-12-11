
## PREDICT 422
## Charity Project - Part 1 (Exploratory Data Analysis)
########################################

########################################
## Exercise 1
## Read Data from CSV File
########################################

charityData = read.csv("trainSample.csv", na.strings=c("NA"," "))

# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
charityData$DONR = as.factor(charityData$DONR)
charityData$HOME = as.factor(charityData$HOME)
charityData$HINC = as.factor(charityData$HINC)

########################################
## Exercise 2
## Data Quality Check
########################################

dim(charityData)      # dimensions of data
names(charityData)    # variable names
str(charityData)      # one form of summary of data
summary(charityData)  # another form of summary

## Check for Missing Values
# HOME, HINC and GENDER have missing values

# Missing values identified in HINC, GENDER, and HOME
# Get counts of missing values for each variable
table(charityData$HOME,useNA="ifany")
table(charityData$HINC,useNA="ifany")
table(charityData$GENDER,useNA="ifany")

# Any data anomalies or variables that might cause issues in a statistical analysis?
table(charityData$GENDER) # data dictionary does not have decodes for A and C genders
# The unknown gender categorical option 
# replace the A and C genders to unkown
charityData$GENDER <- replace(charityData$GENDER, charityData$GENDER == 'A', 'U')
charityData$GENDER <- replace(charityData$GENDER, charityData$GENDER == 'C', 'U')

########################################
## Exercise 3
## Exploratory Data Analysis
########################################

## Part A - General EDA

# Histogram of the response variable DAMT
# (first with 0s included, second with 0s dropped)
hist(charityData$DAMT,col="blue",breaks=20,xlab="DAMT",main="")
hist(charityData$DAMT[charityData$DAMT > 0],col="blue",breaks=20,xlab="DAMT > 0",main="DAMT observations > $0 in donations")
hist(log(charityData$DAMT[charityData$DAMT > 0]),col="blue",breaks=20,xlab="DAMT > 0",main="log(DAMT) observations > $0 in donations")

# Counts of the response variable DONR
table(charityData$DONR)
table(charityData$DONR) / length(charityData$DONR)
barplot(table(charityData$DONR), xlab="DONR")

# distribution of the predictor variables

par(mfrow = c(2,2)) # play with breaks and update title
hist(charityData$MEDINC, breaks = 20, main = "Median Neighboorhood Income")
hist(charityData$MEDAGE, breaks = 20, main = "Median Neighboorhood Age")
hist(charityData$MEDPPH, breaks = 20, main = "Median Neighboorhood Household Size")
hist(charityData$MEDHVAL, breaks = 20, main = "Median Neighboorhood Home Value")
par(mfrow = c(2,2))
hist(charityData$MEDEDUC, breaks = 20, main = "Median Neighboorhood Education")
hist(charityData$AGE, breaks = 20, main = "Age Distribution")
hist(charityData$NUMPROM, breaks = 20, main = "# of Promotions")
hist(charityData$NUMPRM12, breaks = 20, main = "# of Promotions Last 12 Mo.")
par(mfrow = c(2,2))
#hist(charityData$RAMNTALL, breaks = 20, main = "Dollar Amount of Liftime Gifts")
hist(log(charityData$RAMNTALL[charityData$RAMNTALL > 0]), main = "Dollar Amount of Last Gift")
#hist(charityData$NGIFTALL, breaks = 20, main = "# of Lifttime Gifts")
hist(log(charityData$NGIFTALL[charityData$NGIFTALL > 0]),  main = "Dollar Amount of Last Gift")
#hist(charityData$LASTGIFT, breaks = 20, main = "Dollar Amount of Last Gift")
hist(log(charityData$LASTGIFT[charityData$LASTGIFT > 0]),  main = "Dollar Amount of Last Gift")
hist(charityData$TDON, breaks = 20, main = "# of Months Since Last Gift")
par(mfrow = c(1,1)) 

qualitative_vars <- c("HOME","HINC", "GENDER", "RFA_96", "RFA_96_R", "RFA_96_F", "RFA_96_A", "ID", "DONR", "DAMT")
charityData_quant <- charityData[ , !(names(charityData) %in% qualitative_vars)]

## Part B - Regression Problem EDA
# subset the data for DONR = 1 ~ DAMT > 0
charityData_donors <- charityData[charityData$DAMT > 0.0,]

# Boxplot of DAMT amount by categories for GENDER
plot(charityData_donors$GENDER, charityData_donors$DAMT, xlab="Gender", ylab="Donation ($)")
plot(charityData_donors$HOME, charityData_donors$DAMT, xlab="HOME", ylab="Donation ($)")
plot(charityData_donors$HINC, charityData_donors$DAMT, xlab="HINC", ylab="Donation ($)")
plot(charityData_donors$RFA_96, charityData_donors$DAMT, xlab="RFA_96", ylab="Donation ($)")

# Plot DAMT against a quantitative predictor variable
indiv_plots_single <- function(x, y, ...) {
        fit = lm(y ~ x)
        plot(x, y, ...)
        abline(fit, col = 'red')
        
        return(fit)
}

fit_LASTGIFT <- indiv_plots_single(charityData_donors$LASTGIFT, charityData_donors$DAMT, xlab = 'LASTGIFT', ylab = 'DAMT')
cor(charityData_donors$LASTGIFT, charityData_donors$DAMT)

fit_AGE <- indiv_plots_single(charityData_donors$AGE, charityData_donors$DAMT, xlab = 'AGE', ylab = 'DAMT')
summary(fit_AGE)

fit_MEDAGE <- indiv_plots_single(charityData_donors$MEDAGE, charityData_donors$DAMT, xlab = 'MEDAGE', ylab = 'DAMT')
fit_MEDPPH <- indiv_plots_single(charityData_donors$MEDPPH, charityData_donors$DAMT, xlab = 'MEDPPH', ylab = 'DAMT')
fit_MEDHVAL <- indiv_plots_single(charityData_donors$MEDHVAL, charityData_donors$DAMT, xlab = 'MEDHVAL', ylab = 'DAMT')
fit_MEDINC <- indiv_plots_single(charityData_donors$MEDINC, charityData_donors$DAMT, xlab = 'MEDINC', ylab = 'DAMT')
fit_MEDEDUC <- indiv_plots_single(charityData_donors$MEDEDUC, charityData_donors$DAMT, xlab = 'MEDEDUC', ylab = 'DAMT')
fit_NUMPROM <- indiv_plots_single(charityData_donors$NUMPROM, charityData_donors$DAMT, xlab = 'NUMPROM', ylab = 'DAMT')
fit_NUMPRM12 <- indiv_plots_single(charityData_donors$NUMPRM12, charityData_donors$DAMT, xlab = 'NUMPRM12', ylab = 'DAMT')
fit_RAMNTALL <- indiv_plots_single(charityData_donors$RAMNTALL, charityData_donors$DAMT, xlab = 'RAMNTALL', ylab = 'DAMT')
fit_NGIFTALL <- indiv_plots_single(charityData_donors$NGIFTALL, charityData_donors$DAMT, xlab = 'NGIFTALL', ylab = 'DAMT')
fit_MAXRAMNT <- indiv_plots_single(charityData_donors$MAXRAMNT, charityData_donors$DAMT, xlab = 'MAXRAMNT', ylab = 'DAMT')
fit_LASTGIFT <- indiv_plots_single(charityData_donors$LASTGIFT, charityData_donors$DAMT, xlab = 'LASTGIFT', ylab = 'DAMT')
fit_TDON <- indiv_plots_single(charityData_donors$TDON, charityData_donors$DAMT, xlab = 'TDON', ylab = 'DAMT')


## Part C - Classification Problem EDA

# Boxplot of AGE by DONR status
# In order for R to make this into a boxplot, DONR needs to be a factor variable
# and DONR needs to be plotted on the horizontal axis.

# Plot DONR against a quantitative predictor variable
indiv_plots_boxplot <- function(x, y, ...) {
        
        plot(y, x, ...)
}

indiv_plots_boxplot(charityData$AGE, charityData$DONR, xlab = 'DONR', ylab = 'AGE')
indiv_plots_boxplot(charityData$MEDAGE, charityData$DONR, xlab = 'DONR', ylab = 'MEDAGE')
indiv_plots_boxplot(charityData$MEDPPH, charityData$DONR, xlab = 'DONR', ylab = 'MEDPPH')
indiv_plots_boxplot(charityData$MEDHVAL, charityData$DONR, xlab = 'DONR', ylab = 'MEDHVAL')
indiv_plots_boxplot(charityData$MEDINC, charityData$DONR, xlab = 'DONR', ylab = 'MEDINC')
indiv_plots_boxplot(charityData$MEDEDUC, charityData$DONR, xlab = 'DONR', ylab = 'MEDEDUC')
indiv_plots_boxplot(charityData$NUMPROM, charityData$DONR, xlab = 'DONR', ylab = 'NUMPROM')
indiv_plots_boxplot(charityData$NUMPRM12, charityData$DONR, xlab = 'DONR', ylab = 'NUMPRM12')
indiv_plots_boxplot(charityData$RAMNTALL, charityData$DONR, xlab = 'DONR', ylab = 'RAMNTALL')
indiv_plots_boxplot(log(charityData$NGIFTALL), charityData$DONR, xlab = 'DONR', ylab = 'NGIFTALL')
indiv_plots_boxplot(log(charityData$MAXRAMNT), charityData$DONR, xlab = 'DONR', ylab = 'log(MAXRAMNT)')
indiv_plots_boxplot(log(charityData$LASTGIFT), charityData$DONR, xlab = 'DONR', ylab = 'LASTGIFT')
indiv_plots_boxplot(charityData$TDON, charityData$DONR, xlab = 'DONR', ylab = 'TDON')

# Categorical Variables ~ "Right" Way
# A mosaic plot is obtained when we plot one factor variable against another. The
# mosaic plot represents the counts as proportions to the whole. A deviation in
# overall proportion of females donating compared to males donating is meaningful
# whereas the absolute count of females donating compared to males donating was not.
plot(charityData$GENDER,charityData$DONR,xlab="GENDER",ylab="DONR",main="Mosaic Plot")
# These graphs show that M/F doesn't show any difference in DONR status.
plot(charityData$HOME,charityData$DONR,xlab="HOME",ylab="DONR",main="Mosaic Plot")
plot(charityData$HINC,charityData$DONR,xlab="HINC",ylab="DONR",main="Mosaic Plot")

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
charityData = separateRFA(charityData,"RFA_96")

# Check the results
table(charityData$RFA_96,charityData$RFA_96_R)
table(charityData$RFA_96,charityData$RFA_96_F)
table(charityData$RFA_96,charityData$RFA_96_A)

########################################
## Exercise 4
## Principal Component Analysis
########################################

## Part A - Apply PCA
# PCA only works with quantitative variables, I also removed the response variables and ID variable
# we do not want to skew the PCA results by leaving in the response variables
qualitative_vars <- c("HOME","HINC", "GENDER", "RFA_96", "RFA_96_R", "RFA_96_F", "RFA_96_A", "ID", "DONR", "DAMT")
charityData_quant <- charityData[ , !(names(charityData) %in% qualitative_vars)]
#charityData_quant <- na.omit(charityData_quant)
apply(charityData_quant, 2, mean)
apply(charityData_quant, 2, sd)

pca.out <- prcomp(charityData_quant, scale = TRUE)
pca.out

charityData_results <- charityData
charityData_results$PC1 <- pca.out$x[,1]
charityData_results$PC2 <- pca.out$x[,2]
plot(charityData_results$PC1, charityData_results$PC2, col = charityData_results$DONR, xlab = "PC1", ylab = "PC2", pch = 19)
# work on adding; 

## Part B - Generate and show a scree plot
pr.var <- pca.out$sdev^2
pve <- pr.var / sum(pr.var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')


## Part BC - Generate and include a bigplot
biplot(pca.out, scale = 0, cex = 0.6) #, display = c("AGE", "MEDAGE"))
# remove a few of the variables to improve legibility 
