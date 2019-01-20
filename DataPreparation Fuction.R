########################################
## PREDICT 422
## Charity Project - Part 4 (The Mailing List Problem)
##
## DataPreparation.R
########################################

# Separate RFA Values (R = recency, F = frequency, A = amount)
# Note: The same separateRFA function can be called by the functions processPart1
# and processPart2.
processPart = function(myData) {
        
        # Convert categorical variables to factors
        myData$HOME = as.factor(myData$HOME)
        myData$HINC = as.factor(myData$HINC)
        
        # removed the observations that have neighborhood values of zero
        myData <- myData[myData$MEDAGE > 0, ]
        
        # GENDER - Assign A, J, and NA to category U
        idxMF = myData$GENDER %in% c("M","F")
        myData$GENDER[!idxMF] = "J"
        myData$GENDER = factor(myData$GENDER)
        
        # HOME - Make a level 0 and code missing values as 0
        levels(myData$HOME) = c(levels(myData$HOME),"0")
        myData$HOME[is.na(myData$HOME)] = "0"

        # HINC - Make a level 0 and code missing values as 0
        levels(myData$HINC) = c(levels(myData$HINC),"0")
        myData$HINC[is.na(myData$HINC)] = "0"

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
        myData = separateRFA(myData,"RFA_96")
        levels(myData$RFA_96_A) = c(levels(myData$RFA_96_A), "B")
        
        return(myData)
        
}
