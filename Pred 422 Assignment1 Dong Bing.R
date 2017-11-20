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

