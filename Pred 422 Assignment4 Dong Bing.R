#PRED 422 Assignment #4
# Dong Bing


library(ISLR)
library(Hmisc)
library(Corrplot)
library(tree)
library(randomForest)

#Chapter 4.7  
#11.

# a 
describe(Auto)

dim(Auto)

attach(Auto)

mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

fix(Auto_train)

#b

drop_id <- c("name")
Auto1 = Auto[,!(names(Auto) %in% drop_id)]

correlations <- cor(Auto1)
corrplot(correlations, method="square")

#c

testindex <- createDataPartition(y=Auto$mpg01, p=0.25, list=FALSE)    

Auto_test <- Auto[ testindex,]

Auto_train  <- Auto[-testindex,]

#D

# LDA
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto_train)

lda.pred = predict(lda.fit, Auto_test)
mean(lda.pred$class != Auto_test$mpg01)

#0.09183673

#E
# QDA
qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto_train)
qda.pred = predict(qda.fit, Auto_test)
mean(qda.pred$class != Auto_test$mpg01)

#0.08163265

#F

f
# Logistic regression
glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto_train, 
              family = binomial)
glm.probs = predict(glm.fit, Auto_test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != Auto_test$mpg01)

#0.08163265

g
library(class)
train.X = cbind(cylinders, weight, displacement, horsepower)[-testindex, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[testindex, ]
train.mpg01 = mpg01[-testindex]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != Auto_test$mpg01)
#0.1122449

# KNN(k=10)
knn.pred = knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred != Auto_test$mpg01)
#0.1020408

# KNN(k=500)
knn.pred = knn(train.X, test.X, train.mpg01, k = 100)
mean(knn.pred != Auto_test$mpg01)
#0.09183673


#13
#a
library(MASS)
summary(Boston)

attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1
Boston = data.frame(Boston, crime01)

dim(Boston)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]

# logistic regression
glm.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
              subset = train)

## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)
#0.1818182


glm.fit = glm(crime01 ~ . - crime01 - crim - rad - black, data = Boston, family = binomial, 
              subset = train)

glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)
#0.1778656


# LDA
lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)
#0.1343874

lda.fit = lda(crime01 ~ . - crime01 - crim - rad - black, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)
#0.1501976

lda.fit = lda(crime01 ~ . - crime01 - crim - tax - lstat - indus, 
              data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)
#0.1225296

library(class)

train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
               lstat, medv)[test, ]
train.crime01 = crime01[train]

set.seed(2)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crime01, k = 1)
mean(knn.pred != crime01.test)
#0.458498

# KNN(k=10)
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)
#0.1185771

# KNN(k=100)
knn.pred = knn(train.X, test.X, train.crime01, k = 100)
mean(knn.pred != crime01.test)
#0.4940711


#subset variable for knn

train.X = cbind(zn, indus, chas, nox, rm, age, dis,
                lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis,
               lstat, medv)[test, ]
train.crime01 = crime01[train]

set.seed(2)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crime01, k = 1)
mean(knn.pred != crime01.test)
#0.2094862

# KNN(k=10)
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)
#0.229249

# KNN(k=100)
knn.pred = knn(train.X, test.X, train.crime01, k = 100)
mean(knn.pred != crime01.test)
#0.1897233


#Chapter 8.4

#9
#a
attach(Carseats)
set.seed(1)

ind = sample(2,nrow(Carseats), replace = TRUE, prob = c(0.7, 0.3))

Carseats.train = Carseats[ind ==1, ]
Carseats.test = Carseats[ind ==2, ]

#b
library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)
#4.43

#c
cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")


# Best size = 16
pruned.carseats = prune.tree(tree.carseats, best = 9)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)
#4.55

#4.82

#d

bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, 
                            importance = T)

bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)

## [1] 2.537321

importance(bag.carseats)

#e
rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
                           importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)

## [1] 2.4487

importance(rf.carseats)

#10
#a

attach(OJ)
set.seed(2)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

#b

oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

#c
oj.tree

#d

plot(oj.tree)
text(oj.tree, pretty = 0)

#e
oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

pruned = sum(OJ.test$Purchase != oj.pred)
pruned/length(oj.pred)

#f
cv.oj = cv.tree(oj.tree, FUN = prune.tree)

#g
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

#h
#8

#i
oj.pruned = prune.tree(oj.tree, best = 8)

#j
summary(oj.pruned)

#k
pred_unpruned = predict(oj.tree, OJ.test, type = "class")
unpruned = sum(OJ.test$Purchase != pred_unpruned)
unpruned/length(pred.unpruned)

pred_pruned = predict(oj.pruned, OJ.test, type = "class")
pruned = sum(OJ.test$Purchase != pred.pruned)
pruned/length(pred.pruned)



