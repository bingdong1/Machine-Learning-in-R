#chapter 10 Exercise 9

#(a)

library(ISLR)

set.seed(4)

hc_c = hclust(dist(USArrests), method="complete")
plot(hc_c)

#(b)

tree <- cutree(hc_c, 3)

table(tree)

write.csv(tree, file = "tree.csv", row.names=FALSE)

#(C)

sUSArrests = scale(USArrests)
shc_c = hclust(dist(sUSArrests), method="complete")
plot(shc_c)

#(D)

cutree(shc_c, 3)

table(cutree(shc_c, 3))


#chapter 10 Exercise 10

#(a)

set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)

#(b)

pca.out = prcomp(x)
summary(pca.out)
pca.out$x[,1:2]
plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 


x[1:20, 2] = 3
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 3


#(c)

km.out3 = kmeans(x, 3, nstart=20)
km.out3$cluster
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#(D)

km.out2 = kmeans(x, 2, nstart=20)
km.out2$cluster

#(e)

km.out4= kmeans(x, 4, nstart=20)
km.out4$cluster

#(f)

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#(g)

km.out_Scale = kmeans(scale(x), 3, nstart=20)
km.out_Scale$cluster

#chapter 3 Exercise 9

#(a)
Auto = na.omit(Auto)
summary(Auto)

pairs(Auto)

#(b)
cor(subset(Auto, select=-name))

#(c)
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)

#(d)
par(mfrow=c(2,2))
plot(lm.fit1)

plot(predict(lm.fit1), rstudent(lm.fit1))

#(e)
lm.fit2 = lm(mpg~cylinders*weight+year*origin, data=Auto)
summary(lm.fit2)

#(f)
lm.fit3 = lm(log(mpg)~cylinders*weight+year*origin, data=Auto)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

lm.fit4 = lm(mpg^2~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=Auto)
summary(lm.fit4)
par(mfrow=c(2,2))
plot(lm.fit4)

#chapter 3 Exercise 15

#(a)
library(MASS)
summary(Boston)
fix(Boston)
names(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)

attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes

lm.indus = lm(crim~indus)
summary(lm.indus) # yes

lm.chas = lm(crim~chas)
summary(lm.chas) # no
par(mfrow=c(2,2))
plot(lm.chas)


lm.nox = lm(crim~nox)
summary(lm.nox) # yes

lm.rm = lm(crim~rm)
summary(lm.rm) # yes
par(mfrow=c(2,2))
plot(lm.rm)


lm.age = lm(crim~age)
summary(lm.age) # yes

lm.dis = lm(crim~dis)
summary(lm.dis) # yes

lm.rad = lm(crim~rad)
summary(lm.rad) # yes

lm.tax = lm(crim~tax)
summary(lm.tax) # yes

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes

lm.black = lm(crim~black)
summary(lm.black) # yes

lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes

lm.medv = lm(crim~medv)
summary(lm.medv) # yes

#(b)
lm.all = lm(crim~., data=Boston)
summary(lm.all)
par(mfrow=c(2,2))
plot(lm.all )

#(c)
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)

#(d)
lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1,2,3

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # # 1,2,3

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1,2

lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1,2,3

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1,2,3

lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1,2

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1,2

lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1,2,3

lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1

lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) #1,2,3
