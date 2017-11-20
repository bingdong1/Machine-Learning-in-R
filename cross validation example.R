moneyball <- read.table("U:/Midterm/moneyball.csv", header=TRUE, sep=',')

sum(is.na(moneyball))

moneyball=na.omit(moneyball)

library(leaps)
regfit.full=regsubsets(TARGET_WINS~.,moneyball)
summary(regfit.full)

regfit.full=regsubsets(TARGET_WINS~.,data=moneyball,nvmax=17)
reg.summary=summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")


regfit.fwd=regsubsets(TARGET_WINS~.,data=moneyball,nvmax=17,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(TARGET_WINS~.,data=moneyball,nvmax=17,method="backward")
summary(regfit.bwd)


# Choosing Among Models

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(moneyball),rep=TRUE)


fix(train)

test=(!train)
fix(test)

regfit.best=regsubsets(TARGET_WINS~.,data=moneyball[train,],nvmax=17)

test.mat=model.matrix(TARGET_WINS~.,data=moneyball[test,])

val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((moneyball$TARGET_WINS[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,9)


predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(TARGET_WINS~.,data=moneyball,nvmax=17)
coef(regfit.best,10)

k=500
set.seed(1)
folds=sample(1:k,nrow(moneyball),replace=TRUE)
cv.errors=matrix(NA,k,16, dimnames=list(NULL, paste(1:16)))
for(j in 1:k){
  best.fit=regsubsets(TARGET_WINS~.,data=moneyball[folds!=j,],nvmax=16)
  for(i in 1:16){
    pred=predict(best.fit,moneyball[folds==j,],id=i)
    cv.errors[j,i]=mean( (moneyball$TARGET_WINS[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors


par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)
