library(car)
library(dplyr)

pro1.full = read.csv("Pro1_full.csv")
day = (600<time & time<1900)
pro1.day = pro1.full[day,]
pro1.night = pro1.full[!day,]

pro1.day = na.omit(pro1.day)

attach(pro1.day)

lm.fit = lm(pm2.5~pm10+time+no2+o3+co+so2+I(pm10^3))
summary(lm.fit)
vif(lm.fit)

#Resampling methods(10-fold CV)
set.seed(10)
cv.error_k10.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(pm2.5~poly(pm10,i),data=pro1.day)
  cv.error_k10.10[i]=cv.glm(pro1.day,glm.fit,K=10)$delta[1]
}
plot(1:10, cv.error_k10.10, type='b',lty='dotdash',pch=1,
     main='pm10', ylab='MSE', xlab='Order of Polynomial Regression')

for (i in 1:10){
  glm.fit=glm(pm2.5~poly(time,i),data=pro1.day)
  cv.error_k10.10[i]=cv.glm(pro1.day,glm.fit,K=10)$delta[1]
}
plot(1:10, cv.error_k10.10, type='b',lty='dotdash',pch=1,
     main='time', ylab='MSE', xlab='Order of Polynomial Regression')

for (i in 1:10){
  glm.fit=glm(pm2.5~poly(no2,i),data=pro1.day)
  cv.error_k10.10[i]=cv.glm(pro1.day,glm.fit,K=10)$delta[1]
}
plot(1:10, cv.error_k10.10, type='b',lty='dotdash',pch=1,
     main='no2', ylab='MSE', xlab='Order of Polynomial Regression')

for (i in 1:10){
  glm.fit=glm(pm2.5~poly(o3,i),data=pro1.day)
  cv.error_k10.10[i]=cv.glm(pro1.day,glm.fit,K=10)$delta[1]
}
plot(1:10, cv.error_k10.10, type='b',lty='dotdash',pch=1,
     main='o3', ylab='MSE', xlab='Order of Polynomial Regression')

#Model selection and regularization
air1 = subset(pro1.day, pro1.day$district == "관악구")[,-c(1,3)]
air2 = subset(pro1.day, pro1.day$district == "구로구")[,-c(1,3)]
n_pa = length(air2)-1

#Best subset selection
x=model.matrix(pm2.5~.,air2)[,-1]
y=air2$pm2.5
set.seed(7)
train = sample(1:nrow(x),nrow(x)*2/3)
test=(-train)
y.test=y[test]
air2_subset=air2[train,]
attach(air2_subset)

k=5
folds=sample(1:k, nrow(air2_subset),replace=TRUE)
cv.errors = matrix(NA,k,n_pa,dimnames = list(NULL,paste(1:n_pa)))

predict.regsubsets = function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

install.packages("leaps")
library(leaps)

for(j in 1:k){
  best.fit = regsubsets(pm2.5~.,data=air2_subset[folds!=j,],nvmax=n_pa)
  for(i in 1:n_pa){
    pred=predict(best.fit,air2_subset[folds==j,],id=i)
    cv.errors[j,i]=mean((air2_subset$pm2.5[folds==j]-pred)^2)
    }
  }

mean.cv.errors=apply(cv.errors,2,mean)
plot(mean.cv.errors,type='b',xlab="Number of variables",ylab="CV MSE",col="black",lwd=2, main="Best subset selection")

min.index=which.min(mean.cv.errors)
regfit.best=regsubsets(pm2.5~.,data=air2,nvmax=min.index)
coef(regfit.best,min.index)

#Ridge regression & The Lasso
install.packages("glmnet")
library(glmnet)

#Ridge
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh = 1e-12)

cv.out.ridge=cv.glmnet(x[train,],y[train],alpha=0)
bestlam=cv.out.ridge$lambda.min

out.ridge = glmnet(x,y,alpha=0)
predict(out.ridge,type="coefficients",s=bestlam)[1:(n_pa+1),]

#Lasso
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid,thresh = 1e-12)

cv.out.lasso=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out.lasso$lambda.min

out.lasso = glmnet(x,y,alpha=1)
predict(out.lasso,type="coefficients",s=bestlam)[1:(n_pa+1),]

x = cv.out.ridge$lambda
y = cv.out.ridge$cvm
xx = cv.out.lasso$lambda
yy = cv.out.lasso$cvm

plot(x,y,type='b',col='black',xlab=expression(lambda),ylab="CV MSE",lwd=2, main="Ridge")
plot(xx,yy,type='b',col='red',xlab=expression(lambda),ylab="CV MSE",lwd=2, main="Lasso")

#PCR & PLS
install.packages("pls")
library(pls)

x=model.matrix(pm2.5~.,air2)[,-1]
y=air2$pm2.5
set.seed(9)
train = sample(1:nrow(x),nrow(x)*2/3)
test=(-train)
y.test=y[test]

pcr.fit=pcr(air2$pm2.5~.,data=air2,subset=train,scale=TRUE,validation="CV")
pls.fit=plsr(air2$pm2.5~.,data=air2,subset=train,scale=TRUE,validation="CV")

summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP",ylab='CV MSE',,type='b',
               ylim=c(0,500),xlab='Number of Components',col='black',lwd=2, main="")
par(new=TRUE)
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP",ylab='CV MSE',,type='b',
               ylim=c(0,500),xlab='Number of Components',col='red',lwd=2, main="")
legend("topright",legend=c("PCR","PLS"),col=c("black","red"),pch=1,lwd=2)

#Prediction performance using test MSE
lm.fit = lm(air2$pm2.5~.,data=air2,subset=train)
tset_MSE0 = mean((air2$pm2.5-predict(lm.fit,air2))[-train]^2)

pred_test = predict(best.fit,air2[test,],id=min.index)
test_MSE1 = mean((pred_test - y.test)^2)

ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda = grid,thresh=1e-12)
ridge.pred = predict(ridge.mod,s=bestlam,newx=x[test,])
test_MSE2 = mean((ridge.pred-y.test)^2)

lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda = grid,thresh=1e-12)
lasso.pred = predict(lasso.mod,s=bestlam,newx=x[test,])
test_MSE3 = mean((lasso.pred-y.test)^2)

pcr.fit=pcr(air2$pm2.5~.,data=air2,subset=train,scale=TRUE,validation="CV")
pcr.pred = predict(pcr.fit,x[test,],ncomp=5)
test_MSE4 = mean((pcr.pred-y.test)^2)

pls.fit=plsr(air2$pm2.5~.,data=air2,subset=train,scale=TRUE,validation="CV")
pls.pred = predict(pls.fit,x[test,],ncomp=2)
test_MSE5 = mean((pls.pred-y.test)^2)

# n>>p
#Model selection and regularization
air = pro1.day[,-c(1,3)]
n_pa = length(air)-1
attach(air)

#Best subset selection
x=model.matrix(pm2.5~.,air)[,-1]
y=air$pm2.5
set.seed(7)
train = sample(1:nrow(x),nrow(x)*2/3)
test=(-train)
y.test=y[test]
air_subset=air[train,]
attach(air_subset)

k=5
folds=sample(1:k, nrow(air_subset),replace=TRUE)
cv.errors = matrix(NA,k,n_pa,dimnames = list(NULL,paste(1:n_pa)))

predict.regsubsets = function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

install.packages("leaps")
library(leaps)

for(j in 1:k){
  best.fit = regsubsets(pm2.5~.,data=air_subset[folds!=j,],nvmax=n_pa)
  for(i in 1:n_pa){
    pred=predict(best.fit,air_subset[folds==j,],id=i)
    cv.errors[j,i]=mean((air_subset$pm2.5[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
plot(mean.cv.errors,type='b',xlab="Number of variables",ylab="CV MSE",col="black",lwd=2, main="Best subset selection")

min.index=which.min(mean.cv.errors)
regfit.best=regsubsets(pm2.5~.,data=air,nvmax=min.index)
coef(regfit.best,min.index)

#Ridge regression & The Lasso
install.packages("glmnet")
library(glmnet)

#Ridge
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh = 1e-12)

cv.out.ridge=cv.glmnet(x[train,],y[train],alpha=0)
bestlam=cv.out.ridge$lambda.min

out.ridge = glmnet(x,y,alpha=0)
predict(out.ridge,type="coefficients",s=bestlam)[1:(n_pa+1),]

#Lasso
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid,thresh = 1e-12)

cv.out.lasso=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out.lasso$lambda.min

out.lasso = glmnet(x,y,alpha=1)
predict(out.lasso,type="coefficients",s=bestlam)[1:(n_pa+1),]

x = cv.out.ridge$lambda
y = cv.out.ridge$cvm
xx = cv.out.lasso$lambda
yy = cv.out.lasso$cvm

plot(x,y,type='b',col='black',xlab=expression(lambda),ylab="CV MSE",lwd=2, main="Ridge")
plot(xx,yy,type='b',col='red',xlab=expression(lambda),ylab="CV MSE",lwd=2, main="Lasso")

#PCR & PLS
install.packages("pls")
library(pls)

x=model.matrix(pm2.5~.,air)[,-1]
y=air$pm2.5
set.seed(9)
train = sample(1:nrow(x),nrow(x)*2/3)
test=(-train)
y.test=y[test]

pcr.fit=pcr(air$pm2.5~.,data=air,subset=train,scale=TRUE,validation="CV")
pls.fit=plsr(air$pm2.5~.,data=air,subset=train,scale=TRUE,validation="CV")

summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP",ylab='CV MSE',,type='b',
               ylim=c(0,300),xlab='Number of Components',col='black',lwd=2, main="")
par(new=TRUE)
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP",ylab='CV MSE',,type='b',
               ylim=c(0,300),xlab='Number of Components',col='red',lwd=2, main="")
legend("topright",legend=c("PCR","PLS"),col=c("black","red"),pch=1,lwd=2)

#Prediction performance using test MSE
lm.fit = lm(air$pm2.5~.,data=air,subset=train)
tset_MSE0 = mean((air$pm2.5-predict(lm.fit,air))[-train]^2)

pred_test = predict(best.fit,air[test,],id=min.index)
test_MSE1 = mean((pred_test - y.test)^2)

ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda = grid,thresh=1e-12)
ridge.pred = predict(ridge.mod,s=bestlam,newx=x[test,])
test_MSE2 = mean((ridge.pred-y.test)^2)

lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda = grid,thresh=1e-12)
lasso.pred = predict(lasso.mod,s=bestlam,newx=x[test,])
test_MSE3 = mean((lasso.pred-y.test)^2)

pcr.fit=pcr(air$pm2.5~.,data=air,subset=train,scale=TRUE,validation="CV")
pcr.pred = predict(pcr.fit,x[test,],ncomp=5)
test_MSE4 = mean((pcr.pred-y.test)^2)

pls.fit=plsr(air$pm2.5~.,data=air,subset=train,scale=TRUE,validation="CV")
pls.pred = predict(pls.fit,x[test,],ncomp=2)
test_MSE5 = mean((pls.pred-y.test)^2)
