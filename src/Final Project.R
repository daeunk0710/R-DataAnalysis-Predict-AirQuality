install.packages('car')
install.packages('dplyr')
install.packages('tidyr')
library(car)
library(dplyr)
library(tidyr)

#Data Input
raw_201801 = read.csv("2018_1.csv")
raw_201802 = read.csv("2018_2.csv")
raw_201803 = read.csv("2018_3.csv")
raw_201804 = read.csv("2018_4.csv")
summary(raw_201801)
summary(raw_201802)
names(raw_201801)
Data_2018_raw = bind_rows(raw_201801,raw_201802,raw_201803,raw_201804)
Data_2018 = Data_2018_raw[4:10]
attach(Data_2018)

# 도시대기 측정망: 서울 중구
Data_111121 = subset(Data_2018_raw, 측정소코드 == 111121)
summary(Data_111121)
length(which(Data_111121$PM10 >=151))/length(Data_111121$PM10)
length(which(Data_111121$PM25 >=76))/length(Data_111121$PM25)

# 도로변대기 측정망: 서울 강남대로
Data_111264 = subset(Data_2018_raw, 측정소코드 == 111264)
summary(Data_111264)
length(which(Data_111264$PM10 >=151))/length(Data_111264$PM10)
length(which(Data_111264$PM25 >=76))/length(Data_111264$PM25)

# 국가배경농도 측정망: 제주 고산리, 인천 백령도 
Data_339312 = subset(Data_2018_raw, 측정소코드 == 339312)
Data_831492 = subset(Data_2018_raw, 측정소코드 == 831492)
summary(Data_831492)
length(which(Data_831492$PM10 >=151))/length(Data_831492$PM10)
length(which(Data_831492$PM25 >=76))/length(Data_831492$PM25)

# 교외대기 측정망: 경기 파주
Data_131373 = subset(Data_2018_raw, 측정소코드 == 131373)
summary(Data_131373)
length(which(Data_131373$PM10 >=151))/length(Data_131373$PM10)
length(which(Data_131373$PM25 >=76))/length(Data_131373$PM25)

plot(Data_111121$PM10,type="l", col = 1,
     main = "2018년 시간별 미세먼지 농도 추이",
     ylab='PM10 농도 (mcg/m^3)', xlab="시간")
lines(Data_111264$PM10,col=2)
lines(Data_831492$PM10,col=3)
lines(Data_131373$PM10,col=4)
legend("topright", c("도시대기 측정망","도로변대기 측정망","국가배경농도 측정망","교외대기 측정망"), pch=1,col=c(1:4))

plot(Data_111121$PM25,type="l", col = 1,
     main = "2018년 시간별 초미세먼지 농도 추이",
     ylab='PM25 농도 (mcg/m^3)', xlab="시간")
lines(Data_111264$PM25,col=2)
lines(Data_831492$PM25,col=3)
lines(Data_131373$PM25,col=4)
legend("topright", c("도시대기 측정망","도로변대기 측정망","국가배경농도 측정망", "교외대기 측정망(자료없음)"), pch=1,col=c(1:4))

#Data Cleansing
Data_2018$'PM10+1' <- NA
Data_2018$'PM25+1' <- NA
Data_2018$'PM10+4' <- NA
Data_2018$'PM25+4' <- NA

for(i in 1:3271733){
Data_2018[i, 'PM10+1']= Data_2018[i+24,'PM10']
Data_2018[i, 'PM25+1']= Data_2018[i+24,'PM25']
Data_2018[i, 'PM10+4']= Data_2018[i+4,'PM10']
Data_2018[i, 'PM25+4']= Data_2018[i+4,'PM25']
if(i%%10000==0) print(i)
}

Data_2018 = tibble::rownames_to_column(Data_2018,"index")
Data_2018 = Data_2018[,-2]
Data_2018_raw = tibble::rownames_to_column(Data_2018_raw,"index")
Data_2018_reg = Data_2018_raw[,1:3]
Data_2018 = merge(Data_2018,Data_2018_reg,by="index")
Data_2018 = separate(Data_2018,지역,c("reg1","reg2")," ")
Data_2018_cleansed = na.omit(Data_2018)
count(Data_2018_cleansed$`PM10+4`)

set.seed(5)
train10 = sample(1:nrow(Data_2018_cleansed),nrow(PM10.lev)/4)
train25 = sample(1:nrow(PM25.lev),nrow(PM25.lev)/4)
PM10.lev.test = PM10.lev[-train10,]
PM25.lev.test = PM25.lev[-train25,]      
attach(Data_2018_cleansed)

`PM10+4`#Linear Regression
lm.fit1 = lm(Data_2018_cleansed$'PM10+1'~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed)
summary(lm.fit1)
lm.fit2 = lm(Data_2018_cleansed$'PM25+1'~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed)
summary(lm.fit2)
lm.fit3 = lm(Data_2018_cleansed$'PM10+4'~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed)
summary(lm.fit3)
lm.fit4 = lm(Data_2018_cleansed$'PM25+4'~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed)
summary(lm.fit4)

#Collinearity
vif(lm.fit1)
vif(lm.fit2)
vif(lm.fit3)
vif(lm.fit4)

#Interaction
lm.fit31 = update(lm.fit3, ~.+SO2*PM10)
summary(lm.fit31)
lm.fit32 = update(lm.fit3, ~.+CO*PM10)
summary(lm.fit32)
lm.fit33 = update(lm.fit3, ~.+NO2*PM10)
summary(lm.fit33)
lm.fit34 = update(lm.fit3, ~.+PM25*PM10)
summary(lm.fit34)

lm.fit41 = update(lm.fit4, ~.+SO2*PM25)
summary(lm.fit41)
lm.fit42 = update(lm.fit4, ~.+CO*PM25)
summary(lm.fit42)
lm.fit43 = update(lm.fit4, ~.+NO2*PM25)
summary(lm.fit43)
lm.fit44 = update(lm.fit4, ~.+PM10*PM25)
summary(lm.fit44)

#Non-linear relationship
lm.fit35 = lm(Data_2018_cleansed$'PM10+4'~PM10)
summary(lm.fit35)
lm.fit36 = lm(Data_2018_cleansed$'PM10+4'~poly(PM10,2,raw=TRUE))
summary(lm.fit36)
lm.fit37 = lm(Data_2018_cleansed$'PM10+4'~poly(PM10,3,raw=TRUE))
summary(lm.fit37)
lm.fit38 = lm(Data_2018_cleansed$'PM10+4'~poly(PM10,5,raw=TRUE))
summary(lm.fit38)

lm.fit45 = lm(Data_2018_cleansed$'PM25+4'~PM25)
summary(lm.fit45)
lm.fit46 = lm(Data_2018_cleansed$'PM25+4'~poly(PM25,2,raw=TRUE))
summary(lm.fit46)
lm.fit47 = lm(Data_2018_cleansed$'PM25+4'~poly(PM25,3,raw=TRUE))
summary(lm.fit47)
lm.fit48 = lm(Data_2018_cleansed$'PM25+4'~poly(PM25,5,raw=TRUE))
summary(lm.fit48)

#Resampling Methods
#LOOCV
install.packages('boot')
library(boot)
glm.fit1 = glm(Data_2018_cleansed$'PM10+4'~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed)
cv.err1 = cv.glm(Data_2018_cleansed, glm.fit1)
cv.err1$delta

#k-Fold Cross Validation
set.seed(10)
cv.err1_k10.10=rep(0,10)
cv.err2_k10.10=rep(0,10)
glm.fit1 = glm(Data_2018_cleansed$'PM10+4'~SO2+CO+O3+NO2+PM10+PM25)
glm.fit2 = glm(Data_2018_cleansed$'PM25+4'~SO2+CO+O3+NO2+PM10+PM25)

for(i in 1:10){
  cv.err1_k10.10[i]=cv.glm(Data_2018_cleansed, glm.fit1, K=10)$delta[1]
  cv.err2_k10.10[i]=cv.glm(Data_2018_cleansed, glm.fit2, K=10)$delta[1]
  print(i)
  }



plot(1:10,cv.err1_k10.10,type='b',lty='dotdash',ylim=c(0,1.0e-20),
     xlab="K", ylab="MSE",pch=4,col='red')
par(new=T)
lines(1:10,cv.err2_k10.10,type='b',pch=4,col='blue')
par(new=T)
lines(1:10,cv.err3_k10.10,type='b',pch=4,col='green')
par(new=T)
lines(1:10,cv.err4_k10.10,type='b',pch=4,col='black')

#Bootstrap
library(boot)
boot.fn1=function(Data_2018_cleansed,index){
  return(coef(lm(Data_ClPM10+1~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed, subset=index)))
}
boot.fn1(Data_2018_cleansed, 1:500)

set.seed(5)
boot.fn1(Data_2018_cleansed,sample(500,500,replace=T))
boot.fn1(Data_2018_cleansed,sample(500,500,replace=T))

boot(Data_2018_cleansed, boot.fn1, 1000)
summary(lm(PM10+1~SO2+CO+O3+NO2+PM10+PM25, data=Data_2018_cleansed))$coef

#Ridge Lasso
install.packages("glmnet")
library(glmnet)

data = Data_2018_cleansed[,c(2:7,10:11)]
n_pa = 6
attach(data)

x=data.matrix(data[,c(1:6)])
y=data$'PM10+4'
set.seed(7)
train = sample(1:nrow(x),nrow(x)*1/4)
test=(-train)
y.test=y[test]
data.train = data[train,]
attach(data.train)
library(MASS)


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

x=data.matrix(data[,c(1:6)])
y=data$'PM10+4'
set.seed(9)
train = sample(1:nrow(x),nrow(x)*1/4)
test=(-train)
y.test=y[test]

pcr.fit=pcr(y~.,data=data[,c(1:6)],subset=train,scale=TRUE,validation="CV")
pls.fit=plsr(y~.,data=data[,c(1:6)],subset=train,scale=TRUE,validation="CV")

summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP",ylab='CV MSE',,type='b',
               ylim=c(0,1000),xlab='Number of Components',col='black',lwd=2, main="")
par(new=TRUE)
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP",ylab='CV MSE',,type='b',
               ylim=c(0,1000),xlab='Number of Components',col='red',lwd=2, main="")
legend("topright",legend=c("PCR","PLS"),col=c("black","red"),pch=1,lwd=2)

#Prediction performance using test MSE
mlr = lm(y~.,data = data[,c(1:6)],subset=train)
summary(mlr)
tset_MSE1 = mean((y-predict(mlr,data))[-train]^2)

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

#GAMs
install.packages("gam")
library(gam)
attach(Data_2018_cleansed)
gam.m1 = gam(Data_2018_cleansed$'PM10+4'~SO2+CO+O3+NO2+s(PM10,4)+s(PM25,5))
gam.m2 = gam(Data_2018_cleansed$'PM10+4'~s(SO2,4)+s(CO,4)+s(O3,5)+s(NO2,5)+PM10+PM25)
gam.m3 = gam(Data_2018_cleansed$'PM10+4'~s(SO2,4)+s(CO,4)+s(O3,5)+s(NO2,5)+s(PM10,4)+s(PM25,5))
anova(gam.m1, gam.m2, gam.m3, test="F")
par(mfrow=c(2,3))
plot(gam.m3, se=TRUE, col="blue")

gam.n1 = gam(Data_2018_cleansed$'PM25+4'~SO2+CO+O3+NO2+s(PM10,4)+s(PM25,5))
gam.n2 = gam(Data_2018_cleansed$'PM25+4'~s(SO2,4)+s(CO,4)+s(O3,5)+s(NO2,5)+PM10+PM25)
gam.n3 = gam(Data_2018_cleansed$'PM25+4'~s(SO2,4)+s(CO,4)+s(O3,5)+s(NO2,5)+s(PM10,4)+s(PM25,5))
anova(gam.n1, gam.n2, gam.n3, test="F")
par(mfrow=c(2,3))
plot(gam.n3, se=TRUE, col="blue")

#Polynomial optimization
set.seed(10)
error1 = rep(0,10)
error2 = rep(0,10)
error3 = rep(0,10)
error4 = rep(0,10)
error5 = rep(0,10)

data = na.omit(data)
       
for (i in 1:10){
  glm1=glm(data$`PM10+4`~poly(data$SO2,i),data=data)
  glm2=glm(data$`PM10+4`~poly(data$CO,i),data=data)
  glm3=glm(data$`PM10+4`~poly(data$O3,i),data=data)
  glm4=glm(data$`PM10+4`~poly(data$NO2,i),data=data)
  glm5=glm(data$`PM10+4`~poly(data$PM25,i),data=data)
  error1[i] = mean((glm1$residuals)^2)
  error2[i] = mean((glm2$residuals)^2)
  error3[i] = mean((glm3$residuals)^2)
  error4[i] = mean((glm4$residuals)^2)
  error5[i] = mean((glm5$residuals)^2)
  print (i)
}

for (i in 1:10){
  glm1=glm(data$`PM10+4`~poly(data$SO2,i),data=data)
  glm2=glm(data$`PM10+4`~poly(data$CO,i),data=data)
  glm3=glm(data$`PM10+4`~poly(data$O3,i),data=data)
  glm4=glm(data$`PM10+4`~poly(data$NO2,i),data=data)
  glm5=glm(data$`PM10+4`~poly(data$PM25,i),data=data)
  error1[i] = cv.glm(data,glm1,K=2)$delta[1]
  error2[i] = cv.glm(data,glm2,K=2)$delta[1]
  error3[i] = cv.glm(data,glm3,K=2)$delta[1]
  error4[i] = cv.glm(data,glm4,K=2)$delta[1]
  error5[i] = cv.glm(data,glm5,K=2)$delta[1]
  print (i)
}

plot(1:10, error1, type='b',lty='dotdash',pch=1,ylim=c(0,1000),col=1,
     main='PM10',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error2, type='b',lty='dotdash',pch=1,ylim=c(0,1000),col=2,
     main='PM10',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error3, type='b',lty='dotdash',pch=1,ylim=c(0,1000),col=3,
     main='PM10',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error4, type='b',lty='dotdash',pch=1,ylim=c(0,1000),col=4,
     main='PM10',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error5, type='b',lty='dotdash',pch=1,ylim=c(0,1000),col=5,
     main='PM10',ylab='MSE',xlab='Order of Polynomial Regression')
legend("bottomright", c("SO2","CO","O3","NO2","PM25"), pch=1,col=c(1:5))

for (i in 1:10){
  glm1=glm(data$`PM25+4`~poly(data$SO2,i),data=data)
  glm2=glm(data$`PM25+4`~poly(data$CO,i),data=data)
  glm3=glm(data$`PM25+4`~poly(data$O3,i),data=data)
  glm4=glm(data$`PM25+4`~poly(data$NO2,i),data=data)
  glm5=glm(data$`PM25+4`~poly(data$PM10,i),data=data)
  error1[i] = mean((glm1$residuals)^2)
  error2[i] = mean((glm2$residuals)^2)
  error3[i] = mean((glm3$residuals)^2)
  error4[i] = mean((glm4$residuals)^2)
  error5[i] = mean((glm5$residuals)^2)
}

for (i in 1:10){
  glm1=glm(data$`PM25+4`~poly(data$SO2,i),data=data)
  glm2=glm(data$`PM25+4`~poly(data$CO,i),data=data)
  glm3=glm(data$`PM25+4`~poly(data$O3,i),data=data)
  glm4=glm(data$`PM25+4`~poly(data$NO2,i),data=data)
  glm5=glm(data$`PM25+4`~poly(data$PM10,i),data=data)
  error1[i] = cv.glm(data,glm1,K=2)$delta[1]
  error2[i] = cv.glm(data,glm2,K=2)$delta[1]
  error3[i] = cv.glm(data,glm3,K=2)$delta[1]
  error4[i] = cv.glm(data,glm4,K=2)$delta[1]
  error5[i] = cv.glm(data,glm5,K=2)$delta[1]
  print (i)
}

plot(1:10, error1, type='b',lty='dotdash',pch=1,ylim=c(0,400),col=1,
     main='PM25',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error2, type='b',lty='dotdash',pch=1,ylim=c(0,400),col=2,
     main='PM25',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error3, type='b',lty='dotdash',pch=1,ylim=c(0,400),col=3,
     main='PM25',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error4, type='b',lty='dotdash',pch=1,ylim=c(0,400),col=4,
     main='PM25',ylab='MSE',xlab='Order of Polynomial Regression')
par(new=T)
plot(1:10, error5, type='b',lty='dotdash',pch=1,ylim=c(0,400),col=5,
     main='PM25',ylab='MSE',xlab='Order of Polynomial Regression')
legend("bottomright", c("SO2","CO","O3","NO2","PM10"), pch=1,col=c(1:5))

#Final model
attach(data)
lm.fit10 = lm(data$`PM10+4`~SO2+CO+poly(O3,5,raw=TRUE)+NO2+PM10+PM25)
summary(lm.fit10)
lm.fit25 = lm(data$`PM25+4`~SO2+CO+poly(O3,4,raw=TRUE)+NO2+poly(PM10,2,raw=TRUE)+PM25)
summary(lm.fit25)

#Add Variable
Data_2018$'time' <- NA
Data_2018$time = Data_2018$측정일시%%100

Data_2018$'month' <- NA
Data_2018$month = (Data_2018$측정일시%%1000000)%/%10000

Data_2018 = Data_2018[,-18]
ndata = Data_2018[,c(3:8,11:12,16:17)]
ndata = na.omit(ndata)

attach(ndata)
contrasts(time)
lm.fit3 = lm(ndata$`PM10+4`~SO2+CO+poly(O3,5,raw=TRUE)+NO2+PM10+PM25+time+month)
summary(lm.fit3)
lm.fit4 = lm(ndata$`PM25+4`~SO2+CO+poly(O3,4,raw=TRUE)+NO2+poly(PM10,2,raw=TRUE)+PM25+time+month)
summary(lm.fit4)
