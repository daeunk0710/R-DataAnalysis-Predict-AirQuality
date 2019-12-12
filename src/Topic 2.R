install.packages('car')
install.packages('dplyr')
library(car)
library(dplyr)

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

plot(Data_2018_cleansed$PM10)

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

Data_2018_cleansed = na.omit(Data_2018)
count(Data_2018_cleansed$`PM10+4`)
      
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
