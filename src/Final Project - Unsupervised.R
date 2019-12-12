library(tidyr)
Data_2018 = tibble::rownames_to_column(Data_2018,"index")
Data_2018 = Data_2018[,-2]
Data_2018_raw = tibble::rownames_to_column(Data_2018_raw,"index")
Data_2018_reg = Data_2018_raw[,1:3]
Data_2018 = merge(Data_2018,Data_2018_reg,by="index")
Data_2018 = separate(Data_2018,지역,c("reg1","reg2")," ")
Data_2018_cleansed = na.omit(Data_2018)
count(Data_2018_cleansed$`PM10+4`)

Data_2018$'time' <- NA
Data_2018$time = Data_2018$측정일시%%100


seoul = subset(Data_2018,reg1 == '서울') #n=343104
gangwon = subset(Data_2018,reg1 == '강원') #n=108958
busan = subset(Data_2018,reg1 == '부산') #n=192432

set.seed(100000)
train = sample(1:nrow(seoul),nrow(seoul)/2)
training = seoul[train,]
testing = seoul[!train,]

seoul.train = scale(training[-13:14])
summary(seoul.train)

tapply(seoul$PM10,seoul$time,mean)
tapply(seoul$PM10,seoul$time,median)

plot(seoul$time,seoul$PM10, main="Seoul", xlab="Time",ylab="PM10")
plot(seoul$time,seoul$PM25, main="Seoul", xlab="Time",ylab="PM25")

plot(gangwon$time,gangwon$PM10, main="Gangwon", xlab="Time",ylab="PM10")
plot(gangwon$time,gangwon$PM25, main="Gangwon", xlab="Time",ylab="PM25")

plot(busan$time,busan$PM10, main="Busan", xlab="Time",ylab="PM10")
plot(busan$time,busan$PM25, main="Busan", xlab="Time",ylab="PM25")

install.packages("caret")
library(caret)
set.seed(123)

#seoul
seoul.pm10 = seoul[,c("PM10","time")]
seoul.pm10 = na.omit(seoul.pm10)
seoul.kmeans = kmeans(seoul.pm10, 4, nstart = 20)
summary(subset(seoul.pm10,seoul.kmeans$cluster == 1))
summary(subset(seoul.pm10,seoul.kmeans$cluster == 2))
summary(subset(seoul.pm10,seoul.kmeans$cluster == 3))
summary(subset(seoul.pm10,seoul.kmeans$cluster == 4))

seoul.pm25 = seoul[,c("PM25","time")]
seoul.pm25 = na.omit(seoul.pm25)
seoul.kmeans = kmeans(seoul.pm25, 4, nstart = 20)
summary(subset(seoul.pm25,seoul.kmeans$cluster == 1))
summary(subset(seoul.pm25,seoul.kmeans$cluster == 2))
summary(subset(seoul.pm25,seoul.kmeans$cluster == 3))
summary(subset(seoul.pm25,seoul.kmeans$cluster == 4))
seoul.kmeans

#gangwon
gangwon.pm10 = gangwon[,c("PM10","time")]
gangwon.pm10 = na.omit(gangwon.pm10)
gangwon.kmeans = kmeans(gangwon.pm10, 4, nstart = 20)
summary(subset(gangwon.pm10,gangwon.kmeans$cluster == 1))
summary(subset(gangwon.pm10,gangwon.kmeans$cluster == 2))
summary(subset(gangwon.pm10,gangwon.kmeans$cluster == 3))
summary(subset(gangwon.pm10,gangwon.kmeans$cluster == 4))

gangwon.pm25 = gangwon[,c("PM25")]
gangwon.pm25 = na.omit(gangwon.pm25)
gangwon.kmeans = kmeans(gangwon.pm25, 4, nstart = 20)
summary(subset(gangwon.pm25,gangwon.kmeans$cluster == 1))
summary(subset(gangwon.pm25,gangwon.kmeans$cluster == 2))
summary(subset(gangwon.pm25,gangwon.kmeans$cluster == 3))
summary(subset(gangwon.pm25,gangwon.kmeans$cluster == 4))
gangwon.kmeans

#busan
busan.pm10 = busan[,c("PM10","time")]
busan.pm10 = na.omit(busan.pm10)
busan.kmeans = kmeans(busan.pm10, 4, nstart = 20)
summary(subset(busan.pm10,busan.kmeans$cluster == 1))
summary(subset(busan.pm10,busan.kmeans$cluster == 2))
summary(subset(busan.pm10,busan.kmeans$cluster == 3))
summary(subset(busan.pm10,busan.kmeans$cluster == 4))
busan.kmeans

busan.pm25 = busan[,c("PM25")]
busan.pm25 = na.omit(busan.pm25)
busan.kmeans = kmeans(busan.pm25, 4, nstart = 20)
summary(subset(busan.pm25,busan.kmeans$cluster == 1))
summary(subset(busan.pm25,busan.kmeans$cluster == 2))
summary(subset(busan.pm25,busan.kmeans$cluster == 3))
summary(subset(busan.pm25,busan.kmeans$cluster == 4))
busan.kmeans

#전국
all.pm10 = Data_2018[,c("PM10","time")]
all.pm10 = na.omit(all.pm10)
all.kmeans = kmeans(all.pm10, 4, nstart=20)
summary(subset(all.pm10,all.kmeans$cluster == 1))
summary(subset(all.pm10,all.kmeans$cluster == 2))
summary(subset(all.pm10,all.kmeans$cluster == 3))
summary(subset(all.pm10,all.kmeans$cluster == 4))

all.pm25 = Data_2018[,c("PM25","time")]
all.pm25 = na.omit(all.pm25)
all.kmeans = kmeans(all.pm25, 4, nstart=20)
summary(subset(all.pm25,all.kmeans$cluster == 1))
summary(subset(all.pm25,all.kmeans$cluster == 2))
summary(subset(all.pm25,all.kmeans$cluster == 3))
summary(subset(all.pm25,all.kmeans$cluster == 4))
