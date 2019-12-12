library(car)
library(dplyr)
library(tidyr)

#Data Cleansing
attach(Data_2018)

Data_2018$'levPM10' <- NA
Data_2018$'levPM25' <- NA

PM10.lev1 = subset(Data_2018, Data_2018$'PM10+4'<=30)
PM10.lev2 = subset(Data_2018, Data_2018$'PM10+4'>30 & Data_2018$'PM10+4'<=80)
PM10.lev3 = subset(Data_2018, Data_2018$'PM10+4'>80 & Data_2018$'PM10+4'<=150)
PM10.lev4 = subset(Data_2018, Data_2018$'PM10+4'>150)

PM25.lev1 = subset(Data_2018, Data_2018$'PM25+4'<=15)
PM25.lev2 = subset(Data_2018, Data_2018$'PM25+4'>16 & Data_2018$'PM25+4'<=35)
PM25.lev3 = subset(Data_2018, Data_2018$'PM25+4'>35 & Data_2018$'PM25+4'<=75)
PM25.lev4 = subset(Data_2018, Data_2018$'PM25+4'>75)

PM10.lev1$'levPM10' = 1
PM10.lev2$'levPM10' = 2
PM10.lev3$'levPM10' = 3
PM10.lev4$'levPM10' = 4
PM10.lev = bind_rows(PM10.lev1,PM10.lev2,PM10.lev3,PM10.lev4)
PM10.lev = PM10.lev[,-16]
PM10.lev = na.omit(PM10.lev)

PM25.lev1$'levPM25' = 1
PM25.lev2$'levPM25' = 2
PM25.lev3$'levPM25' = 3
PM25.lev4$'levPM25' = 4
PM25.lev = bind_rows(PM25.lev1,PM25.lev2,PM25.lev3,PM25.lev4)
PM25.lev = PM25.lev[,-15]
PM25.lev = na.omit(PM25.lev)

#PM10.lev & PM25.lev
set.seed(1)
train10 = sample(1:nrow(PM10.lev),nrow(PM10.lev)/4)
train25 = sample(1:nrow(PM25.lev),nrow(PM25.lev)/4)
PM10.lev.test = PM10.lev[-train10,]
PM25.lev.test = PM25.lev[-train25,]

plot(PM10.lev$PM10,PM10.lev$PM25,col=c("blue","black","yellow","red")[PM10.lev$levPM10],pch=19,
     xlab = 'PM10', ylab = 'PM25', main = 'Truth: PM10 (n=2366704)')
legend("topright", legend=c("좋음","보통","나쁨","매우나쁨"), col=c("blue","black","yellow","red"), pch=19)

plot(PM25.lev$PM10,PM25.lev$PM25,col=c("blue","black","yellow","red")[PM25.lev$levPM25],pch=19,
     xlab = 'PM10', ylab = 'PM25', main = 'Truth: PM25 (n=2294398)')
legend("topright", legend=c("좋음","보통","나쁨","매우나쁨"), col=c("blue","black","yellow","red"), pch=19)

#LDA, QDA, KNN
library(MASS)
library(class)
PM10.lev.train = PM10.lev[train10,]
PM25.lev.train = PM25.lev[train25,]

attach(PM10.lev.train)
lda.fit1 = lda(levPM10~SO2+CO+O3+NO2+PM10+PM25, data=PM10.lev.train)
lda.class1 = predict(lda.fit1, PM10.lev.test)$class
mean(PM10.lev.test$levPM10 == lda.class1)
table(PM10.lev.test$levPM10,lda.class1)

qda.fit1 = qda(levPM10~SO2+CO+O3+NO2+PM10+PM25, data=PM10.lev.train)
qda.class1 = predict(qda.fit1, PM10.lev.test)$class
mean(PM10.lev.test$levPM10 == qda.class1)
table(PM10.lev.test$levPM10,qda.class1)

train.x = PM10.lev.train[,3:8]
test.x = PM10.lev.test[,3:8]
lev.train = PM10.lev.train$levPM10
knn.pred1 = knn(train.x,test.x,lev.train,k=3)

attach(PM25.lev.train)
lda.fit2 = lda(levPM25~SO2+CO+O3+NO2+PM10+PM25, data=PM25.lev.train)
lda.class2 = predict(lda.fit2, PM25.lev.test)$class
mean(PM25.lev.test$levPM25 == lda.class2)
table(PM25.lev.test$levPM25,lda.class2)

qda.fit2 = qda(levPM25~SO2+CO+O3+NO2+PM10+PM25, data=PM25.lev.train)
qda.class2 = predict(qda.fit2, PM25.lev.test)$class
mean(PM25.lev.test$levPM25 == qda.class2)
table(PM25.lev.test$levPM25,qda.class2)

train.x = PM10.lev.train[,3:8]
test.x = PM10.lev.test[,3:8]
lev.train = PM10.lev.train$levPM10
knn.pred1 = knn(train.x,test.x,lev.train,k=5)

#Tree-based
install.packages('tree')
install.packages('randomForest')
install.packages('rpart')
library(tree)
library(randomForest)
library(rpart)

attach(PM10.lev.train)
tree.pm10 = rpart(levPM10~SO2+CO+O3+NO2+PM10+PM25, data=PM10.lev.train, method="class")
print(tree.pm10)
plot(tree.pm10, uniform=TRUE, main = "Classification Tree for PM10")
text(tree.pm10)
tree.pred = predict(tree.pm10,PM10.lev.test,type="class")
mean(tree.pred == PM10.lev.test$levPM10)
table(tree.pred,PM10.lev.test$levPM10)

printcp(tree.pm10)
plotcp(tree.pm10)

ptree.pm10 = prune(tree.pm10, cp=tree.pm10$cptable[which.min(tree.pm10$cptable[,"xerror"]),"CP"])
print(ptree.pm10)
plot(ptree.pm10,uniform=TRUE, main = "Pruned Classification Tree for PM10")
text(ptree.pm10)
ptree.pred = predict(ptree.pm10,PM10.lev.test,type="class")
mean(ptree.pred == PM10.lev.test$levPM10)
table(ptree.pred,PM10.lev.test$levPM10)

attach(PM25.lev.train)
tree.pm25 = rpart(levPM25~SO2+CO+O3+NO2+PM10+PM25, data=PM25.lev.train, method="class")
print(tree.pm25)
plot(tree.pm25, uniform=TRUE, main = "Classification Tree for PM25")
text(tree.pm25)
tree.pred = predict(tree.pm25,PM25.lev.test,type="class")
mean(tree.pred == PM25.lev.test$levPM25)
table(tree.pred,PM25.lev.test$levPM25)

printcp(tree.pm25)
plotcp(tree.pm25)

ptree.pm10 = prune(tree.pm10, cp=tree.pm10$cptable[which.min(tree.pm10$cptable[,"xerror"]),"CP"])
print(ptree.pm10)
plot(ptree.pm10,uniform=TRUE, main = "Pruned Classification Tree for PM10")
text(ptree.pm10)
ptree.pred = predict(ptree.pm10,PM10.lev.test,type="class")
mean(ptree.pred == PM10.lev.test$levPM10)
table(ptree.pred,PM10.lev.test$levPM10)

#SVM
library(e1071)

attach(PM10.lev.train)
svm10.linear = svm(levPM10~SO2+CO+O3+NO2+PM10+PM25,data=PM10.lev.train,kernel='linear',cost=1)
