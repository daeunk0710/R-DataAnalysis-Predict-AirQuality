install.packages('MASS')
library(MASS)

#전국교량표준데이터
raw_bridge = read.csv("전국교량표준데이터.csv")
data_bridge = raw_bridge[c(16:22,27)]
data_bridge = data_bridge[-(1:3)]
data_bridge_c = subset(data_bridge, 통행하중!=0 & 교량연장<5000 & 통행하중<1000)
data_bridge_n = subset(data_bridge_c, data_bridge_c$Type =Slab)
data_bridge_c$nLength = (교량연장-min(교량연장))/(max(교량연장)-min(교량연장))
data_bridge_c$nWeight = (통행하중-min(통행하중))/(max(통행하중)-min(통행하중))

plot(data_bridge_c)

attach(data_bridge_c)
summary(data_bridge_c)

plot(교량연장,통행하중, col=c(1,2,3,4,5,6)[Type])
plot(nLength, nWeight)

#mlcourse
raw_adult = read.csv("adult.csv")
raw_affairs = read.csv("affairs.csv")

#mushrooms
raw_mushrooms = read.csv("mushrooms.csv")

#Iris
iris = read.csv("iris.csv")
attach(iris)
plot(PetalLengthCm,PetalWidthCm, col=c("red","blue","green")[Species])
legend("bottomright", legend=c("Setosa","Versicolor","Virginica"), 
       col = c("red","blue","green"), pch=1)

#1.Logistic Regression
iris$Type=0
iris$Type[iris$Species=='Iris-setosa']= 1

train=(SepalWidthCm==3)
iris.train=iris[train,]
iris.pred=iris[!train,]
Type.pred=iris$Type[!train]
plot(iris.train$PetalLengthCm, iris.train$PetalWidthCm, main="Given train data set (n=26)",
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=c("red","black","black")[iris.train$Species], pch=1)
legend("bottomright", legend=c("Versicolor & Virginica", "Setosa"), col=1:2, pch=1)
plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="Given test set (n=124): truth",
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=c("red","black","black")[iris.pred$Species], pch=1)
legend("bottomright", legend=c("Versicolor & Virginica", "Setosa"), col=1:2, pch=1)

glm.fit=glm(Type~PetalLengthCm+PetalWidthCm, data=iris.train, family=binomial, control=list(maxit=50))
glm.prob=predict(glm.fit,iris.pred,type="response")
glm.pred=rep(0,length(iris.pred$Type))
glm.pred[glm.prob>0.5]=1
glm.pred=factor(glm.pred, levels = c("0","1"))

plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="Prediction Result (n=124)", 
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=glm.pred, pch=1)
legend("bottomright", legend=c("Versicolor & Virginica", "Setosa"), col=1:2, pch=1)

mean(glm.pred==Type.pred)
table(glm.pred,Type.pred)
glm.pred
Type.pred

install.packages("ROCR")
library(ROCR)
install.packages("arulesViz")
library(arulesViz)
install.packages("randomForest")
library(randomForest)

glm.rocr = prediction(glm.prob, Type.pred)
glm.roc = performance(glm.rocr, "tpr", "fpr")
plot(glm.roc, col=1, lty=1, lwd=2)

glm.auc=performance(glm.rocr, "auc")
glm.auc=glm.auc@y.values[[1]]

#Comparison between classification methods
iris$Type=0
iris$Type[iris$Species=='Iris-versicolor']= 1
iris$nPetalLengthCm = (iris$PetalLengthCm-min(iris$PetalLengthCm))/(max(iris$PetalLengthCm)-min(iris$PetalLengthCm))
iris$nPetalWidthCm = (iris$PetalWidthCm-min(iris$PetalWidthCm))/(max(iris$PetalWidthCm)-min(iris$PetalWidthCm))

train=(SepalWidthCm==3)
iris.train=iris[train,]
iris.pred=iris[!train,]
Type.pred=iris$Type[!train]
plot(iris.train$PetalLengthCm, iris.train$PetalWidthCm, main="Given train data set (n=26)",
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=c("black","red","black")[iris.train$Species], pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)
plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="Given test set (n=124): truth",
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=c("black","red","black")[iris.pred$Species], pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)

#Logistic Regression
glm.fit=glm(Type~PetalLengthCm+PetalWidthCm, data=iris.train, family=binomial, control=list(maxit=50))
glm.prob=predict(glm.fit,iris.pred,type="response")
glm.pred=rep(0,length(iris.pred$Type))
glm.pred[glm.prob>0.5]=1
glm.pred=factor(glm.pred, levels = c("0","1"))

plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="Logistic Regression", 
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=glm.pred, pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)
mean(glm.pred==Type.pred)
table(glm.pred,Type.pred)

#LDA and QDA
lda.fit=lda(Type~PetalLengthCm+PetalWidthCm, data=iris.train)
qda.fit=qda(Type~PetalLengthCm+PetalWidthCm, data=iris.train)
lda.class=predict(lda.fit, iris.pred)$class
qda.class=predict(qda.fit, iris.pred)$class

plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="LDA", 
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=lda.class, pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)
mean(lda.class==Type.pred)
table(lda.class,Type.pred)

plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="QDA", 
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=qda.class, pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)
mean(qda.class==Type.pred)
table(qda.class,Type.pred)

#KNN
library(class)
train.x=cbind(nPetalLengthCm,nPetalWidthCm)[train,]
test.x=cbind(nPetalLengthCm,nPetalWidthCm)[!train,]
Type.train=iris$Type[train]
knn.pred=knn(train.x,test.x,Type.train,k=5,prob=TRUE)
plot(iris.pred$nPetalLengthCm, iris.pred$nPetalWidthCm, main="KNN (n=5)", 
     xlab="nPetalLengthCm", ylab="nPetalWidthCm",col=knn.pred, pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)
mean(knn.pred==Type.pred)
table(knn.pred,Type.pred)

#ROC curve
glm.rocr = prediction(glm.prob, Type.pred)
glm.roc = performance(glm.rocr, "tpr", "fpr")
lda.rocr = prediction(predict(lda.fit, iris.pred)$posterior[,2],Type.pred)
lda.roc = performance(lda.rocr, "tpr", "fpr")
qda.rocr = prediction(predict(qda.fit, iris.pred)$posterior[,2],Type.pred)
qda.roc = performance(qda.rocr, "tpr", "fpr")
knn.prob = attr(knn.pred,"prob")
knn.prob = 2*ifelse(knn.pred == "1", 1-knn.prob, knn.prob)-1
knn.rocr = prediction(knn.prob, Type.pred)
knn.roc = performance(knn.rocr, "tpr", "fpr")

plot(glm.roc, col=1, lty=1, lwd=2)
plot(lda.roc, col=2, lty=3, lwd=2, add=TRUE)
plot(qda.roc, col=3, lty=5, lwd=2, add=TRUE)
plot(knn.roc, col=4, lty=6, lwd=2, add=TRUE)

glm.auc=performance(glm.rocr, "auc")
glm.auc=glm.auc@y.values[[1]]
lda.auc=performance(lda.rocr, "auc")
lda.auc=lda.auc@y.values[[1]]
qda.auc=performance(qda.rocr, "auc")
qda.auc=qda.auc@y.values[[1]]
knn.auc=performance(knn.rocr, "auc")
knn.auc=knn.auc@y.values[[1]]

#modified QDA
mqda.class=(predict(qda.fit, iris.pred)$posterior[,2]>0.1)
plot(iris.pred$PetalLengthCm, iris.pred$PetalWidthCm, main="modified QDA", 
     xlab="PetalLengthCm", ylab="PetalWidthCm",col=mqda.class, pch=1)
legend("bottomright", legend=c("Setosa & Virginica", "Versicolor"), col=1:2, pch=1)
mean(mqda.class==Type.pred)
table(mqda.class,Type.pred)

#3 class classification (LDA, QDA, KNN)
train=(SepalWidthCm==3)
iris.train=iris[train,]
iris.pred=iris[!train,]
species.pred=iris$Species[!train]

#LDA
lda.fit=lda(Species~nPetalLengthCm+nPetalWidthCm, data=iris.train)
lda.class=predict(lda.fit, iris.pred)$class
plot(iris.pred$nPetalLengthCm, iris.pred$nPetalWidthCm, main="LDA", 
     xlab="nPetalLengthCm", ylab="nPetalWidthCm",col=lda.class, pch=1)
legend("bottomright", legend=c("Setosa","Versicolor", "Virginica"), col=1:3, pch=1)
mean(lda.class==species.pred)
table(lda.class,species.pred)

#QDA
qda.fit=qda(Species~nPetalLengthCm+nPetalWidthCm, data=iris.train)
qda.class=predict(qda.fit, iris.pred)$class
plot(iris.pred$nPetalLengthCm, iris.pred$nPetalWidthCm, main="QDA", 
     xlab="nPetalLengthCm", ylab="nPetalWidthCm",col=qda.class, pch=1)
legend("bottomright", legend=c("Setosa","Versicolor", "Virginica"), col=1:3, pch=1)
mean(qda.class==species.pred)
table(qda.class,species.pred)

#KNN
train.x=cbind(nPetalLengthCm,nPetalWidthCm)[train,]
test.x=cbind(nPetalLengthCm,nPetalWidthCm)[!train,]
species.train=iris$Species[train]
knn.pred=knn(train.x,test.x,Type.train,k=5,prob=TRUE)
plot(iris.pred$nPetalLengthCm, iris.pred$nPetalWidthCm, main="KNN (n=5)", 
     xlab="nPetalLengthCm", ylab="nPetalWidthCm",col=knn.pred, pch=1)
legend("bottomright", legend=c("Setosa","Versicolor", "Virginica"), col=1:3, pch=1)
mean(knn.pred==species.pred)
table(knn.pred,species.pred)

#Resampling (k-fold CV)
install.packages("crossval")
library(crossval)

predfun.qda = function(train.x, train.y, test.x, test.y, negative)
{
        qda.fit=qda(train.x, grouping = train.y)
        ynew = predict(qda.fit, test.x)$class
        out = confusionMatrix(test.y, ynew, negative = negative)
        return(out)
}

set.seed(5)
X = as.matrix(iris[,4:5])
Y = iris[,6]

cv.out = crossval(predfun.qda, X, Y, K=10, B=1, negative='No')
cv.out$stat
diagnosticErrors(cv.out$stat)

set.seed(10)
cv.out = crossval(predfun.qda, X, Y, K=10, B=1, negative='No')
cv.out$stat
diagnosticErrors(cv.out$stat)

set.seed(10)
cv.error.10=rep(0,10)
for (i in 1:10){
        cv.error.10[i]=knn.cv(train.x,species.train,k=i,prob=TRUE)
}

knn.cv(train.x,species.train,k=10, prob=TRUE)
knn.cv(train.x,species.train,k=3, prob=TRUE)
