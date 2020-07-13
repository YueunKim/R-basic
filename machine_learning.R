
setwd("c:/Rdata")
getwd()

nrow(iris)
150*0.7

iris_train = iris[1:105,]
iris_test = iris[106:150,]
nrow(iris_train)
nrow(iris_test)
head(iris)

idx = sample(1:nrow(iris),size = nrow(iris)*0.7,replace = F) #replace = F : 중복불가
idx

iris_train = iris[idx,]
iris_test = iris[-idx,]
nrow(iris_train)
nrow(iris_test)
head(iris)

table(iris$Species)
table(iris_train$Species)
table(iris_test$Species)

# 각 변수의 값이 같지 않으므로 형평성이 맞지않음, 따라서 caret패키지를 사용

install.packages("caret")
library(caret)

train.idx = createDataPartition(iris$Species, p=0.7, list=F)

iris_train = iris[train.idx, ]
iris_test = iris[-train.idx, ]

table(iris_train$Species)
table(iris_test$Species)




# Naive Bayes Model Method
library(e1071)
naive.result = naiveBayes(iris_train, iris_train$Species, laplace = 1) 
naive.pred = predict(naive.result,iris_test,type = "class")
table(naive.pred,iris_test$Species)
confusionMatrix(naive.pred, iris_test$Species)




# Logistic Regression Model Method
library(nnet)
multi.result = multinom(Species~., iris_train)
multi.pred = predict(multi.result, iris_test)
table(multi.pred, iris_test$Species)
confusionMatrix(multi.pred, iris_test$Species)




# Decision Tree model method
library(rpart)
rpart.result = rpart(Species~., iris_train)
rpart.pred = predict(rpart.result, iris_test, type = "class")
table(rpart.pred, iris_test$Species)
confusionMatrix(rpart.pred, iris_test$Species)
#시각화
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rpart.result)




# ANN Model Method
library(nnet)
head(iris_train)
iris_train_scale = as.data.frame(sapply(iris_train[,-5], scale)) #5열은 종속변수내용이므로 빼고 입력변수만 할당
iris_test_scale = as.data.frame(sapply(iris_test[,-5],scale))

iris_train_scale$Species = iris_train$Species
iris_test_scale$Species = iris_test$Species

nnet.result = nnet(Species~., iris_train_scale, size = 3)
nnet.pred = predict(nnet.result, iris_test_scale, type = "class")
table(nnet.pred, iris_test$Species)

confusionMatrix(as.factor(nnet.pred), as.factor(iris_test$Species))




# svm model method
install.packages("kernlab")
library(kernlab)

train.idx = createDataPartition(iris$Species, p=0.7, list=F)
iris_train = iris[train.idx, ]
iris_test = iris[-train.idx, ]

svm.result = ksvm(Species~., data = iris_train, kernel="rbfdot")
svm.pred=predict(svm.result, iris_test, type = "response")
table(svm.pred, iris_test$Species)
confusionMatrix(svm.pred, iris_test$Species)




# Random Forest Method
install.packages("randomForest")
library(randomForest)

rf.result = randomForest(Species~., data = iris_train, ntree = 500)
rf.pred = predict(rf.result,iris_test, type = "response")
table(rf.pred, iris_test$Species)
confusionMatrix(rf.pred, iris_test$Species)




#package안의 데이터?
data(package="MASS")

boston = as.data.frame(MASS::Boston)
names(boston)
nrow(boston)
idx = sample(1:nrow(boston), size = nrow(boston)*0.7, replace = F)
idx
boston_train = boston[idx,]
boston_test = boston[-idx,]

lm.fit = lm(medv~., data=boston_train)
summary(lm.fit)

#both방식 (코드,백워드에서 장점만 뽑아)
lm.fit2 = step(lm.fit, method="both")
summary(lm.fit2)

lm.yhat2 = predict(lm.fit2, newdata=boston_test)
mean((lm.yhat2-boston_test$medv)^2) #(예측값 - 실제값)^2
kk=mean((lm.yhat2-boston_test$medv)^2)
sqrt(kk)
plot(lm.yhat2, boston_test$medv)
abline(a=0,b=1,col=2)



par(mfrow = c(1,1))
# 수치해석에서 다중회귀분석
# 의사결정트리기법 in 수치

install.packages("tree")
library(tree)

tree.fit = tree(medv~., data=boston_train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty=0)

tree.yhat = predict(tree.fit, newdata = boston_test)
tree.yhat

kk=mean((tree.yhat-boston_test$medv)^2)
sqrt(kk)

## rpart를 통한 의사결정트리분석방법
library(rpart)
rpart.fit = rpart(medv~., data=boston_train)
summary(rpart.fit)
library(rpart.plot)
rpart.plot(rpart.fit, digits = 3, type = 0, extra = 1, fallen.leaves = F, cex = 1)
rpart.yhat = predict(rpart.fit, newdata=boston_test)
kk=mean((rpart.yhat-boston_test$medv)^2) #(예측값 - 실제값)^2
sqrt(kk)


## ANN 수치예측모형의 머신러닝알고리즘 방법
normalize = function(x){return((x-min(x))/(max(x)-min(x)))}
boston_train_norm = as.data.frame(sapply(boston_train, normalize))
boston_test_norm = as.data.frame(sapply(boston_test, normalize))

library(nnet)
nnet.fit = nnet(medv~., data=boston_train_norm, size = 5)
nnet.yhat = predict(nnet.fit, newdata = boston_test_norm, type = 'raw')
kk=mean((nnet.yhat-boston_test_norm$medv)^2)
sqrt(kk)


## ANN 의 시각화를 위해 사용하는 알고리즘 방법
install.packages("neuralnet")
library(neuralnet)

neural.fit = neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                       data = boston_train_norm, hidden = 5)
neural.results = compute(neural.fit, boston_test_norm[1:13])
neural.yhat = neural.results$net.result
kk=mean((neural.yhat=boston_test_norm$medv)^2)
sqrt(kk)
plot(neural.fit)



# RF Model(앙상블) method 수치머신러닝
library(randomForest)
set.seed(1)

rf.fit=randomForest(medv~.,data=boston_train, mtry=6, importance = T)
plot(rf.fit)
importance(rf.fit)
varImpPlot(rf.fit)
rf.yhat = predict(rf.fit, newdata = boston_test)
kk=mean((rf.yhat-boston_test$medv)^2)
sqrt(kk)




# 자율학습모델(unsupervised model)

#k-means model method

iris2 = iris[,1:4] #목표변수제외
iris2

km.out.withness = c()
km.out.between = c()
kk = c()
for (i in 2:7) {
  set.seed(1)
           km.out = kmeans(iris2,centers=i)
           km.out.withness[i-1] = km.out$tot.withinss
           km.out.between[i-1] = km.out$betweenss
           kk=c(kk,(paste0("k=",i)))
  
          }
tt=data.frame(km.out.withness, km.out.between)
par(mfrow=c(1,2))
plot(kk=c(2:7), tt$km.out.withness, type = 'b' )
plot(kk=c(1:7), tt$km.out.between, type = 'b' )

km.out.k3 = kmeans(iris2,centers = 3)
km.out.k3$centers
km.out.k3$cluster
km.out.k3$size
table(km.out.k3$cluster, iris$Species)

plot(iris2[,1:2], col=km.out.k3$cluster, pch=ifelse(km.out.k3$cluster==1, 16, 
                                                    ifelse(km.out.k3$cluster==2, 17, 18)),
     cex=2)
points(km.out.k3$centers, col= 1:3, pch = 16:18, cex=5)



# 주성분분석
USArrests
pc1 = princomp(USArrests,cor=T)
plot(pc1)
summary(pc1)

pc1$center
pc1$scale
pc1$loadings
pc1$scores

par(mfrow = c(1,1))
plot(pc1$scores[,1], pc1$scores[,2], xlab='21', ylab='22')
abline(v=0,h=0)

biplot(pc1,cex=0.7)
abline(v=0,h=0,col='gray')
