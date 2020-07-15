install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
library(dplyr)
library(ggplot2)
library(caret)

## 데이터 불러오기
setwd("c:/Rdata")
data = read.csv("sales_AI_first1.csv")
head(data)

## 데이터 구성 - 데이터는10개의 변수와 120개의 object로 구성되어 있음.
str(data)

## data1 --> 비타민음료만
data1 = data %>% filter(CATEGORY == "비타민음료")
str(data1)
head(data1)

## data1 - 영향변수들만
data1_eff = data1 %>% select(-YM, -CATEGORY)
data1_eff

## data2 --> 스포츠,이온음료만
data2 = data %>% filter(CATEGORY == "스포츠,이온음료")
str(data2)
head(data2)

## data2 - 영향변수들만
data2_eff = data2 %>% select(-YM, -CATEGORY)
data2_eff



## shapiro.test : 데이터가 정규 분포를 따르는지 샤피로 윌크 검정을 수행한다
### 방법 : 일반 탄산음료와 과즙음료를 비교 분석하여 진행.

### p-valued 값이 0.05보다 작으므로 정규분포를 따른다
shapiro.test(data1_eff$QTY)
shapiro.test(data2_eff$QTY)

#######3hist(data1$QTY)
#######hist(data2$QTY)


## 정규성
par(mfrow = c(1,1))
qqnorm(data1_eff$QTY) #Q-Q plot상의 직선에서 점들이 크게 벗어나 있지 않는다면 QTY 변수는 정규 분포를 따른다고 볼 수 있다.
qqline(data1_eff$QTY) 
qqnorm(data2_eff$QTY)
qqline(data2_eff$QTY)





## 상관관계분석
cor(data1_eff)
cor(data2_eff)



## 다중회귀분석
## R에서는 step() 함수를 사용하여 변수선택을 결정할 수 있다.
## direction이 변수선택 방법을 지정하는 것이다. direction이 both이면 단계이고, forward이면 전진, backward이면 후진이 된다.
## QTY를 종속변수로 하고, 나머지 변수를 모두 독립변수로 한 다중회귀분석

out1 = lm(QTY~., data = data1_eff) 
out2 = lm(QTY~., data = data2_eff)


## Stepwise selection 모형 선택시

both1 = step(out1, direction="both",trace = FALSE)
both2 = step(out2, direction="both",trace = FALSE)

summary(both1)
summary(both2)


## 추정을 위한 회귀모형에 따른 유의성 검증과 잔차 분석
## 분산분석 (ANOVA : Analysis Of Variance): 결과가 유의미한지를 판별

anova(both1)
anova(both2)


## 다중공선성 검사
# 10을 넘지 않으므로 패스
library(car)
a1= vif(both1)
b1= vif(both2)
sqrt(a1)
sqrt(b1)


par(mfrow = c(2,2))
##  Normal Q-Q : 잔차가 정규분포를 따르는지 확인하기 위한 Q-Q plot
##  Scale-Location : 이상점(outlier)을 탐지할 수 있는 그래프로 빨간색 추세선이 0인 직선이 가장 이상적이며 크게 벗어난 값은 이상점일 가능성 있음
##  Residuals vs Leverage : 레버리지(leverage)는 설명변수가 얼마나 극단에 치우쳐 있는지를 말합니다. 
##                          대부분 값들이 1~10일 때 어느 한 값이 1000을 갖는다면 그 1000을 갖는 점은 레버리지 값이 크다고 말함

plot(both1)
plot(both2)

## 예측모형 검증

data1_eff = data1_eff %>% 
  mutate(QTY_pred = -2212 + -29.59*X +52.28*ITEM_CNT + 0.9627*PRICE + 9.764 * MAXTEMP + 0.007023*SALEDAY+ 0.005063*RAIN_DAY)
data1_eff


data2_eff = data2_eff %>% 
  mutate(QTY_pred = 2328 - 3.122*PRICE + 66.72 * MAXTEMP + 0.01273*SALEDAY + 76.38*HOLIDAY)
data2_eff



## Accurancy
data1_eff = data1_eff %>%
  mutate(QTY_acc = ifelse(QTY_pred < QTY, ((QTY_pred/QTY) * 100),((QTY/QTY_pred) * 100)))
data1_eff

data2_eff = data2_eff %>%
  mutate(QTY_acc = ifelse(QTY_pred < QTY, ((QTY_pred/QTY) * 100),((QTY/QTY_pred) * 100)))
data2_eff

mean(data1_eff$QTY_acc)
mean(data2_eff$QTY_acc)




## All subsets 모형 선택시

out1 = lm(QTY~., data = data1_eff) 
out2 = lm(QTY~., data = data2_eff)


par(mfrow = c(2,2))
library(leaps)

leaps1 = regsubsets(QTY~., data = data1_eff, nbest = 5)
summary(leaps1)
plot(leaps1)

plot(leaps,scale='bic')
out_bic1 = lm(QTY~ITEM_CNT+PRICE+MAXTEMP, data =  data1_eff)
summary(out_bic1)

plot(leaps, scale = "Cp")
out_cp1 = lm(QTY~X+ITEM_CNT+PRICE+MAXTEMP+SALEDAY, data = data1_eff)
summary(out_cp1)

plot(leaps,scale = "adjr2")
out_ad1 = lm(QTY~X+ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY, data = data1_eff)
summary(out_ad1)
# adjr2가 가장 높은 결정계수를 가짐
# 하지만 both1과 같은값


leaps2 = regsubsets(QTY~., data = data2_eff, nbest = 5)
summary(leaps2)
plot(leaps2)

plot(leaps2,scale='bic')
out_bic2 = lm(QTY~PRICE+MAXTEMP+SALEDAY+HOLIDAY, data =  data2_eff)
summary(out_bic2)

plot(leaps2, scale = "Cp")
out_cp2 = lm(QTY~PRICE+MAXTEMP+SALEDAY+HOLIDAY, data = data2_eff)
summary(out_cp2)

plot(leaps2,scale = "adjr2")
out_ad2 = lm(QTY~PRICE+MAXTEMP+SALEDAY+HOLIDAY, data = data2_eff)
summary(out_ad2)
# 셋 다 같은 결정계수를 가짐
# 하지만 both2과 같은값




## 추정을 위한 회귀모형에 따른 유의성 검증과 잔차 분석
## 분산분석 (ANOVA : Analysis Of Variance): 결과가 유의미한지를 판별

anova(out_ad1)
anova(out_cp2)


## 다중공선성 검사
# 10을 넘지 않으므로 패스
library(car)
a2= vif(out_ad1)
b2= vif(out_cp2)
sqrt(a2)
sqrt(b2)



par(mfrow = c(2,2))
##  Normal Q-Q : 잔차가 정규분포를 따르는지 확인하기 위한 Q-Q plot
##  Scale-Location : 이상점(outlier)을 탐지할 수 있는 그래프로 빨간색 추세선이 0인 직선이 가장 이상적이며 크게 벗어난 값은 이상점일 가능성 있음
##  Residuals vs Leverage : 레버리지(leverage)는 설명변수가 얼마나 극단에 치우쳐 있는지를 말합니다. 
##                          대부분 값들이 1~10일 때 어느 한 값이 1000을 갖는다면 그 1000을 갖는 점은 레버리지 값이 크다고 말함

plot(out_ad1)
plot(out_cp2)

## 예측모형 검증

data1_eff = data1_eff %>% 
  mutate(QTY_pred = -2212 + -29.59*X +52.28*ITEM_CNT + 0.9627*PRICE + 9.764 * MAXTEMP + 0.007023*SALEDAY+ 0.005063*RAIN_DAY)
data1_eff


data2_eff = data2_eff %>% 
  mutate(QTY_pred = 2328 - 3.122*PRICE + 66.72 * MAXTEMP + 0.01273*SALEDAY + 76.38*HOLIDAY)
data2_eff



## Accurancy
data1_eff = data1_eff %>%
  mutate(QTY_acc = ifelse(QTY_pred < QTY, ((QTY_pred/QTY) * 100),((QTY/QTY_pred) * 100)))
data1_eff

data2_eff = data2_eff %>%
  mutate(QTY_acc = ifelse(QTY_pred < QTY, ((QTY_pred/QTY) * 100),((QTY/QTY_pred) * 100)))
data2_eff

mean(data1_eff$QTY_acc)
mean(data2_eff$QTY_acc)



------------------------------------------------------------------------
data = read.csv("sales_AI_first1.csv")
head(data)

## 데이터 구성 - 데이터는10개의 변수와 120개의 object로 구성되어 있음.
str(data)

## data1 --> 비타민음료만
data1 = data %>% filter(CATEGORY == "비타민음료")
str(data1)
head(data1)

## data1 - 영향변수들만
data1_eff = data1 %>% select(-YM, -CATEGORY)
data1_eff

## data2 --> 스포츠,이온음료만
data2 = data %>% filter(CATEGORY == "스포츠,이온음료")
str(data2)
head(data2)

## data2 - 영향변수들만
data2_eff = data2 %>% select(-YM, -CATEGORY)
data2_eff

## train_set, test_set
set.seed(649)    ## --> 74.3, 87.6
idx = sample(1:nrow(data1_eff),size = nrow(data1_eff)*0.7, replace = F)
idx
#train.idx = createDataPartition(data1_eff$QTY, p = 0.7, list = F)  

data1_train = data1_eff[idx, ]
data1_test = data1_eff[-idx, ]
data2_train = data2_eff[idx, ]
data2_test = data2_eff[-idx, ]

## 70프로 30프로 나눈 것 확인
dim(data1_train)
dim(data1_test)
dim(data2_train)
dim(data2_test)

## 훈련
lm.fit1 = lm(QTY~., data = data1_train)
summary(lm.fit)
lm.fit2 = lm(QTY~., data = data2_train)
summary(lm.fit)

## 예측
lm.yhat1 = predict(lm.fit1, newdata = data1_test)
lm.yhat1

lm.yhat2 = predict(lm.fit2, newdata = data2_test)
lm.yhat2

## 예측 값 데이터프레임에 넣기
data1_test = data1_test %>% 
  mutate(QTY_pred =lm.yhat1)
data1_test

data2_test = data2_test %>% 
  mutate(QTY_pred =lm.yhat2)
data2_test

## 정확도 구하기
data1_test = data1_test %>%
  mutate(QTY_acc = ifelse(QTY_pred < QTY, ((QTY_pred/QTY) * 100),((QTY/QTY_pred) * 100)))
data1_test

mean(data1_test$QTY_acc) 

data2_test = data2_test %>%
  mutate(QTY_acc = ifelse(QTY_pred < QTY, ((QTY_pred/QTY) * 100),((QTY/QTY_pred) * 100)))
data2_test

mean(data2_test$QTY_acc)  

