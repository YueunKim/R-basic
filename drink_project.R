setwd("C:/Users/rlagh/OneDrive/바탕 화면/김유은/R/R(AI)/R-basic")
getwd()

data = read.csv("sales_AI_first.csv")
head(data)
str(data)


library(dplyr)

data1 = data %>% filter(CATEGORY == '비타민음료')
head(data1)

data2 = data %>% filter(CATEGORY == '스포츠,이온음료')
head(data2)


#정규성 검정
par(mfrow = c(1,1))
hist(data1$QTY) # 정규분포 x
hist(data2$QTY) # 정규분포 x

qqnorm(data1$QTY) #Q-Q plot상의 직선에서 점들이 크게 벗어나 있지 않는다면 QTY 변수는 정규 분포를 따른다고 볼 수 있다.
qqline(data1$QTY) #Q-Q plot상의 직선에서 점들이 크게 벗어나 있지 않는다면 QTY 변수는 정규 분포를 따른다고 볼 수 있다.
qqnorm(data2$QTY) #Q-Q plot상의 직선에서 점들이 크게 벗어나 있지 않는다면 QTY 변수는 정규 분포를 따른다고 볼 수 있다.
qqline(data2$QTY)

shapiro.test(data1$QTY) # p-valued 값이 0.05보다 작으므로 정규분포를 따른다
shapiro.test(data2$QTY) # p-valued 값이 0.05보다 작으므로 정규분포를 따른다


data1 = data1[-c(3)] #???
cor(data1)

data2 = data2[-c(3)]
cor(data2)

### https://bioinformaticsandme.tistory.com/290

out1 = lm(QTY~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY,data=data1)
out2 = lm(QTY~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY,data=data2) 

out1
out2

both1=step(out1,direction="both",trcce=FALSE) # step 함수를 사용해 기존 회귀모형에서 유의하지 않은 변수 제거
both2=step(out2,direction="both",trcce=FALSE)

both1
both2

#f-test분산분석으로 두 회귀모형의 설명력을 비교하여 첫번째 회귀모형에서 제거된 변수들의 기여도 평가

anova(out1,both1) # f-test결과 p-value값이 0.7997로 매우 크므로 앞서 제거된 변수가 회귀모형에 대한 기여도가 적음을 알 수 있음 
anova(out2,both2) # f-test결과 p-value값이 0.9637로 매우 크므로 앞서 제거된 변수가 회귀모형에 대한 기여도가 적

# 최종 회귀모형 평가
summary(both1) # 최종 회귀모형이 예측변수들의 70.54%를 설명
summary(both2) # 최종 회귀모형이 예측변수들의 85.1%를 설명
# 결과 아래쪽의 F-statistic 결과의 p-value를 보면 둘다 0.05보다 작아 이 모델은 유의하게 사용할 수 있다고 판단 가능하다.



# 다중공선성 확인
# 다중공선성은 분산팽창지수(VIF)라는 통계량을 사용하여 계산 가능
# VIF가 10을 넘지 않으므로 다중공선성 문제가 없음을 확인
install.packages('car')
library(car)
vif(both1)
vif(both2)




#data1 = lm(QTY ~.,data = data1)
#par(mfrow = c(2,2))
#plot(data)

#data2 = lm(QTY ~.,data = data2)
#par(mfrow = c(2,2))
#plot(data2)

install.packages("forecast")
library(forecast)

pred1 = data1 %>%
  mutate(pred_QTY1 = -1054+22.46*ITEM_CNT+0.6854*PRICE+8.875*MAXTEMP+0.006731*RAIN_DAY)%>%
  summarise(QTY,round(pred_QTY1))
pred1

pred2 = data2 %>%
  mutate(pred_QTY2 = 2328-3.122*PRICE+66.72*MAXTEMP+0.01273*SALEDAY+76.38*HOLIDAY)%>%
  summarise(QTY,round(pred_QTY2))
pred2


# simulation 
# https://macerayarislari.com/ko/300-examples/7-regression-analysis-in-excel.html

# https://m.blog.naver.com/windkiy/221770638020







install.packages("caret")
library(caret)

idx1 = sample(1:nrow(data1), size=nrow(data1)*0.7, replace = F)
data1_train = data1[idx1, ]
data1_test = data1[-idx1, ]

dim(data1_train)
dim(data1_test)
#train.idx1 = createDataPartition(data1$QTY, p=0.7, list=F)
#data1_train

idx2 = sample(1:nrow(data2), size=nrow(data2)*0.7, replace = F)
data2_train = data2[idx2, ]
data2_test = data2[-idx2, ]
dim(data2_train)
dim(data2_test)
#train.idx2 = createDataPartition(data2$QTY, p=0.7, list=F)
#data2_train



lm.fit1 = lm(PRICE ~ ITEM_CNT+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY, data=data1_train)
summary(lm.fit1)

lm.fit11 = step(lm.fit1, method="both")
summary(lm.fit11)

lm.yhat111 = predict(lm.fit11, newdata=data1_test)
lm.yhat111
k1=mean((lm.yhat111-data1_test$PRICE)^2)
sqrt(k1)
plot(lm.yhat111, data1_test$PRICE)
abline(a=0,b=1,col=2)


lm.fit2 = lm(PRICE ~ ITEM_CNT+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY, data=data2_train)
summary(lm.fit2)

lm.fit22 = step(lm.fit2, method="both")
summary(lm.fit22)
#조정r스퀘어와 f통계량이 원래의 결과값보다 증가하고 p-valued값이 0.05보다 작으므로
#이 회귀적합 결과를 이용해 평가데이터로 수치예측시행

lm.yhat222 = predict(lm.fit22, newdata=data2_test)
lm.yhat222
k2=mean((lm.yhat222-data2_test$PRICE)^2)
sqrt(k2)
plot(lm.yhat222, data2_test$PRICE)
abline(a=0,b=1,col=2)
