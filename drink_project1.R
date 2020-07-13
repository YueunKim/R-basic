setwd("C:/Rdata")
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


data1 = data1[-c(3)]
cor(data1)

data2 = data2[-c(3)]
cor(data2)

### https://bioinformaticsandme.tistory.com/290

out1 = lm(QTY~ .,data=data1)
out2 = lm(QTY~ .,data=data2) 

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
  mutate(pred_QTY1 = -2212-29.59*X+52.28*ITEM_CNT+0.9627*PRICE+9.764*MAXTEMP+0.007023*SALEDAY+0.005063*RAIN_DAY)%>%
  summarise(QTY,round(pred_QTY1))
pred1

pred2 = data2 %>%
  mutate(pred_QTY2 = -1069-37.10*X+5.350*YM-3.296*PRICE+70.31*MAXTEMP-0.01059*SALEDAY+63.56*HOLIDAY)%>%
  summarise(QTY,round(pred_QTY2))
pred2


# simulation 
# https://macerayarislari.com/ko/300-examples/7-regression-analysis-in-excel.html

# https://m.blog.naver.com/windkiy/221770638020