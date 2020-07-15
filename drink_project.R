#setwd("C:/Users/rlagh/OneDrive/바탕 화면/김유은/R/R(AI)/R-basic")
setwd("c:/Rdata")
getwd()

#.libPaths()
#.libPaths("c:/Rdata/Library")

install.packages("dplyr")
install.packages('car')
install.packages("forecast")
install.packages("psych")
library(forecast)
library(car)
library(dplyr)

# https://woosa7.github.io/R-%ED%86%B5%EA%B3%84%EB%B6%84%EC%84%9D-%EB%8B%A4%EC%A4%91%ED%9A%8C%EA%B7%80%EB%B6%84%EC%84%9D/


data = read.csv("sales_AI_first1.csv")
head(data)
str(data)

##데이터 추출
data1 = data %>% filter(CATEGORY == '비타민음료')
head(data1)

data2 = data %>% filter(CATEGORY == '스포츠,이온음료')
head(data2)



##정규성 검정
par(mfrow = c(1,1))
#hist(data1$QTY) # 정규분포 x
#hist(data2$QTY) # 정규분포 x

qqnorm(data1$QTY) #Q-Q plot상의 직선에서 점들이 크게 벗어나 있지 않는다면 QTY 변수는 정규 분포를 따른다고 볼 수 있다.
qqline(data1$QTY) 
qqnorm(data2$QTY)
qqline(data2$QTY)

shapiro.test(data1$QTY) # p-valued 값이 0.05보다 작으므로 정규분포를 따른다
shapiro.test(data2$QTY) # p-valued 값이 0.05보다 작으므로 정규분포를 따른다


##상관관계
data1 = data1[-c(3)]
cor(data1)

data2 = data2[-c(3)]
cor(data2)

### https://bioinformaticsandme.tistory.com/290


##회귀모형 생성

#out1 = lm(QTY~., data=data1)
#out2 = lm(QTY~., data=data2)

out1 = lm(QTY~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY,data=data1)
out2 = lm(QTY~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY,data=data2) 

summary(out1)
summary(out2)

# step 함수를 사용해 기존 회귀모형에서 유의하지 않은 변수 제거
# 변수선택 방법은 forward, backward, stepwise 세가지
# forward 결정계수 (both1=0.7164, both2=0.8677)
# backward 결정계수 (both1=0.7261, both2=0.8705)
# stepwise 결정계수 (both1=0.7261, both2=0.8705)
both1=step(out1,direction="both",trace=FALSE)
both2=step(out2,direction="both",trace=FALSE)

summary(both1)
summary(both2)
# 결과 아래쪽의 F-statistic 결과의 p-value를 보면 둘다 0.05보다 작아 이 모델은 유의하게 사용할 수 있다고 판단 가능하다.


##f-test분산분석으로 두 회귀모형의 설명력을 비교하여 제거된 변수들의 기여도 평가
anova(out1,both1)
anova(out2,both2) 
# f-test결과 p-value값이 0.7997와 0.9637로 매우 크므로 앞서 제거된 변수가 회귀모형에 대한 기여도가 적음을 알 수 있음 



# 다중공선성 확인
# 다중공선성은 분산팽창지수(VIF)라는 통계량을 사용하여 계산 가능
# VIF가 10을 넘지 않으므로 다중공선성 문제가 없음을 확인
library(car)
a= vif(both1)
b= vif(both2)
sqrt(a)
sqrt(b)
library(psych)
pairs.panels(data1[names(data1)])

par(mfrow = c(2,2))
plot(both1)
plot(both2)
#눈에 띄는 이상치가 몇개보임


####이상치 지우고 다시 회귀 

#fit1 = lm(QTY ~ X + YM + ITEM_CNT + PRICE + MAXTEMP + SALEDAY + RAIN_DAY + 
#            HOLIDAY, data = data1[-c(22,26,53),])
#summary(fit1) #이상치 제거후 결정계수가 0.7699으로 상승

#fit2 = lm(QTY ~ X + YM + PRICE + MAXTEMP + SALEDAY + HOLIDAY, data = data2[-c(41,39,56),])
#summary(fit2) #이상치 제거후 결정계수가 0.9251으로 상승


fit1 = lm(QTY ~ PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY, data = data1[-c(22,26,53),])
summary(fit1) #이상치 제거후 결정계수가

fit2 = lm(QTY ~ ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY, data = data2, data = data2[-c(41,56),])
summary(fit2) #이상치 제거후 결정계수가

fit11=step(fit1,direction="both",trace=FALSE)
fit22=step(fit2,direction="both",trace=FALSE)

summary(fit11)
summary(fit22)


a= vif(fit11)
b= vif(fit22)
sqrt(a)
sqrt(b)





#pred1 = data1 %>%
#  mutate(pred_QTY1 = -1714+1.193*PRICE+8.825*MAXTEMP+0.007430*RAIN_DAY)%>%
#  summarise(QTY,pred_QTY1)
#pred1

#pred2 = data2 %>%
#  mutate(pred_QTY2 = 2922-3.284*PRICE+61.60*MAXTEMP+0.01221*SALEDAY+56.16*HOLIDAY)%>%
#  summarise(QTY,pred_QTY2)
#pred2



pred1 = data1 %>%
  mutate(pred_QTY1 = -1497+0.08315*PRICE+8.917*MAXTEMP+0.002189*SALEDAY+0.007381*RAIN_DAY)%>%
  summarise(QTY,pred_QTY1)
pred1

pred2 = data2 %>%
  mutate(pred_QTY2 = -1149-42.24*X+5.751*YM-3.296*PRICE+70.58*MAXTEMP+0.01158*SALEDAY)%>%
  summarise(QTY,pred_QTY2)
pred2


#pred1 = data1 %>%
#  mutate(pred_QTY1 = -1495+7.941*ITEM_CNT+1.026*PRICE+7.884*MAXTEMP+0.007359*RAIN_DAY)%>%
#  summarise(QTY,pred_QTY1)
#pred1

#pred2 = data2 %>%
#  mutate(pred_QTY2 = 2747-3.204*PRICE+62.27*MAXTEMP+0.01181*SALEDAY+67.03*HOLIDAY)%>%
#  summarise(QTY,pred_QTY2)
#pred2



#pred1 = data1 %>%
#  mutate(pred_QTY1 = -1054+22.46*ITEM_CNT+0.6854*PRICE+8.875*MAXTEMP+0.006731*RAIN_DAY)%>%
#  summarise(QTY,pred_QTY1)
#pred1

#pred2 = data2 %>%
#  mutate(pred_QTY2 = 2328-3.122*PRICE+66.72*MAXTEMP+0.01273*SALEDAY+76.38*HOLIDAY)%>%
#  summarise(QTY,pred_QTY2)
#pred2


drink1 = merge(data1, pred1, by = 'QTY')
drink2 = merge(data2, pred2, by = 'QTY')
head(drink1)
head(drink2)

drink1 = drink1 %>%
  mutate(accuracy1 = ifelse( round(pred_QTY1) < QTY, ((round(pred_QTY1)/QTY) * 100),(QTY/(round(pred_QTY1))) * 100))
drink1
           
drink2 = drink2 %>%
  mutate(accuracy2 = ifelse( round(pred_QTY2) < QTY, ((round(pred_QTY2)/QTY) * 100),(QTY/(round(pred_QTY2))) * 100))
drink2  
  
  
mean(drink1$accuracy1)  
mean(drink2$accuracy2)    
  
  
  
  






# ----------------------------TEST, TRAIN

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



lm.fit1 = lm(QTY ~ PRICE+ITEM_CNT+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY, data=data1_train)
summary(lm.fit1)

lm.fit11 = step(lm.fit1, method="both")
summary(lm.fit11)

lm.yhat111 = predict(lm.fit11, newdata=dataa)
lm.yhat111
a = data.frame(lm.yhat111)
head(a)
str(a)
qty=c(13,17,22,23,25,26,32,35,40,42,43,44,46,48,50,52,56,58,59)
aa = data.frame(qty,a)
aa

aaa = merge(data1, a, by = 'QTY')



k1=mean((lm.yhat111-data1_test$PRICE)^2)
sqrt(k1)
plot(lm.yhat111, data1_test$PRICE)
abline(a=0,b=1,col=2)


lm.fit2 = lm(QTY ~ PRICE+ITEM_CNT+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY, data=data2_train)
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
