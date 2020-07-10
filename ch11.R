
############################11장############################



## example : programming experience

setwd("c:/Rdata")
getwd()

data=read.csv("programming.csv")
head(data)

model = glm(Success ~ Experience, data=data, family = binomial(logit)) # glm 을통해 로지스틱회귀모형을 fitting
summary(model)

cbind(data$Experience, model$fitted.values)
plot(Success ~ Experience, data = data)
points(model$fitted.values ~ data$Experience, col=2) 
# 경력 14년차인 프로그래머가 특정 작업을 마칠 확률은 0.310

table(data$Success, model$fitted.values>0.5) # 임계값을 0.5로 설정, 0.5보다 큰 것은 1, 작은 것은 0
c('민감도' = 8/11, '특이도' = 11/14)






## example : coupon effectiveness

data = read.csv("coupon.csv")
head(data)
model2 = glm(cbind(N_redeemed, N-N_redeemed)~Price_reduc, data=data,family=binomial(logit))
summary(model2)
exp(0.096834)  
# 쿠폰 할인액이 1달러 증가할때 쿠폰 사용할 odds가 10% 증가 







## example : disease outbreak

data = read.csv("disease.csv")
head(data)
model3 = glm(disease~., data=data, family = binomial(logit))
summary(model3)
# 사회경제적 위치와 지역이 주어져 있을 때 나이가 1살 많아지면 특정 증상을 가질 Odds는 3% 증가한다. 
# 사회경제적 위치와 나이가 주어져 있을 때 sector 2 지역 주민의 Odds는약5배 sector 1 주민에 비해 약 5배크다.



## 모형비교: Deviance Goodness‐of-fit test

model4 = glm(disease~ age+sector, data=data, family = binomial(logit))
summary(model4)

# Reduced Model과 Full Model의 차이가 유의한지 검정 
# 여러 설명변수가 주는 영향이 유의한지 한번에 검정 
# 다중회귀분석의 F‐test와유사
anova(model3,model4, test='Chisq')

table(data$disease)
31/98 # 임계값

kk = table(data$disease, model4$fitted.values>0.3163265)
sum(kk)

#민감도 (Sensitivity): Tru e를 Tru e로 구분한 비율 = 23/31=0.74 
#특이도 (Specificity): False를 False로 구분한 비율 = 47/67=0.70
reduce_M = c('민감도'=23/31, '특이도'=47/(47+20))
kk1 = table(data$disease, model3$fitted.values>0.3163265)
kk1

fulmode_M = c('민감도' =23/31, '특이도'=49/(49+18))
reduce_M
fulmode_M

err_ml = 28/sum(kk)
err_m2 = 26/sum(kk1)
err_ml
err_m2



## ROC (Receiver Operating Characteristic) Curve

install.packages("Deducer")
library(Deducer)
rocplot(model3)
rocplot(model4)
# ROC curve의 아래쪽 면적(AUC)이 클수록 좋은 모형
