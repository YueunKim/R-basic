setwd("c:/Rdata")

View(attitude)
cov(attitude)
cor(attitude)

with(attitude,cor.test(rating, complaints))
cor.test(attitude$rating,attitude$complaints)
plot(attitude$rating,attitude$complaints)



####### lecture 8. 단순회귀분석 (Simple Linear Regression)

# •  하나의 종속변수와 하나의 설명변수 간의 관계를 직선으로 표현하는 방법 
# •  종속변수: 예측될 변수 
# •  설명변수 (독립변수): 종속변수를 예측하는데 활용될 변수


fa = c(150,160,170,180,190)
su=c(176,179,182,178,185)
fasu=data.frame(fa,su)
fasu
lm(su~fa,data=fasu) # 설명변수를 종속변수에 회귀분석

data = read.csv("cars.csv")
data
out = lm(dist~speed,data=data)
summary(out) # p값이 0.05보다 작으므로 speed가 dist에독립적이지 않으며 상관관계가 있음

plot(dist~speed,data=cars,col="blue") 
abline(out,col="red")
out1 = lm(dist~speed+0,data=data)
summary(lm(dist~speed+0,data=data)) # 논리적으로 맞지않아 intercept값이 사라짐 , r-squared값 올라감
plot(out1)

par(mfrow=c(2,2)) # 2x2로 그래프 출력
plot(out1)

# 잔차도, 정규성 검정
shapiro.test(data$dist) #p값이 0.05보다 작으므로 데이터를 가공해야함
shapiro.test(log(data$dist))
shapiro.test(sqrt(data$dist))

out3=lm(sqrt(dist)~speed+0,data=data)
summary(out3)
plot(out3)

out3$fitted.values # x값이 speed가 주어졌을때의 output
cbind(data$speed,out3$fitted.values)

# 추정과 예측
new = data.frame(speed=data$speed)
cbind(new$speed,predict(out3,new,interval = "confidence"))
cbind(new$speed,predict(out3,new,interval = "prediction"))





####### lecture 9. 다중회귀분석

# 종속변수 y가 독립변수 x1, x2, ...xp 및 오차항과 어떤관계가 있는지를 보여주는 식을 다중 회귀모형 이라고 한다.

data = read.csv("salary.csv")
head(data)
out = lm(salary~experience+score, data =data)
summary(out)
plot(out)
cbind(data$experience, data$score, out$fitted.values)

# 계수의  해석
# 경력 연수가 1년 증가할 때 연봉이 $1,404 증가할 것으로 기대된다 (직무적성검사 성적이 일정 하다고 할 때).
# 직무적성검사 성적이 1점 올라갈 때 연봉은 $251 올라갈  것으로 기대된다 (경력연수가 일정하다고 할 때).



# 결정계수  (coefficient of determination; R2)
# SST = 총제곱합 SSR = 회귀제곱합 SSE = 오차제곱합


# 다중공선성
summary(lm(rating~complaints+learning, data=attitude))
summary(lm(rating~learning,data=attitude))





### 모형선택 방법

# •   Forward selection 
# – 가장 유의한 변수부터 하나씩 추가 
# •   Backward selection 
# – 모든 변수를 넣고 가장 기여도가 낮은 것부터 하나씩 제거
# •   Stepwise selection 
# – Forward selection과 backward selection을 조합 
# •   All subsets 
# – 모든 가능한 모형 을 비교하여 최적의 모형 선택 
# – 여러 모형 중 최소 AIC, BIC, Mallow’s Cp 혹은 최대 adjusted R- sq를 갖는 모형을 선택



## Backward Selection
out=lm(rating~.,data=attitude)
summary(out)
# 가장 유의하지 않은 변수 critical 제거
out=lm(rating~.-critical,data=attitude)
summary(out)

backward=step(out,direction="backward",trace=FALSE)
summary(backward)


## Stepwise Selection
both=step(out,direction="both",trace=FALSE)
summary(both)


## All Subsets Regression

install.packages("leaps")
library(leaps)

leaps = regsubsets(rating~.,data=attitude,nbest = 5)
summary(leaps)
par(mfrow = c(1,1))
plot(leaps,scale='bic')

out_bic = glm(rating~complaints, data=attitude)
summary(out_bic)

plot(leaps,scale='cp')

out_cp = lm(rating~complaints+learning, data=attitude)
summary(out_cp)

plot(leaps, scale='adjr2')

out_adjr = lm(rating~complaints+learning+advance, data=attitude)
summary(out_adjr)

