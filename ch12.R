
############################12장############################


### Multivariate analysis(다변량 분석)

## Star Chart

crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)

rownames(crime)

rownames(crime) = crime[, 1] #1열인 state를 열이름으로 
rownames(crime)

stars(crime,flip.labels = FALSE)
stars(crime[,2:8],flip.labels = FALSE, key.loc = c(15,2))



## 나이팅게일 차트
stars(crime[,2:8],flip.labels = FALSE, 
      draw.segments = TRUE, key.loc = c(15,2))


## 체르노프 페이스
install.packages("aplpack")
library(aplpack)
faces(crime[,2:8])


## 평행좌표플롯 (parallel coordinate plot)
education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)

library(lattice)
parallel(education[, 2:7], horizontal.axis = FALSE)
parallel(education[, 2:7], horizontal.axis = FALSE,col = 1)
summary(education$reading)

color=education$reading > 523
color
color+1
parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 
           + 1)
summary(education$dropout_rate)
color = education$dropout_rate > 5.3 #(3rd Qu)
color
color+1
parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 
           + 1)


## example : 2014년 한국 프로야구

data = read.csv("20140528_baseball.csv")
data

model = prcomp(data[, 2:6]) # 승률부터 평균자책까지
model

summary(model)
plot(model)
head(data)
rownames(data)=data[,1]
head(data)
model=prcomp(data[,2:6],scale=T)
biplot(model)
