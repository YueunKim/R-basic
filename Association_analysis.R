
########## 연관성 분석 ############

setwd("c:/Rdata")
getwd()


## 연관성 분석을 위한 분석환경과 데이터세트 준비

install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

data(package = "arules")
data(Groceries)
Groceries
inspect(Groceries[1:10]) # 희박형태로 저장되어서 데이터 내용을 확인하려면 inspect사용



## 데이터 세트 내에 포함된 각 아이템의 빈도통계 확인

summary(Groceries) # 가장 빈번하게 나타난 6개의 아이템 빈도가 나타남
sort(itemFrequency(Groceries, type = 'absolute'), decreasing = T) # 각 아이템의 빈도를 확인
sort(itemFrequency(Groceries, type = 'relative'), decreasing = T) # 상대빈도로 표현
par(mfrow = c(1,1))
itemFrequencyPlot(Groceries, topN=10, type = "absolute") #시각화
itemFrequencyPlot(Groceries, topN=10, type = "relative") #절대빈도로 시각화화


## 데이터 세트를 이용하여 본격적으로 연관성 분석 시행

apriori(Groceries)
9385*0.1/30 #support가 0.1이면 지지도가 너무 높음
9385*0.005/30

#support는 0.05, confidence는 0.5로,한개이상의 아이템만 포함한 규칙하을 배제하려고 최소길이 2
result_rules = apriori(Groceries, parameter = list(support=0.005, confidence = 0.5, minlen=2))
summary(result_rules)
inspect(result_rules[1:5])

rules_lift = sort(result_rules, by ="lift", decreasing = T) #향상도(lift)에 따라
inspect(rules_lift[1:5])
rules_conf = sort(result_rules, by ="confidence", decreasing = T) #신뢰도(confidence)에 따라
inspect(rules_conf[1:5])


## 연관선 분석 규칙 중 관심있는 아이템으로 부분집합하여 규칙보기
# 가장많은 거래가 이루어지는 whole milk 
milk_rule = subset(rules_lift, items %in% "whole milk")
milk_rule
inspect(milk_rule[1:5])

#결과(rhs)에서만 whole milk인 경우 찾기
rhs.milk_rule = subset(rules_lift, rhs %in% "whole milk")
rhs.milk_rule
inspect(rhs.milk_rule[1:3])

wholemilk_rule = apriori(Groceries, parameter = list(support=0.005, confidence = 0.5, minlen = 2),
                         appearance = list(default = "lhs", rhs = "whole milk"))
wholemilk_rule = sort(wholemilk_rule, by = "lift", decreasing = T)
inspect(wholemilk_rule[1:5])



## 연관성 분석 결과 시각화
plot(wholemilk_rule[1:10], method = "graph", measure = "lift", shading = "confidence") # 원의크기는 향상도, 색이 진할수록 신뢰도가 높음






