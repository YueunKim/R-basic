
#################################### 연습문제 풀이

data = read.csv("flushot.csv")
head(data)



# 1. 주어진 세 개의 설명변수로 예방접종 확률을 예측하는 모형을 추정하여 추정된 로지스틱 회귀식을 써라.

log_model = glm(flushot~., data=data, family = binomial(logit))
summary(log_model)


# 2. Exp(b_1),Exp(b_2 ),Exp(b_3)를 해석하라. 

    # b_1 = 0.07279, b_2 = -0.09899, b_3 = 0.43397
exp(0.07279)
exp(-0.09899) #자각이 1 증가할때마다 약 0.9%가 떨어진다
exp(0.43397) #성별이 다를수록 약 54%의 



# 3. 55세의 건강상태에 대한 자각 인덱스가 60인 남성이 예방접종을 받을 확률은?


# 4. 유의하지 않은 설명변수가 있는지 Deviance goodness-of-fit test를 통해 판단하여 최종모형을 추정하라.


# 5. Wald test를 통해 성별이 유의한지 판단하라. 


# 6. Deviance Goodness-of-fit test를 통해 성별을 모형에서 제거해도 좋을지 판단하라.

log_model2 = glm(flushot~age+aware, data=data, family = binomial(logit))
summary(log_model2)
table(data$flushot)

24/(134+24)
tt = table(data$flushot, log_model2$fitted.values>0.1518987)
c('민감도'=19/(5+19), '특이도' = 95/(95+40), '에러율' = (40+5)/sum(tt))
rocplot(log_model2)





# 7. Cutoff를 0.1,0.15,0.2로 두었을 때의 총 error rate과 민감도, 특이도를 계산하라.

tab_01 = table(data$flushot, log_model2$fitted.values>0.1)
tab_015 = table(data$flushot, log_model2$fitted.values>0.15)
tab_02 = table(data$flushot, log_model2$fitted.values>0.2)

tab_01
tab_015
tab_02

res01 = c('민감도' = tab_01[2,2]/sum(tab_01[2,]),
          '특이도' = tab_01[1,1]/sum(tab_01[1,]), 
          '에러율' = (tab_01[1,2]/tab_01[2,1])/sum(tab_01))
res01

res015 = c('민감도' = tab_015[2,2]/sum(tab_015[2,]),
          '특이도' = tab_015[1,1]/sum(tab_015[1,]), 
          '에러율' = (tab_015[1,2]/tab_015[2,1])/sum(tab_015))
res015

res02 = c('민감도' = tab_02[2,2]/sum(tab_02[2,]),
          '특이도' = tab_02[1,1]/sum(tab_02[1,]), 
          '에러율' = (tab_02[1,2]/tab_02[2,1])/sum(tab_02))
res02





# 8. 총 error rate을 최소화 시키는 cutoff는 무엇인가? ROC curve 구하라.

res02






model4$fitted.values

kim = function(){
  k=seq(0.01,0.5,0.01)
  
  n=length(k)
  
  err_min=vector(length = n)
  sens = vector(length = n)
  spec=vector(length = n)
  
  for(i in 1:n){
    tab = table(data$flushot,log_model2$fitted.values > k[i])
    res = c(민감도 = tab[1,1]/sum(tab[2,]),
               특이도 = tab[1,1]/sum(tab[1,]),
               에러율 = (tab[1,2]+tab[2,1])/sum(tab))
    err_min[i]=tab[2,1]/sum(tab[2,])
    spec[i] = tab[1,1]/sum(tab[1,])
    print(res)
  }
  print(err_min)
  print(paste("최소의 error rate =", min(err_min),"이다"))
  index=which(err_min<=min(err_min))
  print(index)
  print(paste("해당하는 민감도=", sens[min(index)], "이다"))
  print(paste("해당하는 특이도=", spec[min(index)],"이다"))
  print(paste("해당하는 에러율 = ", err_min[min(index)], "이다"))
  print(paste("해당하는 cutoff = ",k[min(index)], "이다"))
  
  plot(1-spec, sens, col=2)
}
kim()

