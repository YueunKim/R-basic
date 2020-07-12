setwd("C:/Users/rlagh/OneDrive/바탕 화면/김유은/R/R(AI)/R-basic")
getwd()

data = read.csv("sales_AI.csv")
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

shapiro.test(data1$QTY) # p-valued 값이 0.05보다 작으므로 정규분포를 따른다
shapiro.test(data2$QTY) # p-valued 값이 0.05보다 작으므로 정규분포를 따른다


data1 = data1[-c(3)] #???
cor(data1)

data2 = data2[-c(3)]
cor(data2)

### https://bioinformaticsandme.tistory.com/290

out1 = lm(QTY~ .,data=data1)
out2 = lm(QTY~ .,data=data2) # YM제외?? -YM

#out1 = lm(QTY~ X + ITEM_CNT + PRICE + MAXTEMP + SALEDAY, data=data1)
#out2 = lm(QTY~ X + ITEM_CNT + PRICE + MAXTEMP + RAIN_DAY + HOLIDAY, data=data2)

both1=step(out1,direction="both",trcce=FALSE) # step 함수를 사용해 기존 회귀모형에서 유의하지 않은 변수 제거
both2=step(out2,direction="both",trcce=FALSE)

#f-test분산분석으로 두 회귀모형의 설명력을 비교하여 첫번째 회귀모형에서 제거된 변수들의 기여도 평가

anova(out1,both1) # f-test결과 p-value값이 0.9095로 매우 크므로 앞서 제거된 변수가 회귀모형에 대한 기여도가 적음을 알 수 있음 
anova(out2,both2) # f-test결과 p-value값이 0.652로 매우 크므로 앞서 제거된 변수가 회귀모형에 대한 기여도가 적

# 최종 회귀모형 평가
summary(both1) # 최종 회귀모형이 예측변수들의 75.39%를 설명
summary(both2) # 최종 회귀모형이 예측변수들의 88.37%를 설명
# 결과 아래쪽의 F-statistic 결과의 p-value를 보면 둘다 0.05보다 작아 이 모델은 유의하게 사용할 수 있다고 판단 가능하다.

anova(both1)
anova(both2)

# 다중공선성 확인
# 다중공선성은 분산팽창지수(VIF)라는 통계량을 사용하여 계산 가능
# VIF가 10을 넘으면


install.packages('car')
library(car)
vif(both1)
vif(both2)




vif_func <- function(in_frame,thresh=10, trace=F,...){
  require(fmsb)
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init <- vector('list', length = ncol(in_frame))
  names(vif_init) <- names(in_frame)
  var_names <- names(in_frame)
  
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val,' ~ .'))
    vif_init[[val]] <- VIF(lm(form_in,data=in_frame,...))
  }
  vif_max<-max(unlist(vif_init))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('', times = nrow(vif_init) ),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    
    return(names(in_frame))
    
  }
  
  else{
    
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    
    while(vif_max >= thresh){
      vif_vals <- vector('list', length = ncol(in_dat))
      names(vif_vals) <- names(in_dat)
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val,' ~ .'))
        vif_add <- VIF(lm(form_in,data=in_dat,...))
        vif_vals[[val]] <- vif_add
        
      }
      
      max_row <- which.max(vif_vals)
      #max_row <- which( as.vector(vif_vals) == max(as.vector(vif_vals)) )
      
      vif_max<-vif_vals[max_row]
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        
        vif_vals <- do.call('rbind', vif_vals)
        
        vif_vals
        
        prmatrix(vif_vals,collab='vif',rowlab=row.names(vif_vals),quote=F)
        
        cat('\n')
        
        cat('removed: ', names(vif_max),unlist(vif_max),'\n\n')
        
        flush.console()
        
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% names(vif_max)]
      
    }
    
    return(names(in_dat))
    
  }
  
}

X_independent <- vif_func(both1, thresh=10, trace=T)







data1 = lm(QTY ~.,data = data1)
par(mfrow = c(2,2))
plot(data)

data2 = lm(QTY ~.,data = data2)
par(mfrow = c(2,2))
plot(data2)

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