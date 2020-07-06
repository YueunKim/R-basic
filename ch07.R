
############################7장############################

## 결측치 정제하기

# NA : 값이 있어야 하는데 없을 경우 (결측치)
# NULL : 공란


getwd()
setwd("C:/Rdata")


# 결측치 만들기
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA)) # NA 앞 뒤에 겹따옴표 없음
df

is.na(df)         # 결측치 확인

table(is.na(df))  # 결측치 빈도 출력

table(is.na(df$sex))    # sex 결측치 빈도 출력
table(is.na(df$score))  # score 결측치 빈도 출력

mean(df$score)  # 평균 산출
sum(df$score)   # 합계 산출


# 결측치 있는 행 제거하기
library(dplyr) # dplyr 패키지 로드
df %>% filter(is.na(score))   # score가 NA인 데이터만 출력

df_nomiss <- df %>% filter(!is.na(score))  # score 결측치 제거

mean(df_nomiss$score)                      # score 평균 산출
sum(df_nomiss$score)                       # score 합계 산출


# 여러 변수 동시에 결측치 없는 데이터 추출하기
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex)) # score, sex 결측치 제외
df_nomiss  


# 결측치가 하나라도 있으면 제거하기
df_nomiss2 <- na.omit(df)  # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2                 


# 함수의 결측치 제외 기능 이용하기 - na.rm = T
mean(df$score, na.rm = T)  # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T)   # 결측치 제외하고 합계 산출



# summarise()에서 na.rm = T사용하기

# 결측치 생성
exam <- read.csv("csv_exam.csv")  # 데이터 불러오기
exam[c(3, 8, 15), "math"] <- NA   # 3, 8, 15행의 math에 NA 할당
head(exam)

# 평균 구하기
exam %>% summarise(mean_math = mean(math))  # 평균 산출

exam %>% summarise(mean_math = mean(math, na.rm = T))  # 결측치 제외하고 평균 산출

exam %>% summarise(mean_math = mean(math, na.rm = T),      # 평균 산출
                   sum_math = sum(math, na.rm = T),        # 합계 산출
                   median_math = median(math, na.rm = T))  # 중앙값 산출


## 결측치 대체법(Imputation)
# 대표값(평균, 최빈값 등)으로 일괄 대체
#	통계분석 기법 적용, 예측값 추정해서 대체

# 평균으로 대체하기
mean(exam$math, na.rm = T)  # 결측치 제외하고 math 평균 산출
exam$math <- ifelse(is.na(exam$math), 55, exam$math)  # math가 NA면 55로 대체
kk = table(is.na(exam$math))       # 결측치 빈도표 생성
kk
tt = barplot(kk,col=rainbow(2), ylim=c(0,20))
tt
text(tt,kk,labels = kk, pos=3)

exam
mean(exam$math)  # math 평균 산출



## 7-1 practice 

# 결측치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요.

library(ggplot2)
mpg=as.data.frame(ggplot2::mpg)
mpg
mpg[c(65,124,131,153,212), "hwy"]=NA
table(is.na(mpg$hwy))

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = round(mean(hwy,na.rm = T),1))
df_mpg

ggplot(data = df_mpg,aes(x=drv,y=mean_hwy))+geom_col()
# x축,y축 정해지고 geom_col이면 막대그래프
ggplot(data=mpg,aes(x=drv))+geom_bar()


# Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요.

table(is.na(mpg$drv))  # drv 결측치 빈도표 출력
table(is.na(mpg$hwy))  # hwy 결측치 빈도표 출력

# Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다.

mpg %>%
  filter(!is.na(hwy)) %>%          # 결측치 제외
  group_by(drv) %>%                # drv별 분리
  summarise(mean_hwy = mean(hwy))  # hwy 평균 구하기




## 이상치 정제하기

# 이상치 포함된 데이터 생성 - sex 3, score 6
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

table(outlier$sex)
table(outlier$score)


# 결측 처리하기 

# sex가 3이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

# sex가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

outlier %>%       # 결측치 제외하고 분석
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))





# 상자그림으로 극단치 기준 정해서 제거하기
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy, horizontal = T,col = 2)
summary(mpg$hwy)

IQR(mpg$hwy)
hist(mpg$hwy, probability = T)
lines(density(mpg$hwy),type = 'h', col=2)

boxplot(mpg$hwy)$stats  # 상자그림 통계치 출력

# 결측 처리하기
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy) # 12~37 벗어나면 NA 할당
kk = table(is.na(mpg$hwy))
tt = barplot(kk,col=rainbow(2),ylim = c(0,250))
text(tt,kk,labels = paste0(kk,"건"),col=2,cex=2,pos=3)


# 결측치 제외하고 분석하기
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))


## 7-2 practice 
# 우선 mpg 데이터를 불러와서 일부러 이상치를 만들겠습니다. drv(구동방식) 변수의 값은 4(사륜구동), f(전륜구동), r(후륜구동) 세 종류로 되어있습니다. 몇 개의 행에 존재할 수 없는 값 k를 할당하겠습니다. cty(도시 연비) 변수도 몇 개의 행에 극단적으로 크거나 작은 값을 할당하겠습니다.
# 이상치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요.
# 구동방식별로 도시 연비가 다른지 알아보려고 합니다. 분석을 하려면 우선 두 변수에 이상치가 있는지 확인하려고 합니다.

mpg <- as.data.frame(ggplot2::mpg)                  # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k"                # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)  # cty 이상치 할당

# Q1. drv에 이상치가 있는지 확인하세요. 이상치를 결측 처리한 다음 이상치가 사라졌는지 확인하세요. 결측 처리 할 때는 %in% 기호를 활용하세요.

table(mpg$drv) # 이상치 확인

mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA) # drv가 4, f, r이면 기존 값 유지, 그 외 NA할당

table(mpg$drv)


# Q2. 상자 그림을 이용해서 cty에 이상치가 있는지 확인하세요. 상자 그림의 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.

boxplot(mpg$cty)$stats # 상자 그림 생성 및 통계치 산출

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty) # 9~26 벗어나면 NA 할당

boxplot(mpg$cty)


# Q3. 두 변수의 이상치를 결측처리 했으니 이제 분석할 차례입니다. 이상치를 제외한 다음 drv별로 cty 평균이 어떻게 다른지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다.

mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%  # 결측치 제외
  group_by(drv) %>%                      # drv별 분리
  summarise(mean_hwy = mean(cty))        # cty 평균 구하기




## 정리하기

# 1.결측치 정제하기

# 결측치 확인
table(is.na(df$score))

# 결측치 제거
df_nomiss <- df %>% filter(!is.na(score))

# 여러 변수 동시에 결측치 제거
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))

# 함수의 결측치 제외 기능 이용하기
mean(df$score, na.rm = T)
exam %>% summarise(mean_math = mean(math, na.rm = T))



# 2.이상치 정제하기

# 이상치 확인
table(outlier$sex)

# 결측 처리
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)

# boxplot으로 극단치 기준 찾기
boxplot(mpg$hwy)$stats

# 극단치 결측 처리
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
