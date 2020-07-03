
############################3장############################

x = rnorm(100,175,2) # 100개의 데이터, 175는 평균, 2는 표준편차
x
hist(x, breaks = 5, probability = T)
lines(density(x), col=2, type = 'h', lwd=0.5)
shapiro.test(x)

# 변수 만들고 연산
a <- 1
a
b=2
b
a+b


# 여러 값으로 구성된 변수 만들기
var1 <- c(1,2,6,7,8)
var1

var2 <- c(1:5)
var2

var3 <- seq(1,5) # 1~5까지 연속값으로
var3

var4 <- seq(1,10,by=2) # 1~10까지 2 간격 연속값
var4

var5 <- seq(1,10,by=3) # 1~10까지 3 간격 연속값
var5

var1+2

str1 <- "a"
str1

str2 <- "text"
str2

str3 <- "Hello World!"
str3

str4 <- c("a", "b", "c")
str4

str5 <- c("i","am","a girl", sep="")
str5


## 함수
x <- c(1, 4, 7)
x

mean(x)
max(x)
min(x)
sd(x) # 표준편차

paste(str5, collapse = ",") # 쉼표를 구분자로 str5의 단어들 하나로 합치기
paste(str5, collapse = " ")


# ggplot2 패키지 설치하기, 로드하기
install.packages("ggplot2")  # ggplot2 패키지 설치
library(ggplot2)             # ggplot2 패키지 로드

x <- c("a", "a", "b", "c")
x

qplot(x) # 빈도 그래프 
qplot(data = mpg, x = hwy) # data에 mpg, x축에 hwy 변수 지정하여 그래프 생성
qplot(data = mpg, x = cty)
qplot(data = mpg, x = drv, y = hwy)
qplot(data = mpg, x = drv, y = hwy, geom = "line")      
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)


# 함수의 기능이 궁금할 땐 Help 함수를 활용
?qplot


## practice

# Q1. 시험 점수 변수 만들고 출력하기
score <- c(80, 60, 70, 50, 90)
score

# Q2. 전체 평균 구하기
mean(score)

# Q3. 전체 평균 변수 만들고 출력하기
mean_score <- mean(score) 
mean_score



############################4장############################


# 데이터 프레임 만들기

english <- c(90, 80, 60, 70)  # 영어 점수 변수 생성
english

math <- c(50, 60, 100, 20)    # 수학 점수 변수 생성
math

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

mean(df_midterm$english)  # df_midterm의 english로 평균 산출
mean(df_midterm$math)     # df_midterm의 math로 평균 산술
sd(df_midterm$english)
var(df_midterm$english)
sqrt(var(df_midterm$english)) # sqrt는 루트

# 데이터 프레임 한 번에 만들기
df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm


## practice

# Q1. data.frame()과 c()를 조합해서 표의 내용을 데이터 프레임으로 만들어 출력
sales <- data.frame(fruit = c("사과", "딸기", "수박"),
                    price = c(1800, 1500, 3000),
                    volume = c(24, 38, 13))
sales

# Q2. 앞에서 만든 데이터 프레임을 이용해서 과일 가격 평균, 판매량 평균
mean(sales$price)   # 가격 평균
mean(sales$volume)  # 판매량 평균


## 외부 데이터 이용하기 

# readxl 패키지 설치
install.packages("readxl")

# readxl 패키지 로드
library(readxl)


## 저장공간 설정
setwd("c:/Rdata")
getwd()


# csv 파일
df_midterm
write.csv(df_midterm, "df_midterm.csv") # Rdata 파일에 저장 됨 

df_mid_test <- read.csv("df_midterm.csv") # csv 파일 불러오기
df_mid_test



############################5장############################


x = rnorm(100,175,2)
hist(x, breaks = 5, probability = T)
lines(density(x), col=2, type = 'h', lwd=0.5)
shapiro.test(x)

plot.new()
hist(mpg$hwy,probability = T)
lines(density(mpg$hwy),col = 2, type = 'h', lwd = 1)
shapiro.test(mpg$hwy)

exam <- read.csv("csv_exam.csv")
exam

head(exam)      # 앞에서부터 6행까지 출력
head(exam, 10)  # 앞에서부터 10행까지 출력

tail(exam)      # 뒤에서부터 6행까지 출력
tail(exam, 10)  # 뒤에서부터 10행까지 출력

View(exam) # 데이터 전체 확인

dim(exam)  # 행,열 출력

str(exam)  # 데이터 속성 확인

summary(exam)  # 요약통계량 출력

boxplot(exam$math, horizontal = T, col=2)

hist(exam$math)

x <- sample(0:100, 80, replace = T) # 80개를 가져오는데 반복 가능
plot(x,pch = ifelse(x>=60, 7, 15), col = ifelse(x>=60, 2, 5))
abline(h=60, col=2, lwd=2)

# ggplo2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)

head(mpg)    # Raw 데이터 앞부분 확인
tail(mpg)    
View(mpg)


# 데이터 수정하기 - 변수명 바꾸기
install.packages("dplyr")  # dplyr 설치
library(dplyr)             # dplyr 로드

df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw

df_new <- df_raw  # 복사본 생성
df_new            # 출력

# rename()에 '새 변수명 = 기존 변수명' 순서로 입력
df_new <- rename(df_new, v2 = var2)  # var2를 v2로 수정
df_new 

df_raw
df_new


## practice

# Q1. ggplot2 패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본 만들기 
mpg <- as.data.frame(ggplot2::mpg)     

# Q2. 복사본 데이터를 이용해서 cty는 city로, hwy는 highway로 변수명을 수정
mpg_new <- rename(mpg_new, city = cty)    
mpg_new <- rename(mpg_new, highway = hwy)  

# Q3. 데이터 일부를 출력해서 변수명이 바뀌었는지 확인
head(mpg_new)                              

write.csv(mpg_new, "mpg_new.csv")


# 변수 조합해 파생변수 만들기

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2  # var_sum 파생변수 생성
df

df$var_mean <- (df$var1 + df$var2)/2  # var_mean 파생변수 생성
df

mpg$total <- (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성

head(mpg)

boxplot(mpg$total, horizontal = T)


## 조건문을 활용해 파생변수 만들기

# 1.기준값 정하기
summary(mpg$total)  # 요약 통계량 산출
hist(mpg$total)     # 히스토그램 생성

# 2. 조건문으로 합격 판정 변수 만들기
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") # 20 이상이면 pass, 그렇지 않으면 fail 부여
head(mpg) # 데이터 확인

# 3. 빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test)  # 연비 합격 빈도표 생성

# 4. 막대 그래프 빈도 표현하기
library(ggplot2)  # ggplot2 로드
qplot(mpg$test)   # 연비 합격 빈도 막대 그래프 생성


## 중첩 조건문 활용하기 - 연비 등급 변수 만들기

# total을 기준으로 A, B, C 등급 부여
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))

head(mpg, 20)  # 데이터 확인
table(mpg$grade) # 등급 빈도표 생성
qplot(mpg$grade) # 등급 빈도 막대 그래프 생성

# A, B, C, D 등급 부여
mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))


## practice

# 문제1. ggplot2의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악하세요.
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)

# 문제2. poptotal(전체 인구)을 total로, popasian(아시아 인구)을 asian으로 변수명을 수정하세요.
library(dplyr)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)


# 문제3. total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세
midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)


# 문제4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수를 만들어 보세요.
mean(midwest$ratio)
midwest$group <- ifelse(midwest$ratio > mean(midwest$ratio), "large", "small")
head(midwest)

# 문제5. "large"와 "small"에 해당하는 지역이 얼마나 되는지, 빈도표와 빈도 막대 그래프를 만들어 확인해 보세요.
table(midwest$group)
library(ggplot2)
qplot(midwest$group)



############################6장############################


## 조건에 맞는 데이터만 추출하기

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

# exam에서 class가 1인 경우만 추출하여 출력
class1 <- exam %>% filter(class == 1)
class1

# 2반인 경우만 추출
class23 <- exam %>% filter(class == 2 | class == 3)  # |는 or
class23

# 3반이 아닌 경우
classno3 <- exam %>% filter(class != 3)
classno3

# 1반 이면서 수학 점수가 50점 이상인 경우
classmath <- exam %>% filter(class == 1 & math >= 50)
classmath

# 수학 점수가 90점 이상이거나 영어점수가 90점 이상인 경우
classA <- exam %>% filter(math >= 90 | english >= 90)
classA

exam %>% filter(class == 1 | class == 3 | class == 5)  # 1, 3, 5 반에 해당되면 추출
exam %>% filter(class %in% c(1,3,5))  # 1, 3, 5 반에 해당하면 추출


class1 <- exam %>% filter(class == 1)  # class가 1인 행 추출, class1에 할당
class2 <- exam %>% filter(class == 2)  # class가 2인 행 추출, class2에 할당
mean(class1$math) 
mean(class2$math)


## practice

# Q1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. displ(배기량)이 4 이하인 자동차와 5 이상인 자동차 중 어떤 자동차의 hwy(고속도로 연비)가 평균적으로 더 높은지 알아보세요.
mpg <- as.data.frame(ggplot2::mpg)   # mpg 데이터 불러오기

mpg_a <- mpg %>% filter(displ <= 4)  # displ 4 이하 추출
mpg_b <- mpg %>% filter(displ >= 5)  # displ 5 이상 추출

a <- mean(mpg_a$hwy)  # displ 4 이하 hwy 평균
b <- mean(mpg_b$hwy)  # displ 5 이상 hwy 평균

mpg$mean_a <- a
mpg$mean_b <- b


# Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다. "audi"와 "toyota" 중 어느 manufacturer(자동차 제조 회사)의 cty(도시 연비)가 평균적으로 더 높은지 알아보세요.
mpg_audi <- mpg %>% filter(manufacturer == "audi")      # audi 추출
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")  # toyota 추출

au <- mean(mpg_audi$cty)    # audi의 cty 평균
to <- mean(mpg_toyota$cty)  # toyota의 cty 평균

mpg$mean_au <- au
mpg$mean_to <- to


#	Q3. "chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 알아보려고 합니다. 이 회사들의 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요.

# manufacturer가 chevrolet, ford, honda에 해당하면 추출
mpg_new <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
h <- mean(mpg_new$hwy)

mpg$mean_hwy <- h

write.csv(mpg_new, "mpg_new1.csv")



exam %>% select(english)  # english 추출
exam %>% select(class, math, english)  # class, math, english 변수 추출
exam %>% select(-math)  # math 제외

# class가 1인 행만 추출한 다음 english 추출
exam %>% filter(class == 1) %>% select(english)

# 가독성 있게 줄 바꾸기
exam %>%
  filter(class == 1) %>%  # class가 1인 행 추출
  select(english)         # english 추출

# 일부만 출력하기
exam %>%
  select(id, math) %>%  # id, math 추출
  head                  # 앞부분 6행까지 추출


## practice

# Q1. mpg 데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해서 분석에 활용하려고 합니다. mpg 데이터에서 class(자동차 종류), cty(도시 연비) 변수를 추출해 새로운 데이터를 만드세요. 새로 만든 데이터의 일부를 출력해서 두 변수로만 구성되어 있는지 확인하세요.
mpg <- as.data.frame(ggplot2::mpg)  # mpg 데이터 불러오기

df <- mpg %>% select(class, cty)    # class, cty 변수 추출
head(df)                            # df 일부 출력

# Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한 데이터를 이용해서 class(자동차 종류)가 "suv"인 자동차와 "compact"인 자동차 중 어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.
df_suv <- df %>% filter(class == "suv")          # class가 suv인 행 추출
df_compact <- df %>% filter(class == "compact")  # class가 compact인 행 추출

mean(df_suv$cty)                                 # suv의 cty 평균
mean(df_compact$cty)                             # compact의 cty 평균




# 오름차순으로 정렬하기
exam %>% arrange(math)  # math 오름차순 정렬

# 내림차순으로 정렬하기
exam %>% arrange(desc(math))  # math 내림차순 정렬

# 정렬 기준 변수 여러개 지정
exam %>% arrange(class, math)  # class 및 math 오름차순 정렬



## practice

# "audi"에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가 높은지 알아보려고 합니다. "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차의 데이터를 출력하세요.
mpg <- as.data.frame(ggplot2::mpg)          # mpg 데이터 불러오기

mpg %>% filter(manufacturer == "audi") %>%  # audi 추출
  arrange(desc(hwy)) %>%                    # hwy 내림차순 정렬
  head(5)                                   # 5행까지 출력



## 파생변수 추가하기

exam %>%
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  head                                          # 일부 추출

# 여러 파생변수 한 번에 추가하기
exam %>%
  mutate(total = math + english + science,          # 총합 변수 추가
         mean = (math + english + science)/3) %>%   # 총평균 변수 추가
  head                                              # 일부 추출

# mutate()에 ifelse() 적용하기
exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head

# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  arrange(total) %>%                            # 총합 변수 기준 정렬
  head                                          # 일부 추출


## practice

# Q1. mpg 데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.

mpg <- as.data.frame(ggplot2::mpg)                # mpg 데이터 불러오기
mpg_new <- mpg                                    # 복사본 만들기

mpg_new <- mpg_new %>% mutate(total = cty + hwy)  # 합산 변수 만들기


# Q2. 앞에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가세요.
mpg_new <- mpg_new %>% mutate(mean = total/2)     # 평균 변수 만들기


# Q3. '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.
mpg_new %>%
  arrange(desc(mean)) %>%  # 내림차순 정렬
  head(3)                  # 상위 3행 출력


# Q4. 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요. 데이터는 복사본 대신 mpg 원본을 이용하세요.
mpg %>%
  mutate(total = cty + hwy,   # 합산 변수 만들기
         mean = total/2) %>%  # 평균 변수 만들기
  arrange(desc(mean)) %>%     # 내림차순 정렬
  head(3)                     # 상위 3행 출력



## 요약하기
exam %>% summarise(mean_math = mean(math))  # math 평균 산출

# 집단별로 요약하기
exam %>%
  group_by(class) %>%                # class별로 분리
  summarise(mean_math = mean(math))  # math 평균 산출

# 여러 요약통계량 한 번에 산출하기
exam %>%
  group_by(class) %>%                   # class별로 분리
  summarise(mean_english = mean(english),     # math 평균
            sum_english = sum(english),       # math 합계
            median_english = median(english), # math 중앙값
            student_number = n())             # 학생 수

# 각 집단별로 다시 집단 나누기
mpg <- as.data.frame(ggplot2::mpg)

mpg %>%
  group_by(manufacturer, drv) %>%      # 회사별, 구방방식별 분리
  summarise(mean_cty = mean(cty)) %>%  # cty 평균 산출
  arrange(desc(mean_cty))
  head(10)                             # 일부 출력

  
## dplyr 조합하기

# 회사별로 "suv" 자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고, 1~5위까지 출력하기

mpg %>%
  group_by(manufacturer) %>%           # 회사별로 분리
  filter(class == "suv") %>%           # suv 추출
  mutate(tot = (cty+hwy)/2) %>%        # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>%  # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>%          # 내림차순 정렬
  head(5)                              # 1~5위까지 출력


## practice

#	Q1. mpg 데이터의 class는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로 분류한 변수입니다. 어떤 차종의 연비가 높은지 비교해보려고 합니다. class별 cty 평균을 구해보세요.
mpg <- as.data.frame(ggplot2::mpg)  # mpg 데이터 불러오기

mpg %>%
  group_by(class) %>%               # class별 분리
  summarise(mean_cty = mean(cty))   # cty 평균 구하기



#	Q2. 앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의 도시 연비가 높은지 쉽게 알아볼 수 있도록 cty 평균이 높은 순으로 정렬해 출력하세요.
mpg %>%
  group_by(class) %>%                  # class별 분리
  summarise(mean_cty = mean(cty)) %>%  # cty 평균 구하기
  arrange(desc(mean_cty))              # 내림차순 정렬하기



#	Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다. hwy 평균이 가장 높은 회사 세 곳을 출력하세요.
mpg %>%
  group_by(manufacturer) %>%           # manufacturer별 분리
  summarise(mean_hwy = mean(hwy)) %>%  # hwy 평균 구하기
  arrange(desc(mean_hwy)) %>%          # 내림차순 정렬하기
  head(3)                              # 상위 3행 출력


# Q4. 어떤 회사에서 "compact"(경차) 차종을 가장 많이 생산하는지 알아보려고 합니다. 각 회사별 "compact" 차종 수를 내림차순으로 정렬해 출력하세요.
mpg %>%
  filter(class == "compact") %>%  # compact 추출
  group_by(manufacturer) %>%      # manufacturer별 분리
  summarise(count = n()) %>%      # 빈도 구하기
  arrange(desc(count))            # 내림차순 정렬



## 데이터 합치기

# 가로로 합치기

# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
test1

# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test2 

# id 기준으로 합치기
total <- left_join(test1, test2, by = "id")  # id 기준으로 합쳐 total에 할당
total                                        # total 출력

# 다른 데이터 활용해 변수 추가하기

#반별 담임교사 명단 생성
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

# class 기준 합치기
exam_new <- left_join(exam, name, by = "class")
exam_new



# 세로로 합치기

# 학생 1~5번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_a  

# 학생 6~10번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_b  

# 세로로 합치기
group_all <- bind_rows(group_a, group_b)  # 데이터 합쳐서 group_all에 할당
group_all                                 # group_all 출력




## practice

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel


# Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 연료 가격을 나타낸 변수는 없습니다. 위에서 만든 fuel 데이터를 이용해서 mpg 데이터에 price_fl(연료 가격) 변수를 추가하세요.

mpg <- as.data.frame(ggplot2::mpg)      # mpg 데이터 불러오기
mpg <- left_join(mpg, fuel, by = "fl")  # mpg에 연료 가격 변수 추가
mpg

#	Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.

mpg %>%
  select(model, fl, price_fl) %>%       # model, fl, price_fl 추출
  head(5)    




## 분석 도전

# 문제1. popadults는 해당 지역의 성인 인구, poptotal은 전체 인구를 나타냅니다. midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.

# midwest 불러오기
midwest <- as.data.frame(ggplot2::midwest)

# midwest에 백분율 변수 추가
midwest <- midwest %>%
  mutate(child = (poptotal-popadults) / poptotal * 100)


# 문제2. 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)의 미성년 인구 백분율을 출력하세요.

midwest %>%
  select(county, child) %>%  
  arrange(desc(child)) %>% 
  head(5)                     


# 문제3. 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요.

# midwest에 grade 변수 추가
midwest <- midwest %>%
  mutate(grade = ifelse(child >= 40, "large",
                        ifelse(child >= 30, "middle", "small")))
head(midwest)

# 미성년 비율 등급 빈도표
table(midwest$grade)  



# 문제4. popasian은 해당 지역의 아시아인 인구를 나타냅니다. '전체 인구 대비 아시아인 인구 백분율' 변수를 추가하고, 하위 10개 지역의 state(주), county(지역명), 아시아인 인구 백분율을 출력하세요.
midwest %>%
  mutate(asian = (popasian / poptotal) * 100) %>%  # 백분율 변수 추가
  arrange(asian) %>%                               # 오름차순 정렬
  select(state, county, asian) %>%                 # 변수 추출
  head(10)                                         # 상위 10행 출력 



#######################sales_data#########################

getwd()
setwd("C:/Rdata")

sales <- read.csv("sales_data.csv")
sales

sales_my <- sales %>% filter(CATEGORY == "비타민음료" | CATEGORY == "스포츠,이온음료")
sales_my

write.csv(sales_my, "sales_AI.csv")



## shapiro test

sales_AI <- read.csv("sales_AI.csv")

#hist(sales_AI, breaks = 5, probability = T)
#lines(density(sales_AI), col=2, type = 'h', lwd=0.5)
shapiro.test(sales_AI$RAIN_DAY[1:120])
shapiro.test(sales_AI$HOLIDAY[1:120])
