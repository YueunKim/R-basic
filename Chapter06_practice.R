
############################################## 6장 practice 2

# Q1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. displ(배기량)이 4 이하인 자동차와 5 이상인 자동차 중 어떤 자동차의 hwy(고속도로 연비)가 평균적으로 더 높은지 알아보세요.
mpg <- as.data.frame(ggplot2::mpg)   

mpg_a <- mpg %>% filter(displ <= 4) 
mpg_b <- mpg %>% filter(displ >= 5)  

mean(mpg_a$hwy)
mean(mpg_b$hwy)     


# Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다. "audi"와 "toyota" 중 어느 manufacturer(자동차 제조 회사)의 cty(도시 연비)가 평균적으로 더 높은지 알아보세요.
mpg_audi <- mpg %>% filter(manufacturer == "audi")      
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")

mean(mpg_audi$cty)    
mean(mpg_toyota$cty)  


#	Q3. "chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 알아보려고 합니다. 이 회사들의 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요.

# manufacturer가 chevrolet, ford, honda에 해당하면 추출
mpg_new <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_new$hwy)



############################################## 6장 practice 3


# Q1. mpg 데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해서 분석에 활용하려고 합니다. mpg 데이터에서 class(자동차 종류), cty(도시 연비) 변수를 추출해 새로운 데이터를 만드세요. 새로 만든 데이터의 일부를 출력해서 두 변수로만 구성되어 있는지 확인하세요.
mpg <- as.data.frame(ggplot2::mpg) 

df <- mpg %>% select(class, cty)    
head(df)                            

# Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한 데이터를 이용해서 class(자동차 종류)가 "suv"인 자동차와 "compact"인 자동차 중 어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.
df_suv <- df %>% filter(class == "suv")          
df_compact <- df %>% filter(class == "compact")  

mean(df_suv$cty)                              
mean(df_compact$cty)                         



############################################## 6장 practice 4


# "audi"에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가 높은지 알아보려고 합니다. "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차의 데이터를 출력하세요.
mpg <- as.data.frame(ggplot2::mpg)         

mpg %>% filter(manufacturer == "audi") %>%  
  arrange(desc(hwy)) %>%                   
  head()                                  



############################################## 6장 practice 5


# Q1. mpg 데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.

mpg <- as.data.frame(ggplot2::mpg)                
mpg_1 <- mpg                                  

mpg_1 <- mpg_new %>% mutate(total = cty + hwy) 


# Q2. 앞에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가세요.
mpg_1 <- mpg_1 %>% mutate(mean = total/2)     


# Q3. '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.
mpg_1 %>%
  arrange(desc(mean)) %>% 
  head(3)                 


# Q4. 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요. 데이터는 복사본 대신 mpg 원본을 이용하세요.
mpg %>%
  mutate(total = cty + hwy,   
         mean = total/2) %>%  
  arrange(desc(mean)) %>%    
  head(3)                    



############################################## 6장 practice 6


#	Q1. mpg 데이터의 class는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로 분류한 변수입니다. 어떤 차종의 연비가 높은지 비교해보려고 합니다. class별 cty 평균을 구해보세요.
mpg <- as.data.frame(ggplot2::mpg)  

mpg %>%
  group_by(class) %>%              
  summarise(mean_cty = mean(cty)) 



#	Q2. 앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의 도시 연비가 높은지 쉽게 알아볼 수 있도록 cty 평균이 높은 순으로 정렬해 출력하세요.
mpg %>%
  group_by(class) %>%                  
  summarise(mean_cty = mean(cty)) %>%  
  arrange(desc(mean_cty))              



#	Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다. hwy 평균이 가장 높은 회사 세 곳을 출력하세요.
mpg %>%
  group_by(manufacturer) %>%           
  summarise(mean_hwy = mean(hwy)) %>%  
  arrange(desc(mean_hwy)) %>%          
  head(3)                              


# Q4. 어떤 회사에서 "compact"(경차) 차종을 가장 많이 생산하는지 알아보려고 합니다. 각 회사별 "compact" 차종 수를 내림차순으로 정렬해 출력하세요.
mpg %>%
  filter(class == "compact") %>%  
  group_by(manufacturer) %>%     
  summarise(count = n()) %>%     
  arrange(desc(count))          




############################################## 6장 practice 7


fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel


# Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 연료 가격을 나타낸 변수는 없습니다. 위에서 만든 fuel 데이터를 이용해서 mpg 데이터에 price_fl(연료 가격) 변수를 추가하세요.

mpg <- as.data.frame(ggplot2::mpg)    
mpg <- left_join(mpg, fuel, by = "fl")  
mpg

#	Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.

mpg %>%
  select(model, fl, price_fl) %>%    
  head(5) 




############################################## 6장 분석 도전


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
  mutate(asian = (popasian / poptotal) * 100) %>%  
  arrange(asian) %>%                              
  select(state, county, asian) %>%                
  head(10)                                         

