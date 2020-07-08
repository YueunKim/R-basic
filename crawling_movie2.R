
## 네이버 영화 크롤링을 통한 시나리오 분석


# 가장높은 평가총점을 가진 것의 탑텐의 영화제목을 찾고
# 가장많은 빈도가 나온 10개의 영화제목을 찾고
# 총점수가 500이상이고 리뷰한 총갯수가 100건이상인 영화제목

setwd("c:/Rdata")

library(rvest)
library(stringr)
library(dplyr)

title = c()
grade = c()
time = c()

t_css = ".color_b"
gr_css = ".list_netizen_score em" # list_netizen_score 클래스 안에 em이라는 태그
pt_css = ".title+ .num"

base_url = "https://movie.naver.com/movie/point/af/list.nhn?&page="


for (i in 1:100){
cr_url = paste0(base_url,1)
cr_url

hdoc = read_html(cr_url,encoding = "CP949")
n_title = html_nodes(hdoc,t_css)
n_gr = html_nodes(hdoc,gr_css)
n_pt = html_nodes(hdoc,pt_css)

title_part = html_text(n_title)
grade_part = html_text(n_gr)
pt_part = html_text(n_pt)
time_part = str_sub(pt_part, -8)

title = c(title, title_part)
grade = c(grade, grade_part)
time = c(time, time_part)
}


movie = data.frame(title,grade,time)
View(movie)



write.csv(movie,"movie.csv")
data = read.csv('movie.csv')
head(data)


## 시나리오분석

# 데이터 전처리

top10_A = data %>%
  select(title,grade)%>%
  group_by(title)%>%
  summarise(total = sum(grade),
            count = n())%>%
  arrange(desc(total),desc(count))%>%
  head(10)

top10_A


library(ggplot2)

ggplot(data=top10_A,aes(x=reorder(title,total),y=total))+
  geom_col(fill='red')+
  geom_text(aes(label=top10_A$total),hjust=-0.2,col='blue')+
  coord_flip()






























