setwd("c:/Rdata")
getwd()

install.packages("ggmap")

library(ggmap)
library(stringr)
library(dplyr)
library(rvest)

register_google(key = "my_key")
tt = get_map(location = "대전 중구 대흥동", zoom = 15,
        maptype = 'satellite', #roadmap,hybird,terrian,...
        source='google')
ggmap(tt)

qmap(location = "대전 중구 대흥동", zoom = 15,
     maptype = 'satellite', #roadmap,hybird,terrian,...
     source='google')

# 옆에 그래프 깨끗하게
plot.new()
frame()


geocodeQueryCheck()

geocode(location = "대전 중구 대흥동",
        output = 'latlon', # 경도와 위도
        source = 'google')

geocode(location = "대전 중구 대흥동",
        output = 'latlona', # 경도와 위도
        source = 'google')

geocode(location = enc2utf8(x="대전 중구 대흥동$language=ko"), #한글로
        output = 'latlona', # 경도와 위도
        source = 'google')

myloc = geocode(location = '대전광역시 중구 수도산로 28-1',
                output = 'latlon',
                source = 'google')
myloc
center = c(myloc$lon, myloc$lat)
qmap(location = center, 
     zoom = 18,
     maptype = 'hybrid',
     source = 'google')+
  geom_point(data = myloc,
             mapping = aes(x=lon,y=lat),
             shape = '*',
             color = 'red',
             stroke=18,size=10)






## 서울특별시의 대학목록 크롤링

url = "https://namu.wiki/w/%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C%EC%9D%98%20%EB%8C%80%ED%95%99%EA%B5%90%20%EB%AA%A9%EB%A1%9D"

hdoc = read_html(url,encoding = 'UTF-8')
df = hdoc%>%
  html_nodes(".wiki-paragraph a")%>%
  html_text()
head(df,50)


str_detect(df,pattern = '대학교')
univ=ifelse(str_detect(df,pattern = '대학교'),df,"") # 대학교라는 글자가있으면 df, 아니면 공백   
univ

kk=univ%>%
  data.frame()
kk=Filter(function(x){nchar(x)>=5},univ) # 문자길이가 5개 이상
kk
univName = kk[2:28]
univName
univCord=geocode(location = univName,
                 output = 'latlon',
                 source = 'google')
univDf=data.frame(univ=univName,
                  lon=univCord$lon,
                  lat=univCord$lat)
head(univDf)

univDfNa=na.omit(univDf)
univDfNa

center = c(mean(x=univDfNa$lon),mean(x=univDfNa$lat))
center

qmap(location = center,
     zoom=12,
     maptype='satellite',
     source = 'google')+
  geom_point(data=univDfNa,
             aes(x=lon,y=lat),
             shape = '*',
             color='red',
             size=6)+
  geom_text(data=univDfNa,
            aes(x=lon,y=lat,label=univ),
            color='green', hjust=0.5, 
            vjust=-0.1, size=3,
            fontface='bold', #글씨두께
            family='NanumGodic') #글씨체


