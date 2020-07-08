setwd("c:/Rdata")
getwd()

library(dplyr)
library(ggplot2)
df = data(package="ggplot2")
df$results

mpg = as.data.frame(ggplot2::mpg)
head(mpg)

table(mpg$drv)
df_g = mpg%>%
  group_by(drv)%>%
  summarise(mean_cty=round(mean(cty),1))
df_g

ggplot(data = df_g,aes(x=reorder(drv,-mean_cty), y=mean_cty))+
  geom_col(fill = c('red','blue','orange'))+
  geom_text(aes(label=df_g$mean_cty),vjust=-0.2,col="red")+
  coord_flip()+xlab("구동타입")+ylab("도시연비비")





## seed함수 (랜덤으로 변하는 숫자를 고정으로 쓰고 싶을 때)
runif(3)
rnorm(3) # = rnorm(3,0,1) mean=0, sd=1

set.seed(1234) 
runif(15) #랜덤으로 15개, set.seed를하면 같은 값





