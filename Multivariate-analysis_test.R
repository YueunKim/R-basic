
#################################### 연습문제 풀이



# 1. 1.	20140528_baseball.csv는 2014년 5월 28일 현재 한국 프로야구 각 팀의 성적을 보여준다.
# 이 자료를 이용해 별그림,체르노프페이스, 나이팅게일 차트를 적절한 label을 포함하여 
# 그리고 비슷한 패턴을 가지는 그룹으로 나누어 각 그룹이 어떤 변수적 특징을 가지는지 서술하여라.


data = read.csv("20140528_baseball.csv")
head(data)
rownames(data)=data[,1] #승률부터 평균자책까지
head(data)

stars(data[,2:6],flip.labels = F, key.loc = c(9,3))
stars(data[,2:6],flip.labels = F, key.loc = c(9,3), draw.segments=TRUE)
faces(data[,2:6])




# 2. 2013_baseball.csv는 2013년 시즌 각 타자의 성적을 포함하고 있다.
# 평행좌표 플롯을 활용해 선수들의 성적 패턴에 어떤 경향이 있는지 알아보려 한다.
# 포지션 별,팀별 평행좌표 플롯을 그리고 각 포지션,혹은 팀별 타자의 성적에 어떤 경향이 있는지 서술하라.


bb2013 = read.csv("2013_baseball.csv")
head(bb2013)
positon=bb2013$포지션
head(positon)

base2_pos = bb2013[,c(2,4:11)]
base2_pos2 = aggregate(base2_pos[, 2:9], by = list(포지션 = base2_pos$포지션), sum)
head(base2_pos2)

rownames(base2_pos2)=base2_pos2[,1]
head(base2_pos2)

library(lattice)
parallel(~bb2013[,4:11] | position,horizontal.axis = F, col=1)




# 팀별평행좌표
team = bb2013$팀
parallel(~bb2013[,4:11]|team,horizontal.axis = F, col =1)



# 3.	2013_baseball.csv에 있는 각 타자에 대한 변수를 사용하여 주성분 분석을 시행하라.
# 적당한 주성분의 개수를 파악하고 해당 주성분들로 설명되는 분산의 비율을 구하라.
# 행렬도를 통해 선수들의 특징이 어떻게 파악되는지 살펴보시오.



rownames(bb2013)=bb2013[,1]
rownames(bb2013)
head(2013)
model=prcomp(bb2013[,4:11],scale=T)
plot(model)
summary(model)
biplot(model)
