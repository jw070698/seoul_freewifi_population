rm(list=ls())
setwd("/Users/jeonjiwon/Desktop/me/3-2/3.빅데이터통계분석")
population=read.csv("Seoulpopulation.csv")
library(ggplot2)
library(dplyr)
gu_count <- aggregate(총생활인구수~id,population,sum)
gu_count <- gu_count %>% mutate(name= case_when(
  id == 11680 ~"강남구",id == 11740 ~"강동구",id == 11305 ~"강북구",
  id == 11500 ~"강서구",id == 11620 ~"관악구",id == 11215 ~"광진구",
  id == 11530 ~"구로구",id == 11545 ~"금천구",id == 11350 ~"노원구",
  id == 11320 ~"도봉구",id == 11230 ~"동대문구",
  id == 11590 ~"동작구",id == 11440 ~"마포구",
  id == 11410 ~"서대문구",id == 11650 ~"서초구",
  id == 11200 ~"성동구",id ==11290 ~"성북구", id == 11710 ~"송파구",
  id == 11470 ~"양천구",id == 11560 ~"영등포구",
  id == 11170 ~"용산구",id == 11380 ~"은평구",id == 11110 ~"종로구",
  id == 11140 ~"중구", id == 11260 ~"중랑구", id == 11320 ~"도봉구"
))

#워드클라우드
library(wordcloud)
library(RColorBrewer)
radius=gu_count$총생활인구수 #글씨 크기
pal<-brewer.pal(8,"Dark2")
set.seed(123)
par(family="AppleGothic")
wordcloud(words=gu_count$name,freq=radius,random.order=F,rot.per=.1,colors=pal)

#treemap
library(treemap)
treemap(gu_count,
        index="name",
        vSize="총생활인구수", # 타일의 크기
        vColor="총생활인구수", # 타일의 컬러
        type="value", # 타일 컬러링 방법
        bg.labels="yellow",fontfamily.labels="AppleGothic") # 레이블의 배경색
