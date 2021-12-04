############2014부터 누적하는거 해야됨!!!!
# 2. EDA 탐색과정 ####
# 무료 와이파이 현황(개방표준)####

library(dplyr)

SeoulWifi.df<-read.csv("SeoulWifi.csv")
str(SeoulWifi.df)
SeoulWifi.df$id<-as.numeric(SeoulWifi.df$id)
View(SeoulWifi.df)
'gu_count<-SeoulWifi.df %>%
  group_by(id, 위도, 경도) %>%
  summarise(count=n())'

## 년도 상관없이 자치구별 공공와이파이 설치현황
gu_count<-SeoulWifi.df%>%
  group_by(id)%>%
  summarise(n=n())

## 법정동코드에 해당하는 자치구 이름 추가
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

# 년도별 자치구별 공공와이파이 설치현황
gu_year <- SeoulWifi.df %>% 
  group_by(설치년도,자치구) %>% 
  summarise(n=n())

#SeoulWifi.df<-SeoulWifi.df %>% filter(설치년도>=2018)
## 히스토그램
ggplot(data= gu_year, aes(x = reorder(자치구,-n), y=n)) +geom_col()
ggplot(data= gu_count, aes(x = reorder(name,-n), y=n)) +geom_col()

## 지도 시각화
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

map <- shapefile("TL_SCCO_SIG.shp") ## 파일 저장되어있어야함.
## map 좌표계 변환
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
## map을 데이터프레임으로 변환
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)
new_map$id <- as.numeric(new_map$id)
## 서울 데이터 추출
seoul_map <- new_map[new_map$id <= 11740,]
View(seoul_map)

merge <- merge(seoul_map, gu_count, by='id')

'count<-merge %>%
  group_by(id, long, lat, group) %>%
  summarise(count=n())'

'count2<-merge%>%
  group_by(id)%>%
  mutate(n=n())'

#View(merge)
#ggplot() + geom_polygon(data = count, aes(x=long, y=lat, group=group), fill = 'white', color='grey')+
#  geom_point(color="grey", alpha=.55, shape=1, aes(), size=3)
#ggplot() + geom_polygon(data = count, aes(x=long, y=lat, group=group), fill = 'white', color='black')
ggplot() + geom_polygon(data = merge, aes(x=long, y=lat, group=group, fill = n))

ggplot(merge, aes(x=long, y=lat))+
  geom_polygon()
