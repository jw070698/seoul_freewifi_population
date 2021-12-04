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

gu_count<-SeoulWifi.df%>%
  group_by(id)%>%
  summarise(n=n())

#SeoulWifi.df<-SeoulWifi.df %>% filter(설치년도>=2018)

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
