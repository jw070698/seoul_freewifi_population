library(XML)
library(RCurl)
library(dplyr)

# 1. 데이터 수집 - API 스크래핑 ####
# 서울시 공공와이파이 서비스 위치 정보 ####
url<-"http://openapi.seoul.go.kr:8088"
myKey<-"4a5a416b4b796f6f33366d796a4a5a"
StartIndex=1
EndIndex=1

myURL<-paste(url,"/",myKey,"/xml/TbPublicWifiInfo/",StartIndex,"/",EndIndex,sep="")
myURL

#13628개의 데이터 불러오기 위해 수열 지정
start<-seq(from=1, to=580033, by=1000)

df_wifi<-data.frame()
for (StartIndex in start) {
  EndIndex=StartIndex+999 #한번 요청할 때 1000을 넘지 못함
  myURL<-paste(url,"/",myKey,"/xml/TbPublicWifiInfo/",StartIndex,"/",EndIndex,sep="")
  myxml<-getURL(myURL)
  myxml<-xmlParse(myxml)
  xmlRoot(myxml)
  temp<-xmlToDataFrame(getNodeSet(myxml,'//row'))
  df_wifi<-rbind(df_wifi,temp)
}

str(df_wifi)

##열이름 변환
FreeWifi<-select(df_wifi,X_SWIFI_WRDOFC, X_SWIFI_CNSTC_YEAR, LAT, LNT)
FreeWifi<-rename(FreeWifi, 자치구=X_SWIFI_WRDOFC, 설치년도=X_SWIFI_CNSTC_YEAR, X좌표=LAT, Y좌표=LNT)
View(FreeWifi)
str(FreeWifi)

FreeWifi[i,]$id <- 0 # 새로운 법정동코드 변수추가

## 법정동코드 변수 추가
for (i in 1:nrow(FreeWifi)) {
    FreeWifi[i,]$id<-switch(FreeWifi[i,]$자치구, "강남구"=11680, "강동구"=11740, "강북구"=11305, "강서구"=11500, "관악구"=11620, "광진구"=11215, "구로구"=11530, "금천구"=11545, "노원구"=11350, "도봉구"=11320, "동대문구"=11230, "동작구"=11590, "마포구"=11440, "서대문구"=11410, "서초구"=11650, "성동구"=11200, "성북구"=11290, "송파구"=11710, "양천구"=11470, "영등포구"=11560, "용산구"=11170, "은평구"=11380, "종로구"=11110, "중구"=11140, "중랑구"=11260, "도봉구"=11320)
}

View(FreeWifi) 


## csv파일로 저장
write.csv(FreeWifi, file="SeoulWifi.csv")

# 행정동별 서울생활인구(내국인) ####
url<-"http://openapi.seoul.go.kr:8088"
myKey<-"494c547a6a796f6f38325944585859"
StartIndex=1
EndIndex=1
month=202111

start<-seq(from=1, to=284929, by=1000) # 11월 28일까지 데이터 284929개
df_pop<-data.frame()
df_nov <- data.frame()

for (day in 1:28){
  myURL<-paste(url,"/",myKey,"/xml/SPOP_LOCAL_RESD_DONG/",StartIndex,"/",EndIndex,sep="","/",month,day)
  for (StartIndex in start) {
    EndIndex=StartIndex+999 #한번 요청할 때 1000을 넘지 못함
    myURL<-paste(url,"/",myKey,"/xml/SPOP_LOCAL_RESD_DONG/",StartIndex,"/",EndIndex,sep="")
    myxml<-getURL(myURL)
    myxml<-xmlParse(myxml)
    xmlRoot(myxml)
    temp<-xmlToDataFrame(getNodeSet(myxml,'//row'))
    df_pop<-rbind(df_pop,temp)
  }
  df_nov <- rbind(df_nov,df_pop)
}

View(df_nov)
str(df_nov)

## 필요한열 선택 및 열이름 변환
population<-select(df_nov,STDR_DE_ID, ADSTRD_CODE_SE, TOT_LVPOP_CO)
population<-rename(population, 기준일ID=STDR_DE_ID, 행정동코드=ADSTRD_CODE_SE, 총생활인구수=TOT_LVPOP_CO)
View(population)
str(population)

## 법정동코드 변수 추가

for (i in 1:nrow(population)){
  population$id <- switch(substr(population[i,]$행정동코드,1,5), "강남구"=11680, "강동구"=11740, "강북구"=11305, "강서구"=11500, "관악구"=11620, "광진구"=11215, "구로구"=11530, "금천구"=11545, "노원구"=11350, "도봉구"=11320, "동대문구"=11230, "동작구"=11590, "마포구"=11440, "서대문구"=11410, "서초구"=11650, "성동구"=11200, "성북구"=11290, "송파구"=11710, "양천구"=11470, "영등포구"=11560, "용산구"=11170, "은평구"=11380, "종로구"=11110, "중구"=11140, "중랑구"=11260, "도봉구"=11320)
} 

View(pop)

## csv파일로 저장
write.csv(population, file="Seoulpopulation.csv")



# 2. EDA 탐색과정 ####
# 서울시 공공와이파이 서비스 위치 정보 ####
library(dplyr)

SeoulWifi.df<-read.csv("SeoulWifi.csv")
str(SeoulWifi.df)
SeoulWifi.df$id<-as.numeric(SeoulWifi.df$id)
View(SeoulWifi.df)
'gu_count<-SeoulWifi.df %>%
  group_by(id, 위도, 경도) %>%
  summarise(count=n())'

"gu_count<-SeoulWifi.df%>%
  group_by(id)%>%
  summarise(n=n())"

Wifi_year<-SeoulWifi.df%>%
  group_by(id,설치년도)%>%
  summarise(count=n())
View(Wifi_year)

"for (i in 1:length(Wifi_year)) {
  if (Wifi_year.df$설치년도 <=2019) {
    
  }
}"
"Wifi_year$cum_count <- cumsum(Wifi_year$count) ##설치 누적개수
Wifi_year<-Wifi_year[Wifi_year$설치년도>=2019,]
View(Wifi_year)"

Wifi_year_cum<-Wifi_year %>%
  group_by(id) %>%
  mutate(cum_count=cumsum(count))

Wifi_year_cum<-Wifi_year_cum[Wifi_year_cum$설치년도>=2019,]
View(Wifi_year_cum)

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

merge <- merge(seoul_map, Wifi_year_cum, by='id')
View(merge)

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

plot2019<-ggplot() + geom_polygon(data = merge[merge$설치년도==2019,], aes(x=long, y=lat, group=group, fill = cum_count)) +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") 
plot2019
plot2020<-ggplot() + geom_polygon(data = merge[merge$설치년도==2020,], aes(x=long, y=lat, group=group, fill = cum_count)) +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") 
plot2020
plot2021<-ggplot() + geom_polygon(data = merge[merge$설치년도==2021,], aes(x=long, y=lat, group=group, fill = cum_count)) +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") 
plot2021
ggplot(merge, aes(x=long, y=lat))+
  geom_polygon()

# 행정동별 서울생활인구(내국인) ####
table(population$id)
hist(population$id)
library(ggplot2)
qplot(population$id)