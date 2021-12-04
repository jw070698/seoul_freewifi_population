# 2. EDA 탐색과정 ####
# 행정동별 서울생활인구(내국인) ####
library(dplyr)
library(ggplot2)

population_df<-read.csv("Seoulpopulation.csv")
str(population_df)
population_df$id<-as.numeric(population_df$id)
glimpse(population_df)
table(is.na(population_df))
head(population_df)
View(population_df)

# 자치구별로 그룹화 (11월)
pop_group<- 
  population_df %>% 
  group_by(id) %>% 
  summarise(sum_population=sum(총생활인구수))
View(pop_group)

# 법정동코드에 해당하는 자치구이름 부여
pop_group <- pop_group %>% mutate(name= case_when(
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

View(pop_group)
  
# 막대그래프
ggplot(data=pop_group, aes(x=name, y=sum_population)) + geom_col()
# 막대그래프 인구 큰 순서대로 정렬 
ggplot(data=pop_group, aes(x=reorder(name,-sum_population), y=sum_population)) + geom_col()

