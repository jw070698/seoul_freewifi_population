# 3. 시각화 및 대시보드(Rshiny) - 인사이트 도출
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringr)

#source("SeoulWifi_EDA.r")
load("Wifi_map_merge.rda")
load("gu_count.rda")
load("wifi_pop.rda")
gu_name<-read.csv("gu_name.csv")

create_wordcloud <- function() {
  radius=gu_count$총생활인구수 #글씨 크기
  pal<-brewer.pal(8,"Dark2")
  set.seed(123)
  #par(family="AppleGothic") #!!폰트 저장하는 과정 있어야할듯
  wordcloud<-wordcloud(words=gu_count$name,freq=radius,random.order=F,rot.per=.1,colors=pal)
  return(wordcloud)
  }

create_treemap <- function() {
  library(treemap)
  treemap<-treemap(gu_count,
          index="name",
          vSize="총생활인구수", # 타일의 크기
          vColor="총생활인구수", # 타일의 컬러
          type="value", # 타일 컬러링 방법
          bg.labels="yellow",fontfamily.labels="AppleGothic") # 레이블의 배경색
  return(treemap)
}

ui <- dashboardPage(
  dashboardHeader(title="유동인구수를 통한 와이파이 추가적인 설치 필요 지역 탐색", titleWidth=600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA", tabName = "EDA"),
      menuItem("INSIGHT", tabName="INSIGHT")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "EDA",
        fluidRow(
          box(
            selectInput(inputId = "year",
                         "연도를 고르세요",
                         width=250,
                         list(2019,2020,2021)
            ), width=1
          ),
          box(title="서울시 구별 공공와이파이",status="primary",solidHeader=TRUE,collapsible=TRUE,
              plotOutput(outputId = "map", width=600),
              width=5)
          #box(title="연도별 공공와이파이", status="primary", solidHeader=TRUE, collapsible=TRUE,
          #    plotOutput(outputId = "TimeSeries", width=700) )
        ),
        fluidRow(
          box(radioButtons(inputId = "graph",
                         "그래프를 고르세요",width=250,
                         choices=c("WordCloud","TreeMap")), width=1),
          box(title="서울시 구별 유동인구", status="primary",solidHeader=TRUE,collapsible=TRUE,
              plotOutput(outputId = "graph_out", width=600)))
          
          # tabBox(
          #   title="서울시 구별 유동인구", side="right", 
          #   id="tab",
          #   tabPanel("WordCloud", plotOutput("wordcloud")),
          #   tabPanel("TreeMap", plotlyOutput("treemap"))
          # )
        ),
      tabItem(tabName = "INSIGHT",
              fluidRow(
                box(
                  selectInput(inputId = "year_i",
                              "연도를 고르세요",
                              width=250,
                              list(2019,2020,2021)
                  ), width=1
                ),
                box(title="생활인구 수 대비 공공와이파이 설치 수",solidHeader=TRUE, collapsible=TRUE,
                    plotOutput(outputId = "ratio", width = 1000), width=8)
            )
    )
  )
)
)
      
      
      
      
      

"server <- function(input, output) {
  output$map=renderPlot({
    if(input$year==2019) {print(plot2019)}
    else if(input$year==2020) {print(plot2020)}
    else if(input$year==2021) {print(plot2021)}
  })
}"



server <- function(input, output) {
  plot <- reactive({
    #options(repr.plot.width = 300, repr.plot.height = 100)
    
    ggplot() + geom_polygon(data = merge[merge$설치년도==input$year,], aes(x=long, y=lat, group=group, fill = cum_count)) +
      labs(fill="설치 누적 개수")+
      geom_text(data=gu_name, aes(x=long, y=lat, label=gu))+
      scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
      coord_map()+
      ggtitle(label=input$year)+
      theme(plot.title=element_text(hjust=0.5, face='bold'))

  })
  
  output$map=renderPlot({
#    options(repr.plot.width = 300, repr.plot.height = 100)
    print(plot())
  })
  output$graph_out=renderPlot({
    if (input$graph=="WordCloud") {
      create_wordcloud()
    }
    if (input$graph=="TreeMap") {
      create_treemap()
    }
  })
  output$ratio=renderPlot({
    #barplot(ratio~자치구,wifi_pop[wifi_pop$year==input$year,], main=input$year,xlab="자치구", ylab="누적 설치 수/생활인구 수", col=rainbow(5))
    y=input$year_i
    ggplot(wifi_pop[wifi_pop$year==y,], aes(x=reorder(자치구,-ratio), y=ratio, fill=자치구))+geom_bar(stat="identity", width=0.8)+
      ggtitle(paste(input$year_i,"년"))+xlab("자치구")+ylab("누적 설치 수/생활인구 수")+
      theme(plot.title=element_text(hjust=0.5))+
      theme(axis.text.y = element_blank())+
      theme_bw()
    })
  
  
  "output$TimeSeries=renderPlot({
    ggplot(merge, aes(x=설치년도, y=cum_count, group=자치구))+
      geom_line(aes(color=자치구), size=1)
  })"
}


shinyApp(ui=ui, server=server)
  
  




 " tags$h1('빅데이터 통계분석 1조 대시보드'),
  tags$hr(),
  tags$br(),
  tags$p(strong('유동인구수를 통한 와이파이 추가적인 설치 필요 지역 탐색')),
  tags$p(em('1914243 이윤아 | 1911815 임유나 | 1816622 전지원'))
)"




