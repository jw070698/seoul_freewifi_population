# 3. 시각화 및 대시보드(Rshiny) - 인사이트 도출
library(shiny)
library(shinydashboard)
#library(repr)
#library(shinycssloaders)

#source("SeoulWifi_EDA.r")
load("Wifi_map_merge.rda") ##근데 저장되어있어야함......ㅜㅜ
gu_name<-read.csv("gu_name.csv")

ui <- dashboardPage(
  dashboardHeader(title="유동인구수를 통한 와이파이 추가적인 설치 필요 지역 탐색", titleWidth=600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Charts",
               menuSubItem("Seoul-public Wi-Fi",tabName = "SeoulWifi")
               )
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        radioButtons(inputId = "year",
                     "연도를 고르세요",
                     list(2019,2020,2021)
        ), width=1
      ),
      box(title="서울시 구별 공공와이파이",status="primary",solidHeader=TRUE,collapsible=TRUE,
          plotOutput(outputId = "map", width=600),
          plotOutput(outputId = ""),
          width=5)
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
}


shinyApp(ui=ui, server=server)
  
  


 " tags$h1('빅데이터 통계분석 1조 대시보드'),
  tags$hr(),
  tags$br(),
  tags$p(strong('유동인구수를 통한 와이파이 추가적인 설치 필요 지역 탐색')),
  tags$p(em('1914243 이윤아 | 1911815 임유나 | 1816622 전지원'))
)"


