# 폐업률과 프랜차이즈 점포수 간의 관계(Shiny)

library(shiny)
library(ggplot2)
library(dplyr)

# 서버 부분
server <- function(input, output) {
  
  # 데이터 불러오기
  svdong <- reactive({
    read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_dong.csv") # 이 부분은 데이터 파일 위치에 맞게 수정하셔야 합니다.
  })
  
  # 폐업률과 프랜차이즈 점포수 간의 관계 시각화 - 산점도
  output$scatterPlot <- renderPlot({
    ggplot(svdong(), aes(x = CLS_RATE, y = FRC_MK_NUM)) +
      geom_point() +
      labs(x = "Closure Rate", y = "Franchise Store Count", title = "Relationship between Closure Rate and Franchise Store Count")
  })
  
  # 상관계수 계산 및 시각화
  output$correlationPlot <- renderPlot({
    correlation <- cor(svdong()$FRC_MK_NUM, svdong()$CLS_RATE)
    ggplot(svdong(), aes(x = FRC_MK_NUM, y = CLS_RATE)) +
      geom_point() +
      labs(x = "Franchise Store Count", y = "Closure Rate", title = "Relationship between Franchise Store Count and Closure Rate") +
      geom_smooth(method = "lm", se = FALSE) +
      annotate("text", x = max(svdong()$FRC_MK_NUM), y = min(svdong()$CLS_RATE), 
               label = paste("Correlation =", round(correlation, 2)), hjust = 1)
  })
}

# 사용자 인터페이스 부분
ui <- fluidPage(
  titlePanel("Closure Rate and Franchise Store Count Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # 추가적인 사용자 입력 요소가 필요하다면 이곳에 위치
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),  # 산점도 출력
      plotOutput("correlationPlot")  # 상관계수 계산 및 시각화 출력
    )
  )
)

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
