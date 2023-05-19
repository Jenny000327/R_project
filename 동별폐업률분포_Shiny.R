#동별 폐업률 분포 시각화(Shiny)

library(shiny)
library(ggplot2)
library(dplyr)

# 서버 부분
server <- function(input, output) {
  
  # 데이터 불러오기
  data_dong <- reactive({
    read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_dong.csv")
  })
  
  # 폐업률 분포 히스토그램
  output$distPlot <- renderPlot({
    hist(data_dong()$CLS_RATE, xlab = "Closure Rate", main = "Distribution of Closure Rate")
  })
  
  # 폐업률 기준으로 정렬된 데이터프레임
  data_sorted <- reactive({
    data_dong() %>% arrange(desc(CLS_RATE)) %>% slice_head(n = input$n)
  })
  
  # 폐업률 기준 동별 정렬된 바플롯
  output$barPlot <- renderPlot({
    ggplot(data_sorted(), aes(x = reorder(DONG_NM, -CLS_RATE), y = CLS_RATE)) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Dong") +
      ylab("Closure Rate") +
      ggtitle("Closure Rate by Dong")
  })
}

# 사용자 인터페이스 부분
ui <- fluidPage(
  titlePanel("Closure Rate by Dong"),
  
  sidebarLayout(
    sidebarPanel(
      # 사용자가 상위 몇개를 보고 싶은지 선택할 수 있는 슬라이더 추가
      sliderInput("n", "Number of top neighborhoods to show:", 
                  min = 1, max = 401, value = 10)
    ),
    
    mainPanel(
      plotOutput("distPlot"),  # 폐업률 분포 히스토그램 출력
      plotOutput("barPlot")  # 폐업률 기준 동별 바플롯 출력
    )
  )
)

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
