library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)

# 데이터 불러오기
data_dong <- read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_dong.csv")

# 선택 가능한 수치형 변수 목록
numeric_vars <- names(data_dong)[sapply(data_dong, is.numeric)]

# 서버 부분
server <- function(input, output) {
  
  # 선택한 두 변수의 상관계수 계산
  correlation <- reactive({
    cor(data_dong[, input$xvar], data_dong[, input$yvar])
  })
  
  # 선택한 두 변수의 산점도 시각화
  output$scatterPlot <- renderPlot({
    ggplot(data_dong, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "#FF6F61") +
      geom_smooth(method = "lm", se = FALSE, color = "#D4AF37") +  # 선 색상 변경
      labs(x = input$xvar, y = input$yvar, title = paste("Relationship between", input$xvar, "and", input$yvar),
           title.theme = element_text(color = "#FFFFFF", size = 18),
           axis.title.x = element_text(color = "#FFFFFF", size = 14),
           axis.title.y = element_text(color = "#FFFFFF", size = 14)) +
      annotate("text", x = max(data_dong[, input$xvar]), y = min(data_dong[, input$yvar]), 
               label = paste("Correlation =", round(correlation(), 2)),
               hjust = 1, color = "#FFFFFF", size = 14)
  })
  # 전체 상관 관계 행렬 표
  output$corPlot <- renderPlot({
    corr_matrix <- cor(data_dong[, numeric_vars])
    corrplot(corr_matrix, method = "circle",
             tl.col = "#FFFFFF", tl.srt = 45, tl.cex = 0.8, tl.offset = 0.6,
             cl.lim = c(-1, 1), cl.length = 0.8, cl.ratio = 0.3, cl.align = "r",
             addrect = 2, rect.col = "#000000", rect.lty = 1)
  })
}

# 사용자 인터페이스 부분
ui <- fluidPage(
  titlePanel("Variable Correlation"),
  
  theme = shinythemes::shinytheme("superhero"),  # 배경색을 블랙으로 변경하는 테마 적용
  
  tags$style(
    HTML(
      "
      body {
        background-color: #000000;
        color: #FFFFFF;
      }
      .well {
        background-color: #222222;
        border-color: #000000;
      }
      .plot-container {
        background-color: #333333;
        border-color: #000000;
      }
      .selectize-dropdown {
        color: #000000 !important;
      }
      .selectize-input {
        color: #000000 !important;
      }
      .selectize-dropdown-content .active {
        color: #D4AF37 !important;
      }
      .selectize-input.items > div {
        color: #D4AF37 !important;
      }
      "
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # 변수 선택을 위한 selectInput 추가
      selectInput("xvar", "Choose an X variable:", choices = numeric_vars),
      selectInput("yvar", "Choose a Y variable:", choices = numeric_vars)
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      plotOutput("corPlot")  # 전체 상관 관계 행렬 표 출력
    )
  )
)

# Shiny 앱 실행
shinyApp(ui = ui, server = server)


