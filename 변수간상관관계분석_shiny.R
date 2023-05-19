#install.packages("shiny")
svdong <- read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_dong.csv") %>% as_tibble()

library(shiny)
library(ggplot2)
library(tidyr)

#변수 간 상관 관계 분석
# UI 구성
ui <- fluidPage(
  titlePanel("Correlation Matrix Heatmap"),
  sidebarLayout(
    sidebarPanel(
      # 상관계수 히트맵 색상 설정
      selectInput("color", "Heatmap Color", c("Red", "Blue", "Green", "Gray"))
    ),
    mainPanel(
      plotOutput("heatmap")
    )
  )
)

#Server 구성
server <- function(input, output) {
  # 상관 행렬 계산
  correlation <- cor(svdong[, c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "OP_RATE", "CLS_RATE")])
  
  # 상관 행렬을 데이터프레임으로 변환
  cor_df <- as.data.frame(correlation)
  cor_df <- cor_df %>% rownames_to_column(var = "Var1") %>%
    pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value")
  
  # 히트맵 시각화
  output$heatmap <- renderPlot({
    ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = input$color, high = "white") +
      labs(x = "Variable", y = "Variable", title = "Correlation Heatmap")
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
