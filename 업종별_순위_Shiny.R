#데이터 불러오기
data_sv <- read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_sv.csv") %>% as_tibble()

#데이터 구조 확인
str(data_sv)
summary(data_sv)
head(data_sv)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#결측치 분석
sum(is.na(data_sv))
#결측치 2363발생

# 각 열별 결측치 개수 확인
colSums(is.na(data_sv)) #OP_RATE과 CLS_RATE에 결측치 발생

#그냥 0인 값인줄 알았는데 결측치와 NA는 다른것.
# OP_RATE 변수에서 NA를 가진 행들의 DONG_NM 선택
missing_OP_RATE <- data_sv[is.na(data_sv$OP_RATE), "DONG_NM"]
print(missing_OP_RATE)

# 결측치 채우기
#개업률(OP_RATE) = (개업 점포 수 / 전체 점포 수) * 100
#폐업률(CLS_RATE) = (폐업 점포 수 / 전체 점포 수) * 100
data_sv$OP_RATE[is.na(data_sv$OP_RATE)] <- (data_sv$OP_MK_NUM[is.na(data_sv$OP_RATE)] / data_sv$MK_NUM[is.na(data_sv$OP_RATE)]) * 100
data_sv$CLS_RATE[is.na(data_sv$CLS_RATE)] <- (data_sv$CLS_MK_NUM[is.na(data_sv$CLS_RATE)] / data_sv$MK_NUM[is.na(data_sv$CLS_RATE)]) * 100
# 이걸 실행 했더니 MK_NUM이 0이면 분모가 0이라결측치가 되어버림.

# MK_NUM이 0인 행을 찾아 OP_RATE와 CLS_RATE 값을 0으로 설정, 그리고 나서 결측치 채우기.
data_sv$OP_RATE[data_sv$MK_NUM == 0] <- 0
data_sv$CLS_RATE[data_sv$MK_NUM == 0] <- 0
data_sv$OP_RATE[is.na(data_sv$OP_RATE)] <- (data_sv$OP_MK_NUM[is.na(data_sv$OP_RATE)] / data_sv$MK_NUM[is.na(data_sv$OP_RATE)]) * 100
data_sv$CLS_RATE[is.na(data_sv$CLS_RATE)] <- (data_sv$CLS_MK_NUM[is.na(data_sv$CLS_RATE)] / data_sv$MK_NUM[is.na(data_sv$CLS_RATE)]) * 100

#결측치 확인
sum(is.na(data_sv))

#----------------------------------------------------------------------------------------------------------------------------------------------------

#이상치 분석
boxplot(data_sv$CLS_RATE)

#변수의 분포 확인
hist(data_sv$CLS_RATE)

#----------------------------------------------------------------------------------------------------------------------------------------------------

# 필요한 라이브러리를 불러옴.
library(ggplot2)
library(corrplot)

# 수치형 변수만 골라서
numeric_data <- data_sv[c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "OP_RATE", "CLS_RATE")]

# 상관 계수 행렬 계산
correlation_matrix <- cor(numeric_data, use = "complete.obs")  # 'use' 옵션을 사용하여 결측값을 제외하고 계산합니다

# 상관 계수 행렬 시각화
corrplot(correlation_matrix, method = "circle")

# 폐업률과 각 변수간의 산점도그리기
par(mfrow = c(2,3)) # 2행 3열로 그림 배치
plot(data_sv$MK_NUM, data_sv$CLS_RATE, main="MK_NUM vs CLS_RATE")
plot(data_sv$SMK_NUM, data_sv$CLS_RATE, main="SMK_NUM vs CLS_RATE")
plot(data_sv$OP_MK_NUM, data_sv$CLS_RATE, main="OP_MK_NUM vs CLS_RATE")
plot(data_sv$CLS_MK_NUM, data_sv$CLS_RATE, main="CLS_MK_NUM vs CLS_RATE")
plot(data_sv$FRC_MK_NUM, data_sv$CLS_RATE, main="FRC_MK_NUM vs CLS_RATE")
plot(data_sv$OP_RATE, data_sv$CLS_RATE, main="OP_RATE vs CLS_RATE")


#----------------------------------------------------------------------------------------------------------------------------------------------------

#업종별 폐업률 보기
# 필요한 라이브러리를 불러온다
library(ggplot2)
library(dplyr)

# 각 업종별 평균 폐업률을 계산한다
avg_closure_rate <- data_sv %>%
  group_by(SV_NM) %>%
  summarise(avg_cls_rate = mean(CLS_RATE, na.rm = TRUE))

# 평균 폐업률을 이용해 막대 그래프를 그린다
ggplot(avg_closure_rate, aes(x = SV_NM, y = avg_cls_rate)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "업종", y = "평균 폐업률", title = "업종별 폐업률")

#----------------------------------------------------------------------------------------------------------------------------------------------------

# 업종별 폐업률과 점포수 계산
avg_cls_rate <- aggregate(data_sv$CLS_RATE ~ data_sv$SV_NM, data = data_sv, FUN = mean, na.rm = TRUE)
total_mk_num <- aggregate(data_sv$MK_NUM ~ data_sv$SV_NM, data = data_sv, FUN = sum, na.rm = TRUE)

# 결과를 데이터 프레임으로 병합
data_agg <- merge(avg_cls_rate, total_mk_num, by.x = "data_sv$SV_NM", by.y = "data_sv$SV_NM")
names(data_agg) <- c("SV_NM", "Avg_CLS_RATE", "Total_MK_NUM")

#----------------------------------------------------------------------------------------------------------------------------------------------------

#[Shiny]
#폐업률과 점포수에 가중치를 부여하여 새로운 점수 변수를 만들어 시각화.
# 폐업률과 점포수에 가중치를 부여하여 '점수' 변수 생성
data_agg$Score <- (data_agg$Avg_CLS_RATE * 0.7) + (data_agg$Total_MK_NUM * 0.3)

# Shiny 앱 코드
library(shiny)

ui <- fluidPage(
  numericInput("n", "Enter the number of top industries:", 5),
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    top_industries <- data_agg[order(-data_agg$Score), ][1:input$n, ]
    ggplot(top_industries, aes(x = SV_NM, y = Score)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Industry", y = "Score", title = "Top Industries by Score")
  })
}

shinyApp(ui = ui, server = server)


#----------------------------------------------------------------------------------------------------------------------------------------------------


#좀더 간지나게
install.packages("shinydashboard") #기본
install.packages("plotly")

# 필요한 패키지 로드
library(shiny)
library(shinydashboard)
library(plotly)

# UI 정의
ui <- dashboardPage(
  dashboardHeader(title = "Top Industries by Score"),
  dashboardSidebar(
    sliderInput("n", "Select the number of top industries:", min = 1, max = 100, value = 5)
  ),
  dashboardBody(
    plotlyOutput("plot")
  )
)

# Server 정의
server <- function(input, output) {
  output$plot <- renderPlotly({
    top_industries <- data_agg[order(-data_agg$Score), ][1:input$n, ]
    p <- plot_ly(top_industries, x = ~SV_NM, y = ~Score, type = 'bar') %>%
      layout(title = "Top Industries by Score",
             xaxis = list(title = "Industry"),
             yaxis = list(title = "Score"))
    p
  })
}

# 앱 실행
shinyApp(ui = ui, server = server)

#----------------------------------------------------------------------------------------------------------------------------------------------------