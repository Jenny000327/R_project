# 패키지 설치
install.packages("tidyverse") # 얘는 최초 1회만 
library(tidyverse)

# 경로 지정
sv <- read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/SV_DATA.csv") %>% as_tibble()
svdong <- read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_dong.csv") %>% as_tibble()
svdata <- read.csv("C:/Users/jaewo/OneDrive/바탕 화면/R/data_sv.csv") %>% as_tibble()
# 구조 확인
str(sv)
str(svdong)
str(svdata)
names(sv)
names(svdong)
names(svdata)

#-----------------------------------------------------------------------------------
#동별 폐업률
summary(svdong)

#결측치 확인
colSums(is.na(svdong))

install.packages("ggplot2")
install.packages("reshape2")

#--------------------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

#변수 간 상관 관계 분석
# 상관 행렬 계산
correlation <- cor(svdong[, c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "OP_RATE", "CLS_RATE")])

# 상관 행렬을 데이터프레임으로 변환
cor_df <- as.data.frame(correlation)
cor_df <- cor_df %>% rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value")

# 히트맵 시각화
ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Variable", y = "Variable", title = "Correlation Heatmap")


#---------------------------------------------------------------------------------------
#동별 폐업률 분포
library(ggplot2)
library(dplyr)

# 폐업률 분포 확인
hist(svdong$CLS_RATE, xlab = "Closure Rate", main = "Distribution of Closure Rate")

# 폐업률 기준으로 데이터 정렬
data_sorted <- svdong %>% arrange(desc(CLS_RATE))

# 동별 폐업률 분포 시각화
ggplot(data_sorted, aes(x = reorder(DONG_NM, CLS_RATE), y = CLS_RATE)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Dong Name", y = "Closure Rate", title = "Distribution of Closure Rate by Dong")

#--------------------------------------------------------------------------------------
library(ggplot2)

# 폐업률과 프랜차이즈 점포수 간의 관계 시각화 - 산점도
ggplot(svdong, aes(x = CLS_RATE, y = FRC_MK_NUM)) +
  geom_point() +
  labs(x = "Closure Rate", y = "Franchise Store Count", title = "Relationship between Closure Rate and Franchise Store Count")

#---------------------------------------------------------------------------------------
#프랜차이즈 점포수와 폐업률의 상관관계

# 상관계수 계산
correlation <- cor(svdong$FRC_MK_NUM, svdong$CLS_RATE)

# 산점도 시각화
ggplot(svdong, aes(x = FRC_MK_NUM, y = CLS_RATE)) +
  geom_point() +
  labs(x = "Franchise Store Count", y = "Closure Rate", title = "Relationship between Franchise Store Count and Closure Rate") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = max(svdong$FRC_MK_NUM), y = min(svdong$CLS_RATE), 
           label = paste("Correlation =", round(correlation, 2)), hjust = 1)


#----------------------------------------------------------------------------------------------------
#상관관계 분석
library(ggplot2)
library(reshape2)

# 상관계수 행렬 계산
cor_matrix <- cor(svdong[, c("MK_NUM", "SMK_NUM", "OP_MK_NUM", "CLS_MK_NUM", "FRC_MK_NUM", "OP_RATE", "CLS_RATE")])

# 히트맵 시각화
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Variable", y = "Variable", title = "Correlation Heatmap")


