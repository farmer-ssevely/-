## Association Analysis (AA)

# install packages and import packages for AA
install.packages("arules")
library(arules)
library(dplyr)
install.packages("arulesViz")
library(arulesViz)

# import data (night/day)
setwd('C:\\Users\\sseve\\Desktop\\datascience\\code\\data')
night <- read.csv('night.csv', header = T, fileEncoding = "euc-kr", stringsAsFactors = F)
day <- read.csv('day.csv', header = T, fileEncoding = "euc-kr", stringsAsFactors = F)

## night analysis
head(night)
str(night)
night <- night[,2:length(night)]

# make shopping basket
night_basket <- night %>%
  group_by(receipt_no) %>% 
  distinct(night, item_nm)
night_basket <- as.data.frame(night_basket)
receipt <- as.vector(unlist(distinct(night_basket, receipt_no)))

night_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(night_basket[night_basket$receipt_no==num,]['item_nm']))
  night_shopping_basket <- append(night_shopping_basket, list(vec))
}

# 연관성 규칙 구하기
night_buyItemStr <-as(night_shopping_basket, "transactions")
night_buyItemStr
inspect(night_buyItemStr)

item_len = dim(night)[1]
t_len = length(unique(night$item_nm))
one_item = item_len / t_len
one_item / item_len

# 결과 보기
# night_buyItemResult1 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.5))
# night_buyItemResult2 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.6))
# night_buyItemResult3 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.7))
# night_buyItemResult4 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.8))
# night_buyItemResult5 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.9))
# night_buyItemResult6 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.95))
# night_buyItemResult7 <- apriori(night_buyItemStr, parameter = list(support = 0.001, confidence = 0.99))
# 
# night_buyItemResult8 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.5))
# night_buyItemResult9 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.6))
# night_buyItemResult10 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.7))
# night_buyItemResult11 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.8))
# night_buyItemResult12 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.9))
# night_buyItemResult13 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.95))
# night_buyItemResult14 <- apriori(night_buyItemStr, parameter = list(support = 0.0007, confidence = 0.99))
# 
# inspect(night_buyItemResult1)
# inspect(night_buyItemResult4)
# inspect(night_buyItemResult5)
# inspect(night_buyItemResult8)
# 
night_buyItemResult15 <- apriori(night_buyItemStr, parameter = list(support = 0.0005, confidence = 0.5))
night_buyItemResult <- sort(night_buyItemResult15, by='lift')
inspect(night_buyItemResult)
night_buyItemResult16 <- apriori(night_buyItemStr, parameter = list(support = 0.0003, confidence = 0.5))

itemFrequencyPlot(night_buyItemStr, support=0.03, col="lightpink", main="많이 사먹는 친구들")

# 네트워크 그래프
night_order <- sort(night_buyItemResult15, by=c("support","lift","confidence"))
plot(night_order, method = "graph")

plot(night_order)

## day analysis
head(day)
str(day)
day <- day[,2:length(day)]

# make shopping basket
day_basket <- day %>%
  group_by(receipt_no) %>% 
  distinct(day, item_nm)
day_basket <- as.data.frame(day_basket)
receipt <- as.vector(unlist(distinct(day_basket, receipt_no)))

day_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(day_basket[day_basket$receipt_no==num,]['item_nm']))
  day_shopping_basket <- append(day_shopping_basket, list(vec))
}

item_len = dim(day)[1]
t_len = length(unique(day$item_nm))
one_item = item_len / t_len
one_item / item_len

# 연관성 규칙 구하기
day_buyItemStr <-as(day_shopping_basket, "transactions")
day_buyItemStr
inspect(day_buyItemStr)

# 결과 보기
# day_buyItemResult1 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.5))
# day_buyItemResult2 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.6))
# day_buyItemResult3 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.7))
# day_buyItemResult4 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.8))
# day_buyItemResult5 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.9))
# day_buyItemResult6 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.95))
# day_buyItemResult7 <- apriori(day_buyItemStr, parameter = list(support = 0.001, confidence = 0.99))
# 
# day_buyItemResult8 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.5))
# day_buyItemResult9 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.6))
# day_buyItemResult10 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.7))
# day_buyItemResult11 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.8))
# day_buyItemResult12 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.9))
# day_buyItemResult13 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.95))
# day_buyItemResult14 <- apriori(day_buyItemStr, parameter = list(support = 0.0007, confidence = 0.99))
# 
# inspect(day_buyItemResult1)
# inspect(day_buyItemResult4)
# inspect(day_buyItemResult5)
# inspect(day_buyItemResult8)
# 
# 메로나 확인
day_buyItemResult15 <- apriori(day_buyItemStr, parameter = list(support = 0.0005, confidence = 0.5))
inspect(sort(day_buyItemResult15, by='lift'))
day_buyItemResult16 <- apriori(day_buyItemStr, parameter = list(support = 0.0003, confidence = 0.5))
inspect(subset(day_buyItemResult16, subset=lhs %pin% "메로나"))

# 시각화
itemFrequencyPlot(day_buyItemStr, support=0.03, col="lightpink", main="많이 사먹는 친구들")

# 네트워크 그래프
day_order <- sort(day_buyItemResult15, by=c("support","lift","confidence"))
plot(day_order, method = "graph")
