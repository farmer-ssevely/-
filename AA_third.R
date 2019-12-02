## Association Analysis (AA)

# install packages and import packages for AA
install.packages("arules")
library(arules)
library(dplyr)
install.packages("arulesViz")
library(arulesViz)

# import data (ice_rich/totrich)
setwd('C:\\Users\\sseve\\Desktop\\datascience\\code\\data')
ice_rich <- read.csv('ice_rich.csv', header = T, fileEncoding = "euc-kr", stringsAsFactors = F)
totrich <- read.csv('totrich.csv', header = T, fileEncoding = "euc-kr", stringsAsFactors = F)

## ice_rich analysis
head(ice_rich)
str(ice_rich)
ice_rich <- ice_rich[,2:length(ice_rich)]

# make shopping basket
ice_rich_basket <- ice_rich %>%
  group_by(receipt_no) %>% 
  distinct(ice_rich, item_nm)
ice_rich_basket <- as.data.frame(ice_rich_basket)
receipt <- as.vector(unlist(distinct(ice_rich_basket, receipt_no)))

ice_rich_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(ice_rich_basket[ice_rich_basket$receipt_no==num,]['item_nm']))
  ice_rich_shopping_basket <- append(ice_rich_shopping_basket, list(vec))
}

# 연관성 규칙 구하기
ice_rich_buyItemStr <-as(ice_rich_shopping_basket, "transactions")
ice_rich_buyItemStr
inspect(ice_rich_buyItemStr)

item_len = dim(ice_rich)[1]
t_len = length(unique(ice_rich$item_nm))
one_item = item_len / t_len
one_item / item_len

# 결과 보기
# ice_rich_buyItemResult1 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.001, confidence = 0.5))
# ice_rich_buyItemResult2 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.1, confidence = 0.6))
# ice_rich_buyItemResult3 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.1, confidence = 0.7))
# ice_rich_buyItemResult4 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.1, confidence = 0.8))
# ice_rich_buyItemResult5 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.1, confidence = 0.9))
# ice_rich_buyItemResult6 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.1, confidence = 0.95))
# ice_rich_buyItemResult7 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.1, confidence = 0.99))
# 
# ice_rich_buyItemResult8 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.5))
# ice_rich_buyItemResult9 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.6))
# ice_rich_buyItemResult10 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.7))
# ice_rich_buyItemResult11 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.8))
# ice_rich_buyItemResult12 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.9))
# ice_rich_buyItemResult13 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.95))
# ice_rich_buyItemResult14 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.99))
# 
# inspect(ice_rich_buyItemResult1)
# inspect(ice_rich_buyItemResult4)
# inspect(ice_rich_buyItemResult5)
# inspect(ice_rich_buyItemResult8)
# 
# ice_rich_buyItemResult15 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0005, confidence = 0.5))
# inspect(ice_rich_buyItemResult15)
# ice_rich_buyItemResult16 <- apriori(ice_rich_buyItemStr, parameter = list(support = 0.0003, confidence = 0.5))

itemFrequencyPlot(ice_rich_buyItemStr, support=0.03, col="lightpink", main="많이 사먹는 친구들")

# 네트워크 그래프
ice_rich_order <- sort(ice_rich_buyItemResult15, by=c("support","lift","confidence"))
plot(ice_rich_order, method = "graph")

## totrich analysis
head(totrich)
str(totrich)
totrich <- totrich[,2:length(totrich)]

# make shopping basket
totrich_basket <- totrich %>%
  group_by(receipt_no) %>% 
  distinct(totrich, item_nm)
totrich_basket <- as.data.frame(totrich_basket)
receipt <- as.vector(unlist(distinct(totrich_basket, receipt_no)))

totrich_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(totrich_basket[totrich_basket$receipt_no==num,]['item_nm']))
  totrich_shopping_basket <- append(totrich_shopping_basket, list(vec))
}

item_len = dim(totrich)[1]
t_len = length(unique(totrich$item_nm))
one_item = item_len / t_len
one_item / item_len

# 연관성 규칙 구하기
totrich_buyItemStr <-as(totrich_shopping_basket, "transactions")
totrich_buyItemStr
inspect(totrich_buyItemStr)

# 결과 보기
totrich_buyItemResult1 <- apriori(totrich_buyItemStr, parameter = list(support = 0.3, confidence = 0.5))
totrich_buyItemResult2 <- apriori(totrich_buyItemStr, parameter = list(support = 0.001, confidence = 0.6))
totrich_buyItemResult3 <- apriori(totrich_buyItemStr, parameter = list(support = 0.001, confidence = 0.7))
totrich_buyItemResult4 <- apriori(totrich_buyItemStr, parameter = list(support = 0.3, confidence = 0.8))
totrich_buyItemResult5 <- apriori(totrich_buyItemStr, parameter = list(support = 0.3, confidence = 0.9))
totrich_buyItemResult6 <- apriori(totrich_buyItemStr, parameter = list(support = 0.4, confidence = 0.95))
totrich_buyItemResult7 <- apriori(totrich_buyItemStr, parameter = list(support = 0.001, confidence = 0.99))

totrich_buyItemResult8 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.5))
totrich_buyItemResult9 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.6))
totrich_buyItemResult10 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.7))
totrich_buyItemResult11 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.8))
totrich_buyItemResult12 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.9))
totrich_buyItemResult13 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.95))
totrich_buyItemResult14 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0007, confidence = 0.99))

inspect(totrich_buyItemResult1)
inspect(totrich_buyItemResult4)
inspect(totrich_buyItemResult6)
inspect(totrich_buyItemResult8)

# 메로나 확인
totrich_buyItemResult15 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0005, confidence = 0.5))
inspect(totrich_buyItemResult16)
totrich_buyItemResult16 <- apriori(totrich_buyItemStr, parameter = list(support = 0.0003, confidence = 0.5))
inspect(subset(totrich_buyItemResult16, subset=lhs %pin% "메로나"))

# 시각화
itemFrequencyPlot(totrich_buyItemStr, support=0.03, col="lightpink", main="많이 사먹는 친구들")

# 네트워크 그래프
totrich_order <- sort(totrich_buyItemResult15, by=c("support","lift","confidence"))
plot(totrich_order, method = "graph")


## mintae rich
