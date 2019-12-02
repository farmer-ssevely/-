## Association Analysis (AA)

# install packages and import packages for AA
install.packages("arules")
library(arules)
library(dplyr)
install.packages("arulesViz")
library(arulesViz)
install.packages("visNetwork")
library(visNetwork)
library(igraph)

# import data (multi/only)
setwd('C:\\Users\\sseve\\Desktop\\datascience\\code\\data')
multi <- read.csv('multi.csv', header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
only <- read.csv('only.csv', header = T, fileEncoding = "euc-kr", stringsAsFactors = F)

total <- rbind(multi, only)

## multi analysis
head(multi)
head(only)
str(multi)
multi <- multi[,2:length(multi)]

# make shopping basket
multi_basket <- multi %>%
  group_by(receipt_no) %>% 
  distinct(multi, item_nm)
multi_basket <- as.data.frame(multi_basket)
receipt <- as.vector(unlist(distinct(multi_basket, receipt_no)))

multi_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(multi_basket[multi_basket$receipt_no==num,]['item_nm']))
  multi_shopping_basket <- append(multi_shopping_basket, list(vec))
}

# 연관성 규칙 구하기
multi_buyItemStr <-as(multi_shopping_basket, "transactions")
multi_buyItemStr
inspect(multi_buyItemStr)

item_len = dim(multi)[1]
t_len = length(unique(multi$item_nm))
one_item = item_len / t_len
one_item / item_len

# 결과 보기
# multi_buyItemResult1 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.5))
# multi_buyItemResult2 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.6))
# multi_buyItemResult3 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.7))
# multi_buyItemResult4 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.8))
# multi_buyItemResult5 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.9))
# multi_buyItemResult6 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.95))
# multi_buyItemResult7 <- apriori(multi_buyItemStr, parameter = list(support = 0.001, confidence = 0.99))
# 
# multi_buyItemResult8 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.5))
# multi_buyItemResult9 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.6))
# multi_buyItemResult10 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.7))
# multi_buyItemResult11 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.8))
# multi_buyItemResult12 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.9))
# multi_buyItemResult13 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.95))
# multi_buyItemResult14 <- apriori(multi_buyItemStr, parameter = list(support = 0.0007, confidence = 0.99))
# 
# inspect(multi_buyItemResult1)
# inspect(multi_buyItemResult4)
# inspect(multi_buyItemResult5)
# inspect(multi_buyItemResult8)

# 메로나 확인
multi_buyItemResult15 <- apriori(multi_buyItemStr, parameter = list(support = 0.0005, confidence = 0.5))
multi_buyItemResult15 <- sort(multi_buyItemResult15, by='lift')
# inspect(multi_buyItemResult15)
# multi_buyItemResult16 <- apriori(multi_buyItemStr, parameter = list(support = 0.0003, confidence = 0.5))
inspect(subset(multi_buyItemResult15, subset=rhs %pin% "코카콜라"))
inspect(multi_buyItemResult15[1:20])
# 시각화
itemFrequencyPlot(multi_buyItemStr, support=0.03, col="lightpink", main="다른 항목과 많이 사먹는 친구들")

# 네트워크 그래프
multi_order <- sort(multi_buyItemResult15, by=c("support","lift","confidence"))
plot(multi_order, method = "graph")
plot(multi_buyItemResult15)
plot(multi_buyItemResult15, method = "grouped")
plot(multi_buyItemResult15, method = "graph", control = list(type = "names"))
plot(multi_buyItemResult15, method = "paracoord", control = list(reorder = TRUE))
plot(multi_buyItemResult15, method = "graph", control = list(type="items"))
plot(multi_buyItemResult15, measure = c("support", "lift"), shading = "confidence")
submulti_buyItemResult15 <- head(sort(multi_buyItemResult15, by="lift"), 20) ## lift 기준으로 상위 20개만을 시각화
ig <- plot(submulti_buyItemResult15, method="graph", control=list(type="items") )

# saveAsGraph seems to render bad DOT for this case
tf <- tempfile( )
saveAsGraph(submulti_buyItemResult15, file = tf, format = "dot" )
ig_df <- get.data.frame(ig, what = "both")
visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name
    ,value = ig_df$vertices$support # could change to lift or confidence
    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
    ,ig_df$vertices
    ,color = "orange"
  )
, edges = ig_df$edges
, main = "icecream & other items network (아이스크림 & 다른 품목)"
) %>%
  visEdges(ig_df$edges) %>%
  visOptions(highlightNearest = T )


## only analysis
head(only)
str(only)
only <- only[,2:length(only)]

# make shopping basket
only_basket <- only %>%
  group_by(receipt_no) %>% 
  distinct(only, item_nm)
only_basket <- as.data.frame(only_basket)
receipt <- as.vector(unlist(distinct(only_basket, receipt_no)))

only_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(only_basket[only_basket$receipt_no==num,]['item_nm']))
  only_shopping_basket <- append(only_shopping_basket, list(vec))
}

# 연관성 규칙 구하기
only_buyItemStr <-as(only_shopping_basket, "transactions")
only_buyItemStr
inspect(only_buyItemStr)

item_len = dim(only)[1]
t_len = length(unique(only$item_nm))
one_item = item_len / t_len
one_item / item_len

# 결과 보기
# only_buyItemResult1 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.5))
# only_buyItemResult2 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.6))
# only_buyItemResult3 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.7))
# only_buyItemResult4 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.8))
# only_buyItemResult5 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.9))
# only_buyItemResult6 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.95))
# only_buyItemResult7 <- apriori(only_buyItemStr, parameter = list(support = 0.001, confidence = 0.99))
# 
# only_buyItemResult8 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.5))
# only_buyItemResult9 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.6))
# only_buyItemResult10 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.7))
# only_buyItemResult11 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.8))
# only_buyItemResult12 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.9))
# only_buyItemResult13 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.95))
# only_buyItemResult14 <- apriori(only_buyItemStr, parameter = list(support = 0.0007, confidence = 0.99))
# 
# inspect(only_buyItemResult1)
# inspect(only_buyItemResult4[1:6])
# inspect(only_buyItemResult5[1:6])
# inspect(only_buyItemResult8)

# 메로나 확인
only_buyItemResult15 <- apriori(only_buyItemStr, parameter = list(support = 0.005, confidence = 0.5))
only_buyItemResult15 <- sort(only_buyItemResult15, by='lift')
inspect(only_buyItemResult15)
# only_buyItemResult16 <- apriori(only_buyItemStr, parameter = list(support = 0.0003, confidence = 0.5))
# inspect(subset(only_buyItemResult16, subset=lhs %pin% "메로나"))

# 시각화
itemFrequencyPlot(only_buyItemStr, support=0.03, col="lightpink", main="많이 사먹는 친구들")

# 네트워크 그래프
only_order <- sort(only_buyItemResult15, by=c("support","lift","confidence"))
plot(only_order, method = "graph")

# total
# make shopping basket
total_basket <- total %>%
  group_by(receipt_no) %>% 
  distinct(total, item_nm)
total_basket <- as.data.frame(total_basket)
receipt <- as.vector(unlist(distinct(total_basket, receipt_no)))

total_shopping_basket <- list()
for (num in receipt){
  vec <- as.vector(unlist(total_basket[total_basket$receipt_no==num,]['item_nm']))
  total_shopping_basket <- append(total_shopping_basket, list(vec))
}

# 연관성 규칙 구하기
total_buyItemStr <-as(total_shopping_basket, "transactions")
total_buyItemStr
inspect(total_buyItemStr)

item_len = dim(total)[1]
t_len = length(unique(total$item_nm))
one_item = item_len / t_len
one_item / item_len

total_buyItemResult <- apriori(total_buyItemStr, parameter = list(support = 0.0005, confidence = 0.5))
total_buyItemResult <- sort(total_buyItemResult, by='lift')
inspect(total_buyItemResult)
