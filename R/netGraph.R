library(tm)
library(NLP)
library(jiebaR)
library(plyr)
library(readxl)
library(HMM)
#library(xlsx)
library(Rwordseg)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(igraph)
library(ggraph)
library(tidyr)
#三、知识网络图
#注：若需要计算其他文件网络图，只需改变read.csv()函数中的文件路径和文件中需要提取的列
#加载文件
file_1<-read.csv("~/Downloads/Rpkg/nlph/data/merge.csv",
                 header = TRUE, sep = ",", quote="\"", encoding = "UTF-8",
                 stringsAsFactors = TRUE)
#读取text列文本
fi_1<-file_1[7]
austen_bigrams <- fi_1 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !str_detect(word1,'https'),!str_detect(word1,'http'),!str_detect(word1,'www.ndtv.com'),
         !str_detect(word2,'https'),!str_detect(word2,'http')
  ) %>%
#  count(word1, word2)
  count(word1, word2, sort = TRUE)
#输出组合后的文件n-gram.csv到文件夹
write.csv(austen_bigrams,
          file = "~/Downloads/Rpkg/nlph/data/n-gram-new.csv",row.names=F)


#画图
set.seed(2016)
austen_bigrams<-read.csv(file = "~/Downloads//Rpkg/nlph/data/n-gram-new.csv")
austen_bigrams_p<-austen_bigrams[austen_bigrams$n>100,]
head(austen_bigrams)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(austen_bigrams_p, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5) +
  theme_void()
