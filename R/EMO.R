library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(officer)
library(readxl)
#读入数据，为了代码简便，我直接在原数据文件中删除了其他列，只留下了username和text列，保存为emo_merge.xlsx
xle<-read_excel("C://Users/栗谷/Desktop/emo_merge.xlsx")
emo<-xle%>%
  #将此csv文件中的所有单词建立一个整洁的数据框架
  unnest_tokens(word,TEXT)

afin<-emo%>%
  #使用inner_join函数进行词与情感得分的连接
  inner_join(get_sentiments("afinn"))%>%
  #根据进行分类
  group_by(USERNAME)%>%
  #将每类的情感得分/有情感分数的单词字数*100
  summarise(sentiment = sum(value)/nrow(emo)*100)
  
write.csv(afin,file = 'C://Users/栗谷/Desktop/percent-emo.csv',row.names=F)
