---
title: "nlp_tw"
author: "haiyan yu"
date: "2/21/2020"
output:
  pdf_document: default
  html_document: default
---




```r
library(tm)
```

```
## Loading required package: NLP
```

```r
library(NLP)
library(jiebaR)
```

```
## Loading required package: jiebaRD
```

```r
library(plyr)
library(readxl)
library(HMM)
#library(xlsx)
library(Rwordseg)
```

```
## Loading required package: tmcn
```

```
## # tmcn Version: 0.2-13
```

```
## HMM model has been loaded.
```

```
## # 
## The defalut analyzer is 'hmm' implemented by native R codes, which is still in development.
```

```
## If you want to improve the performance you can choose:
```

```
##   - "jiebaR", a popular segmentation module, by running "setAnalyzer('jiebaR')".
```

```
##   - "coreNLP", a R wrappers around Stanford CoreNLP, by running "setAnalyzer('coreNLP')".
```

```
##   - "fmm", the easiest way of using forward maximum matching algorithm, by running "setAnalyzer('fmm')".
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidytext)
library(ggplot2)
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following object is masked from 'package:NLP':
## 
##     annotate
```

```r
library(stringr)
library(igraph)
```

```
## 
## Attaching package: 'igraph'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     as_data_frame, groups, union
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
library(ggraph)
library(tidyr)
```

```
## 
## Attaching package: 'tidyr'
```

```
## The following object is masked from 'package:igraph':
## 
##     crossing
```
#二、计算词频表

```r
#加载文件
file_1<-read.csv("~/Downloads/Rpkg/nlph/data/merge.csv",
                 header = TRUE, sep = ",", quote="\"", encoding = "UTF-8",
                 stringsAsFactors = TRUE)
#将text列数据转成character类型
file_1$text <- as.character(file_1$text)
txtFile<-file_1$text
engin1<-worker()
#对文本进行分词处理
segWords<-segment(txtFile,engin1)
#停止词的处理
mystopwords<- readLines('~/Downloads/Rpkg/nlph/data/en-stopword.txt')
segWords<-removeWords(segWords,mystopwords)
#去除数字和标点符号
segWords<-gsub('\\d|\\.|,|\\!|:|;|\\?',"",segWords)
#到此为止，清洗完成
sre<-sort(table(segWords),decreasing = TRUE)
#输出组合后的文件wordFreq.csv到文件夹
#write.csv(sre,file = "~/Downloads/Rpkg/nlph/data/wordFreq2.csv",row.names=F)

sre.df<-data.frame(sre)
head(sre.df[sre.df$Freq>500,])
```

```
##      segWords  Freq
## 1             93186
## 2 coronavirus  7373
## 3       China  5075
## 4    outbreak  2784
## 5       Wuhan  2261
## 6      people  1036
```

```r
sredf.df <- data.frame(stringsAsFactors=FALSE,
                 word = sre.df$segWords,
                 freq = sre.df$Freq)
# "1" blank
sredf.df<-sredf.df[2:30,]
library(ggplot2)

ggplot(sredf.df, aes(x = reorder(word, freq), y = freq)) +
    geom_col() +
    labs(title="Twitter ",
         x = NULL,
         y = "Frequency") +
    coord_flip()
```

![](nlp_tw_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 




#三、知识网络图

```r
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
```

```
##         word1       word2    n
## 1 coronavirus    outbreak 1817
## 2       wuhan coronavirus  698
## 3       death        toll  523
## 4       china coronavirus  421
## 5      deadly coronavirus  277
## 6       world        news  235
```

```r
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(austen_bigrams_p, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5) +
  theme_void()
```

![](nlp_tw_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

#情感：量化

```r
#读入数据，为了代码简便，我直接在原数据文件中删除了其他列，只留下了username和text列，保存为emo_merge.xlsx
xle<-read_excel("~/Downloads/Rpkg/nlph/data/emo_merge.xlsx")
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
```

```
## Joining, by = "word"
```

```r
#write.csv(afin,file = '~/Downloads/Rpkg/nlph/data/percent-emo2.csv',row.names=F)


afin.df <- data.frame(stringsAsFactors=FALSE,
                 word = afin$USERNAME,
                 freq =afin$sentiment)

library(ggplot2)

ggplot(afin.df, aes(x = reorder(word, freq), y = freq)) +
    geom_col() +
    labs(title="Twitter ",
         x = NULL,
         y = "Sentiment") +
    coord_flip()
```

![](nlp_tw_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 
