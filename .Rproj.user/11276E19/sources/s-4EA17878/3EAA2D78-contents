library(tm)
library(NLP)
library(jiebaR)
library(plyr)
library(readxl)
library(HMM)
library(xlsx)
library(Rwordseg)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(igraph)
library(ggraph)
library(tidyr)
#一、将所有csv文件合并成一个总文件

#设置根目录
#setwd("C://Users/栗谷/Desktop")
#list.files命令将input文件夹下所有文件名输入a
a <- list.files("/Users/hayyen/Desktop/Yanglu_twitter/twitter292")
#用paste命令构建路径变量dir
dir <- paste("~/Downloads/Rpkg/nlph/data/twitter29/",a,sep="")
#读取dir长度，也就是文件夹下的文件个数
n <- length(dir)
#读入第一个文件内容（可以不用先读一个，但是为了简单，省去定义data.frame的时间，我选择先读入一个文件。
merge.data <- read.csv(file = dir[1],header=T,sep=",")
#循环从第二个文件开始读入所有文件，并组合到merge.data变量中
for (i in 2:n){
  new.data <- read.csv(file = dir[i], header=T, sep=",")
  merge.data <- rbind(merge.data,new.data)
}
#输出组合后的文件merge.csv到文件夹
write.csv(merge.data,file = "~/Downloads/Rpkg/nlph/data/merge2.csv",row.names=F)


#二、计算词频表

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
write.csv(sre,file = "~/Downloads/Rpkg/nlph/data/wordFreq2.csv",row.names=F)




