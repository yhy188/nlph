library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(officer)
library(readxl)
library(plyr)
library(Rwordseg)
#读入案例数据，由于pdf文档不好操作，因此我把这个文档转为excel格式
xle<-read_excel("~/Downloads//Rpkg/nlph/dataMFS/txtP.xlsx")
#读入自定义词典
mfsWeight <- read_excel("~/Downloads//Rpkg/nlph/dataMFS/MFS.xlsx")

#将案例数据文件中的所有单词建立一个整洁的数据框架
oclient_1<-xle%>%
  unnest_tokens(word,TEXT)

oclient_2 <- xle %>%
  unnest_tokens(word, TEXT, token = "ngrams", n = 2)
#将分词成一个单词的数据框和分成两个单词的数据框进行合并
oclient <- rbind(oclient_1,oclient_2)

#为oclient中可赋值的单词进行赋值
testterm <- join(oclient, mfsWeight)
testterm <- testterm[!is.na(testterm$weight), ]

#根据id分组，把每组所得分值进行相加
omfs <- aggregate(testterm[,3],list(testterm[,1]),sum)
#对得到的数据框的列进行重命名
names(omfs) <- c('id','weight')
#增加新一列，用于显示案例的风险程度
omfs$程度 <- ''

#得到有多少id
mrow <- nrow(omfs)
ovar <- c(1:mrow)
#通过每个id得到的分数进行风险评估
for (i in ovar) {
  if(omfs[i,2]<=24){
    omfs[i,3]<- 'No Risk'
  } else if(omfs[i,2] <=50){
    omfs[i,3]<-'Low Risk'}
  else if(omfs[i,2] >= 51)
    {omfs[i,3] <-'High Risk'}
}
write.csv(omfs,file = "~/Downloads//Rpkg/nlph/dataMFS/client-mfs2.csv",row.names=F)
