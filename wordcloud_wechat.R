# Setwd your working directoty fisrt

# Install packages needed in this program (refer to Readme.md)
Sys.setenv(JAVA_HOME='input Java directory') 
library(rJava)
library(Rwordseg)
library(wordcloud2)


# Read your data
data<-read.csv("record.csv",stringsAsFactors = FALSE,header = TRUE)

# Clean data 
comp.data<-as.data.frame(data[complete.cases(data),5],stringsAsFactors = FALSE)
colnames(comp.data)<-"Message"
newdata<-as.data.frame(comp.data[!grepl(pattern = "msg",comp.data$Message),],stringsAsFactors = FALSE)
colnames(newdata)<-"Message"

# Split the message into words
insertWords(c())
words.list<-unlist(lapply(newdata, segmentCN)[[1]])
##installDict("G:\\WORDCLOUD\\四十万汉语大词库续一.scel","hanyuciku1")
##installDict("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\中文分词词库整理\\百度分词词库.txt","hanyuciku3")

# Read stopwords
stopwords1<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\哈工大停用词表.txt",quote="")
stopwords2<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\中文停用词库.txt")
stopwords3<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\百度停用词列表.txt",skip=10)
stopwords4<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\四川大学机器智能实验室停用词库.txt")
stopwords12<-merge(stopwords1,stopwords2,all = TRUE)
stopwords34<-merge(stopwords3,stopwords4,all = TRUE)
stopwords<-unique(merge(stopwords12,stopwords34,all = TRUE))

# Function for removing stopwords
removestopwords<-function(x,stopwords){
        temp<-character(0)
        n<-1
        for(n in 1:length(x)){
                if(sum(x[n]==stopwords)<1){
                        temp<-c(temp,x[n])
                }
                n<-n+1
        }
        temp
}

# Remove stopwords
words<-lapply(words.list,removestopwords,stopwords)
t<-as.data.frame(unlist(words))
total<-t[complete.cases(t),]

# Sorting my words 
wordsNum<-sort(table(total),decreasing = TRUE)
wordsdf<-data.frame(wordsNum)
wordsdf$total<-as.character(wordsdf$total)

# Take the top 200 words to make a wordcloud
finaldata <- wordsdf[1:200,] 
finaldata[1,2]<-finaldata[2,2]+500  # In case that the first one is extremely larger than the second word

# Form the wordcloud in specific shape, e.g picture
picture<-system.file("examples/shibainu.png",package = "wordcloud2") 
result<-wordcloud2(finaldata, size = 1, color = 'random-light',    
                   backgroundColor = "gray", fontFamily = "微软雅黑",figPath = picture)  
