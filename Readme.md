Packages needed in this project
===============================

Firstly, here are some packages which should be installed to run the project.

``` r
library(rJava)
install.packages(Rwordseg)
install.packages(wordcloud2)
```

If you could not load the `rJava` package in R, please check if you have already installed the Java in your computer. If you have installed it, please check if it fits the R/Rstudio version/system and reinstall it if you need to. If not, you need to install the latest version of Java before you load the `rJava` package in R.

Here is the website that you can get the latest version of Java:

**Access to the latest version of Java** <https://www.java.com/en/download/manual.jsp>

After installing the Java, run the following code before you load the 'rJava':

``` r
# The directory depends on where you install the Java
Sys.setenv(JAVA_HOME='C:\Program Files\Java\jre7') # for 64-bit version 
Sys.setenv(JAVA_HOME='C:\Program Files (x86)\Java\jre7') # for 32-bit version
```

Reference: <https://bbs.pinggu.org/thread-2522933-1-1.html>

Since many users(including me) find that the `wordcloud2` package installed from cran has problem returning the result that users expect, and it seems the author has not solved this problem yet. Here, I recommand you to install `wordcloud2`package from github.

``` r
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
```

Reference: <https://github.com/Lchiffon/wordcloud2/issues/12>

Get data and clean data
=======================

- Get data
----------

We only introduce the method of getting data from wechat(ios).

1.  Tools you need:

-   **iTunes**
-   **pp助手**
-   **sqlcipher.exe**
-   **Excel**

1.  Use iTunes to backup your iPhone

2.  Connect your iPhone with PP助手

3.  Choose **备份还原** - **查看**- Enter `AppDomain-com.tencent.xin:/`- Search `MM.sqlite`file - Export it to local

4.  Open `MM.sqlite` with **sqlcipher.exe** and export the table you want in `.txt` file. Then, turn `.txt` file into `.csv` file with **Excel**.

Reference: <https://zhuanlan.zhihu.com/p/28935173>

- Clean data
------------

``` r
data<-read.csv("Messages.csv",stringsAsFactors = FALSE,header = TRUE)
# Remove NaNs and useless data
comp.data<-as.data.frame(data[complete.cases(data),5],stringsAsFactors = FALSE)
colnames(comp.data)<-"Message"
newdata<-as.data.frame(comp.data[!grepl(pattern ="msg",comp.data$Message),],stringsAsFactors = FALSE)
colnames(newdata)<-"Message"
```

Split Messages into words
-------------------------

In this section, you need to download some **Thesaurus** like `四十万汉语大词库续一.scel`. The effect of this process depends on your **Thesaurus**. So, try more **Thesaurus** and adjust them with `insertWords` until you get a reasonable result.

`insertWords()`: insert words which are not included in Thaesaurus.

``` r
installDict("G:\\WORDCLOUD\\四十万汉语大词库续一.scel","hanyuciku1")
installDict("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\中文分词词库整理\\百度分词词库.txt","hanyuciku3")
insertWords(c("好鸭","笨笨"))  #insert words I want so it won't be split into "好""鸭""笨""笨" :)
words.list<-unlist(lapply(newdata, segmentCN)[[1]]) 
```

Remove stopwords
----------------

Similar with splitting messages into words, you will need some stopwords list as something like reference to tell the computer which words should be removed.

``` r
stopwords1<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\哈工大停用词表.txt",quote="")
stopwords2<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\中文停用词库.txt")
stopwords3<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\百度停用词列表.txt",skip=10)
stopwords4<-read.table("G:\\WORDCLOUD\\funNLP-master\\funNLP-master\\data\\停用词\\四川大学机器智能实验室停用词库.txt")
stopwords12<-merge(stopwords1,stopwords2,all = TRUE)
stopwords34<-merge(stopwords3,stopwords4,all = TRUE)
stopwords<-unique(merge(stopwords12,stopwords34,all = TRUE))
```

Write a function to remove stopwords.

``` r
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
```

Remove stopwords.

``` r
# Remove stopwords
words<-lapply(words.list,removestopwords,stopwords)
t<-as.data.frame(unlist(words))
total<-t[complete.cases(t),]
```

Sorting words
-------------

``` r
# Sorting my words 
wordsNum<-sort(table(total),decreasing = TRUE)
wordsdf<-data.frame(wordsNum)
wordsdf$total<-as.character(wordsdf$total)
```

Make a wordcloud
----------------

Take the top 200 words to make a wordcloud.

``` r
# Take the top 200 words to make a wordcloud
finaldata <- wordsdf[1:200,] 
finaldata[1,2]<-finaldata[2,2]+500  # In case that the first one is extremely larger than the second word

# Form the wordcloud in specific shape, e.g picture
picture<-system.file("examples/shibainu.png",package = "wordcloud2") 
result<-wordcloud2(finaldata, size = 1, color = 'random-light',    
                   backgroundColor = "gray", fontFamily = "微软雅黑",figPath = picture)  
# help(wordcloud2) for more fun:)
```
