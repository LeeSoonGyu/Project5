# 사례연구 5 텍스트 데이터 분석 -남원식
library(KoNLP)
library(tm)
library(wordcloud)
library(Sejong)
library(arules)
library(wordcloud2)
library(igraph)
library(httr)
library(XML)
rm(list = ls())
setwd("c:/Rwork/data")
lincoln <- file("lincoln.txt", encoding = "UTF-8")
lincoln_l <- readLines(lincoln)
lincoln_l
# 단어 추출 사용자 정의함수로 만들기
exNouns <- function(x){paste(extractNoun(as.character(x)),
                             collapse = " ")}
#### (1)제공된 데이터를 이용하여 토픽 분석을 실시하여 단어구름으로 시각화 하고 단어 출현 빈도수를 기반하여 어떤 단어 들이 주요 단어인지 설명하시오.####
lincoln_word <- sapply(lincoln_l, exNouns)
lincoln_word
# 전처리
myCorpus <- Corpus(VectorSource(lincoln_word))
# 문장부호 제거
lincoln_pre <-  tm_map(myCorpus,removePunctuation)
#  수치제거
lincoln_pre <-  tm_map(lincoln_pre,removeNumbers)
# 전처리된 단어들 2~8음절인 단어만 추려내기
mylincoln_pre <- TermDocumentMatrix(lincoln_pre,control = list(wordLengths = c(4,16)))
# matrix형을 dataframe형으로 변환
mylincoln_pre_df <- as.data.frame(as.matrix(mylincoln_pre))
dim(mylincoln_pre_df)
# 단어구름으로 시각화 하기위해 빈도수 구하기
# 워드클라우드1
lincoln_word <- sort(rowSums(mylincoln_pre_df),decreasing = TRUE) # 내림차순으로 순서정렬
wordcount <- table(lincoln_word) # 빈도수 wordcount 에저장
myName <- names(lincoln_word) # 변수명 myName에 저장
word.df <- data.frame(word = myName, freq = lincoln_word) # 빈도수와 변수명을 데이터프레임 word.df로 생성
pal <- brewer.pal(10, "Paired")
wordcloud(word.df$word, word.df$freq, scale = c(3,0.5), 
          min.freq = 2, random.order = F, 
          rot.per = .1, colors = pal)
# 워드 클라우드 2
wc <- wordcloud2(data = word.df,size = 1.5,color = "random-light",backgroundColor = "black",rotateRatio = 0.75)

#### (2)제공된 데이터를 이용하여 연관어 분석을 실시하여 연관어를 시각화 하고 시각화 결과에 대해 설명하시오 ####
# 줄별로 단어 추출
l_word <- Map(extractNoun,lincoln_l)
length(l_word)
# 중복단어 제거하기 
l_word <- unique(l_word)
# 2 ~ 4 글자의 한글만 추출하는 필터
filter1 <- function(x) {nchar(x) <= 4&& nchar(x) >= 2&& is.hangul(x)}
filter2 <- function(x) {Filter(filter1,x)}
l_word <- sapply(l_word,filter2)
l_word
# 트랜잭션 생성하기 
l_wordtran <- as(l_word,"transactions")
l_wordtran
# 연관어 분석
l_wordtran_rule <- apriori(l_wordtran,
                           parameter = list(supp = 0.05,conf = 0.25))
detach(package:tm ,unload = TRUE)
inspect(l_wordtran_rule)
l_rules <- labels(l_wordtran_rule, ruleSep = " ")
rules <- sapply(l_rules, strsplit, " ", USE.NAMES = F)
rulemat <- do.call("rbind", rules)
ruleg <- graph.edgelist(rulemat[,], directed = F)
plot.igraph(ruleg, vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')

#### (3)
#### (3)
#### 뉴스(https://news.daum.net/) 전처리 ####
#install.packages("httr")
#library(httr)
#install.packages("XML")
#library(XML)
url <- "http://news.daum.net/"
web <- GET(url)
web
# HTML 파싱
html <- htmlTreeParse(web,useInternalNodes = T,trim =T,encoding = "UTF-8")
rootNode <- xmlRoot(html)
# 태그자료 수집
news <- xpathApply(rootNode,"//a[@class = 'link_txt']",xmlValue)
# 전처리
news_pre <- gsub("[\r\n\t]", ' ', news) #줄바꿈
news_pre <- gsub('[[:punct:]]', ' ', news_pre) #특수문자
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
news_pre <- gsub('\\d+', ' ', news_pre)  #숫자 
news_pre <- gsub('[a-z]+', ' ', news_pre)#소문자
news_pre <- gsub('[A-Z]+', ' ', news_pre)#대문자
news_pre <- gsub('\\s+', ' ', news_pre) #빈칸
news_pre # 1~ 46번까지만 기사내용임
news_data <- news_pre[1:46]
# 데이터 저장
setwd("C:/Rwork/Data")
write.csv(news_data, "reserch5.csv", quote = F)
#### (3) 다음 포털사이트의 실시간 뉴스(https://news.daum.net/)를 수집하고 실시간 토픽분석을 실행하여 단어구름으로 시각화 하고, 분석 시점에서 주요 이슈가 무엇인지 설명하시오.####
# 전처리한 파일 토픽분석하기
exNouns <- function(x){paste(extractNoun(as.character(x)),
                             collapse = " ")}
news_word <- sapply(news_data,exNouns)
# 말뭉치 생성후 집계행렬 만들기
newsCorpus <- Corpus(VectorSource(news_word))
# 집계행렬 생성
news_TDM <- TermDocumentMatrix(newsCorpus,control = list(wordlengths = c(4,16)))
# 데이터프레임 변경
news_TDM_df <- as.data.frame(as.matrix(news_TDM))
# 단어구름 시각화 위한 빈도수 구하기
word_news <- sort(rowSums(news_TDM_df),decreasing = TRUE)
# 변수와 빈도수 데이터 프레임 만들기
news_name <-  names(word_news)
news.df <- data.frame(word = news_name,freq=word_news)
# 클라우드 생성
news_cloud2 <- wordcloud2(data = news.df,size = 0.75,color = "random-light",backgroundColor = "black",rotateRatio = 0.75)
news_cloud2
