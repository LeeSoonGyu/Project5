# 사례연구 5 이순규
# install.packages("igraph", type = "binary") # igraph 미작동시 설치방법
# install.packages("arules")
# install.packages('backports')
# install.packages('shiny') # worldcloud2 오류시 쓰이는 패키지
# devtools::install_github("lchiffon/wordcloud2")
library(KoNLP)
library(tm)
library(wordcloud)
library(devtools)
library(wordcloud2)
library(arules)
library(backports)
library(igraph)
library(httr)
library(XML)
library(stringr)

rm(list = ls())
getwd()
setwd('E:/')
Lincoln <- file('Lincoln.txt', encoding = "UTF-8")
Lincoln_data <- readLines(Lincoln)
head(Lincoln_data)

Lincoln_data <- str_replace_all(Lincoln_data, '["들이", "하게"]', ' ')

exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse = " ") }

## 1번 : 링컨연설 분석 ##

# 단어 추출 
Lincoln_nouns <- sapply(Lincoln_data, exNouns)
Lincoln_nouns[3]

# 추출된 단어를 이용하여 말뭉치(Corpus) 생성 및 전처리
myCorpus <- Corpus(VectorSource(Lincoln_nouns))
myCorpusPrepro <- tm_map(myCorpus, removePunctuation) # 문장부호 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers) # 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower) # 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english')) # 불용어 제거
inspect(myCorpusPrepro[1:5]) # 전처리 결과 확인

# 전처리된 단어집에서 2 ~ 8 음절 단어 대상 선정 
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro, 
                     control = list(wordLengths = c(4, 16)))

myCorpusPrepro_term

# matrix 자료구조를 data.frame 자료구조로 변경
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)

# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]

# 단어 이름과 빈도수로 data.frame 생성
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
str(word.df)
# 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")
# 단어 구름 시각화
wordcloud(word.df$word, word.df$freq, scale = c(5, 1), 
          min.freq = 2, random.order = F, 
          rot.per = .1, colors = pal, family = "malgun")
# worldcloud2 이용
myName <- names(wordResult)
df <- data.frame(word = myName, freq = wordResult)
head(df, 50)
wordcloud2(data = df , fontFamily = "나눔바른고딕")

# 2번 : 링컨 연관어 분석

# 줄 단위 단어 추출
lword <- Map(extractNoun, Lincoln_data)
length(lword)
lword <- unique(lword)
length(lword)

# 중복 단어 제거와 추출 단어 확인
lword <- sapply(lword, unique)
length(lword)
lword

# 단어 필터링 함수 정의
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) { Filter(filter1, x) }

# 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword

# 트랜잭션 생성
wordtran <- as(lword, "transactions")
wordtran

tranrules <- apriori(wordtran, 
                     parameter = list(supp = 0.15, conf = 0.99)) # 변동을 줘보자
tranrules

# 연관규칙 생성 결과보기
detach(package:tm, unload = TRUE)
inspect(tranrules)

# 연관단어 시각화를 위해서 자료구조 변경
rules <- labels(tranrules, ruleSep = " ")
rules

# 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules

# 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)

# edgelist 보기
ruleg <- graph.edgelist(rulemat[c(1:22), ], directed = F)
ruleg

# 시각화
par(family="NanumGothicBold")
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2,
            vertex.label.color = 'black',
            vertex.size = 20, 
            vertex.color = 'pink',
            vertex.frame.color = 'red')

# 3. 뉴스 토픽분석

# 실시간 뉴스 데이터 불러오기
url <- "http://news.daum.net"
web <- GET(url)
web

# html 파싱

html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)

# 태그 자료 수집하기
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
news

# 자료 전처리하기
news_pre <- gsub("[\r\n\t]", ' ', news)
news_pre <- gsub('[[:punct:]]', ' ', news_pre)
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
news_pre <- gsub('\\d+', ' ', news_pre)
news_pre <- gsub('[a-z]+', ' ', news_pre)
news_pre <- gsub('[A-Z]+', ' ', news_pre)
news_pre <- gsub('\\s+', ' ', news_pre)

news_pre

# 기사와 관계 없는 내용 제거
news_data <- news_pre[1:59]
news_data

# 파일 저장 및 읽기

setwd('C:/rwork')
write.csv(news_data, "news_data.csv", quote = F)
news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)
str(news_data)
names(news_data) <- c("no", "news_text")
head(news_data)
news_text <- news_data$news_text
news_text

# 세종 사전에 단어 추가
user_dic <- data.frame(term = c("코스피", "코스닥", "상승", "포인트", "하락"), tag = 'ncn')
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

# 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(x), collapse = " ")}

# exNouns() 함수를 이용하어 단어 추출
news_nouns <- sapply(news_text, exNouns)
news_nouns

# 추출 결과 확인
str(news_nouns)

# 추출된 단어를 이용한 말뭉치(corpus) 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5])

# 단어 vs 문서 집계 행렬 만들기
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 16)))
TDM

# matrix 자료구조를 data.frame 자료구조로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df )

# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:10]

# 단어 이름 추출
myNames <- names(wordResult)
myNames

# 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df)

# 단어 구름 생성
pal <- brewer.pal(12, "Paired")
wordcloud(df$word, df$freq, min.freq = 2,
          random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family = "malgun")

# worldcloud2 사용
wc2data <- data.frame(df$word, df$freq)
wc2data
wordcloud2(data=wc2data, size=0.8, color='random-dark')
wordcloud2(data=wc2data, size=0.8, color='random-light', backgroundColor = "black")