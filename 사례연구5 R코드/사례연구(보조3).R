# 사례연구 5 텍스트 데이터 분석 - 문태웅
# 토픽 분석(빈도 분석)
# 설명 : 텍스트 데이터를 대상으로 단어를 추출하고,
# 이를 단어 사전과 비교하여 단어의 출현 빈도수를 분석하는 과정을 의미

#### 1)
### 패키지 준비
install.packages("Sejong")
install.packages("tm")
install.packages("KoNLP")
install.packages("Corpus")
install.packages("wordcloud")
# KoNLP 패키지 오류 시 아래와 같은 방식으로 두 패키지를 설치하고 로드하면 정상 동작함.
install.packages("devtools") 
devtools::install_github("haven-jeon/KoNLP")

### 패키지 로드
library(Sejong)
# 세종 말뭉치 패키지 로드
library(tm)
# 텍스트 분석에 사용되는 패키지
library(KoNLP) 
# 한국어 형태소 분석을 위한 R 패키지이다.
# 크롤링 혹은 워드클라우드를 작성할때 많이 이용한다.
library(Corpus)
# tm 패키지가 로드된 상태에서 로드해야 정상적으로 실행됨.
# Corpus는 데이터의 정제, 통합, 선택, 변환 과정을 구조화된 단계로서 더 이상
# 추가적인 절차 없이 데이터 마이닝 알고리즘 실험에서 활용할 수 있도록 해준다.
library(wordcloud)
# 워드클라우드를 만들 수 있는 패키지 이다.
# 단순히 데이터만 넣어도 간단한 워드클라우드를 만들 수 있으며,
# 포함된 다양한 함수를 활용하여 색이나 모양 등을 자유롭게 설정할 수 있다.

### 자료 가져오기
# 경로 확인 및 설정
getwd() # 디렉토리 경로 확인
setwd("D:/OneDrive/R/dataset") # 경로 재설정
getwd() # 재설정 경로 확인

# 텍스트 파일 불러오기 및 문자 인코딩
Lincoln <- file("Lincoln_Gettysburg_Address2.txt", encoding = "UTF-8")
# 줄 단위 데이터 생성
Lincoln_data <- readLines(Lincoln)
# 데이터 앞 3줄 보기
head(Lincoln_data,3)

### 단어 추출을 위한 사용자 함수 정의하기
# 단계 1: 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse = " ") }

# 단계 2: exNouns() 함수를 이용하여 단어 추출
Lincoln_nouns <- sapply(Lincoln_data, exNouns)
Lincoln_nouns[1]

### 추출된 단어를 대상으로 전처리하기
# 추출된 단어를 이용하여 말뭉치(Corpus) 생성
# 코퍼스(말뭉치,Corpus) 함수를 사용하기 위한 tm,corpus 패키지 로드
myCorpus <- Corpus(VectorSource(Lincoln_nouns))
myCorpus
# 아래 데이터가 의미하는 것은?
# <<SimpleCorpus>>
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 20

# 데이터 전처리
# 단계 2-1: 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)

# 단계 2-2: 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)

# 단계 2-3: 불용어 제거
# stopwords= c(삭제 단어들:불용어) 하면 tdm()함수에 데이터 정제(삭제)를 해줌
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english')) 

# 단계 2-4: 전처리 결과 확인
inspect(myCorpusPrepro[1:5])

# 단어 선별(2 ~ 8 음절 사이 단어 선택)하기
# 단계 1: 전처리된 단어집에서 2 ~ 8 음절 단어 대상 선정
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro, 
                     control = list(wordLengths = c(4, 16)))
myCorpusPrepro_term
# 아래 결과값의 의미는?
# <<TermDocumentMatrix (terms: 71, documents: 12)>>
# Non-/sparse entries: 99/753
# Sparsity           : 88%
# Maximal term length: 5
# Weighting          : term frequency (tf)

# 단계 2: matrix 자료구조를 data.frame 자료구조로 변경
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df )


# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]


# 불용어 제거하기
# 불용어 : 인터넷 검색 시 검색 용어로 사용하지 않는 단어. 관사, 전치사, 조사,
# 접속사 등 검색 색인 단어로 의미가 없는 단어이다. 다만 각 검색 엔진마다
# 동일하지 않기 때문에 다를 수도 있다.

# 단계 1: 데이터 전처리
# 단계 1-1: 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
# 단계 1-2: 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
# 단계 1-3: 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
# 단계 1-4: 제거할 단어 지정
myStopwords = c(stopwords('english'), "사용", "하기")
# 단계 1-5: 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, myStopwords)

#단계 2: 단어 선별과 평서문 변환
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro,
                     control = list(wordLengths = c(4, 16)))
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))


# 단계 3: 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]
# 출현 빈도수
# 우리 나라 헌신 자리 국민 신념 잉태 자유 정신 하게 
# 13    5    3    3    3    2    2    2    2    2

# 단어 구름에 디자인(빈도수, 색상, 위치, 회전 등) 적용하기
# 단계 1: 단어 이름과 빈도수로 data.frame 생성
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
str(word.df)

# 단계 2: 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")

# 단계 3: 단어 구름 시각화
# 단어구름 시각화
wordcloud(word.df$word, word.df$freq, scale = c(7, 1), 
          min.freq = 1, random.order = F, 
          rot.per = .1, colors = pal, family = "malgun")
# rot.per = : 단어 회전도
# scale = : 글자 크기
# min.freq = : 최소 빈도
# random.order = F : 최빈 단어가 중간에 나오도록 설정(F)

### 2)
# 제공된 데이터를 이용하여 연관어 분석을 실시하여 연관어를
# 시각화 하고 시각화 결과에 대해 설명하시오.

library(KoNLP)
# 한국어 형태소 분석을 위한 R 패키지이다.
# 크롤링 혹은 워드클라우드를 작성할때 많이 이용한다.
library(tm)

# 연관어 분석
# 단계 1: 텍스트 파일 가져오기
getwd() # 디렉토리 경로 확인
setwd("D:/OneDrive/R/dataset") # 경로 재설정
getwd() # 재설정 경로 확인

marketing <- file("Lincoln_Gettysburg_Address2.txt", encoding = "UTF-8")
marketing2 <- readLines(marketing) # readLines : 함수는 여러 라인을 리스트(List)에 저장
close(marketing)
head(marketing2)

# 단계 2: 줄 단위 단어 추출
lword <- Map(extractNoun, marketing2)
length(lword)
lword <- unique(lword)
length(lword)

# 단계 3: 중복 단어 제거와 추출 단어 확인
lword <- sapply(lword, unique)
length(lword)
lword

# 연관어 분석을 위한 전처리하기
# 단계 1: 단어 필터링 함수 정의
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) { Filter(filter1, x) }

# 단계 2: 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword

# 트랜잭션 생성하기
# 단계 1: 연관분석을 위한 패키지 설치와 로딩
install.packages("arules") 
library(arules)

# 단계 2: 트랜잭션 생성
wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error발생
wordtran
# 트랜잭션 내용 보기 : 각 트랜잭션의 단어 보기
inspect(wordtran)

install.packages("backports")
library(backports)
# 단어 간 연관규칙 발견하기
# 단계 1: 연관규칙 발견
tranrules<- apriori(wordtran,parameter = list(supp = 0.25, conf =0.05))
# supp = : 지지도 , conf = : 신뢰도
# witing... 5개의 규칙 발견

# 연관규칙 생성 결과보기
detach(package:tm, unload=TRUE)
inspect(tranrules)
# 에러 : Error in inspect(tranrules) : object 'tranrules' not found
# 에러 시 우측 패키지 창에서 재실행


# 연관어 시각화하기
# 단계 1: 연관단어 시각화를 위해서 자료구조 변경
rules <- labels(tranrules, ruleSep = " ")
rules

# 단계 2: 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules

# 단계 3: 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)

# 단계 4: 연관어 시각화를 위한 igraph 패키지 설치와 로딩
install.packages("igraph")
library(igraph)

# 단계 5: edgelist 보기
ruleg <- graph.edgelist(rulemat[c(2:8), ], directed = F) # rulemat : 범위를 잘못 지정할 경우 에러 발생함
ruleg

# 단계 6: edgelist 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')



### 3)
# 다음 포털사이트의 실시간 뉴스(https://news.daum.net/)를 수집하고 실시간
# 토픽분석을 실행하여 단어구름으로 시각화 하고, 분석 시점에서 주요 이슈가
# 무엇인지 설명하시오.


install.packages("httr")
library(httr) #  http에 요청하고 응답에 필요한 httr 패키지 실행 
install.packages("XML")
library(XML) # 데이터를 계층 구조로 표현하기 위해 XML 패키지 실행

# 실습: 웹 문서 요청
url <- "https://news.daum.net/" # 다음
web <- GET(url) 
# HTTP 요청에 관한 함수들: GET(),POST()
# 1) GET 방식으로 HTTP 통신이 사용된 경우
# - GET 방식은 가장 일반적인 HTTP Request 형태로
# - 웹 브라우저에 다음과 같은 요청 데이터에 대한
# - 인수를 URL(Uniform Resource Locator)을 통해 전송한다.

# 2) POST 방식으로 HTTP 통신이 사용된 경우
# - POST 방식은 URL에 요청 데이터를 기록하지 않고 HTTP 헤더에
# - 데이터를 전송하기 때문에 GET 방식에서의 '?page=1&search=test'와
# - 같은 부분이 존재하지 않는다.
web


# (3) HTML 파싱
# 실습: HTML 파싱하기
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "UTF-8")
# htmlTreeParse() : url 소스 -> html 태그 파싱
# useInternalNodes = T : root node
# trim = T : 앞뒤 공백 제거
# encoding = "utf-8" : 문자셋 인코딩

rootNode <- xmlRoot(html)

# 태그(tag) 자료 수집
# 실습: 태그 자료 수집하기
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
news


# 수집한 자료 전처리
# 실습: 자료 전처리하기
# 단계 1: 자료 전처리 - 수집한 문서를 대상으로 불용어 제거
news_pre <- gsub("[\r\n\t]", ' ', news)
news_pre <- gsub('[[:punct:]]', ' ', news_pre)
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
# news_pre <- gsub('\\d+', ' ', news_pre) # corona19(covid19) 때문에 숫자 제거 생략
news_pre <- gsub('[a-z]+', ' ', news_pre)
news_pre <- gsub('[A-Z]+', ' ', news_pre)
news_pre <- gsub('\\s+', ' ', news_pre)
news_pre


# 단계 2: 기사와 관계 없는 'TODAY', '검색어 순위' 등의 내용은 제거
news_data <- news_pre[1:60]
news_data


# 파일 저장 및 읽기
# 실습: 수집한 자료를 파일로 저장하고 읽기
getwd() # 디렉토리 경로 확인
setwd("D:/OneDrive/R/dataset") # 경로 재설정
getwd() # 재설정 경로 확인

write.csv(news_data, "news_data.csv", quote = F)

news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)
str(news_data)

names(news_data) <- c("no", "news_text")
head(news_data)

news_text <- news_data$news_text
news_text


# 토픽분석
# 실습: 세종 사전에 단어 추가
library(KoNLP) # buildDictionary 실행을 위해 KoNLP 패키지 로드
user_dic <- data.frame(term = c("정치", "코로나19", "대통령령"), tag = 'ncn')
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

# 실습: 단어 추출 사용자 함수 정의하기
# 단계 1: 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(x), collapse = " ")}

# 단계 2: exNouns() 함수를 이용하어 단어 추출
news_nouns <- sapply(news_text, exNouns)
news_nouns

# 단계 3: 추출 결과 확인
str(news_nouns)
library(tm) ### tm 패키지 reload!!!!

# 실습: 말뭉치 생성과 집계 행렬 만들기
# 단계 1: 추출된 단어를 이용한 말뭉치(corpus) 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5]) 
# 단계 2: 단어 vs 문서 집계 행렬 만들기
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 16)))
TDM
# 단계 3: matrix 자료구조를 data.frame 자료구조로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)
# 실습: 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:10]

# 실습: 단어 구름 생성
# 단계 1: 패키지 로딩과 단어 이름 추출
library(wordcloud)
library(devtools)
myNames <- names(wordResult)
myNames

# 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df)

##### Wordcloud2
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
install_github("lchiffon/wordcloud2")
library(wordcloud2)

wc2data <- data.frame(df$word, df$freq)
wc2data

#1)
wordcloud2(data=df, size=0.5, color='random-dark')
#2)
wordcloud2(data=df, size=0.3, color='skyblue', backgroundColor = "black", shape='star')
#3) 워드클라우드 문자 회전 설정
wordcloud2(data=df, minRotation=-pi/6, maxRotation=-pi/6, rotateRatio=3)






