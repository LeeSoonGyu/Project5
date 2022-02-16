# 사례연구 5 텍스트 데이터 분석 A3 - 문태웅,이순규,남원식

# 패키지 설치
install.packages("KoNLP") # 형태소 분석 패키지
install.packages("tm") # 텍스트 분석 패키지
install.packages("wordcloud") # 워드클라우드 시각화 패키지
install.packages("Sejong") # 한글 사전 패키지
install.packages("arules") # 연관성 분석 패키지
install.packages("wordcloud2") # 워드클라우드2 시각화 패키지
install.packages("igraph") # 변수들 사이의 네트워크 연결망을 그려주는 패키지
install.packages("httr") # http에 요청하고 응답에 필요한 패키지
install.packages("XML") # XML 문서처리 패키지

# 패키지 실행
library(KoNLP)
library(tm)
library(wordcloud)
library(Sejong)
library(arules)
library(wordcloud2)
library(igraph)
library(httr)
library(XML) 

# 텍스트 자료 가져오기
rm(list = ls()) 
setwd("c:/Rwork/data") # 경로 설정
lincoln <- file("lincoln.txt", encoding = "UTF-8") # 텍스트 파일 UTF-8인코딩 및 변수에 담기
lincoln_l <- readLines(lincoln) # 불러온 데이터를 라인화 하여 변수에 저장
lincoln_l

# 단어 추출 사용자 정의함수로 만들기
exNouns <- function(x){paste(extractNoun(as.character(x)),
                             collapse = " ")}

#### (1)제공된 데이터를 이용하여 토픽 분석을 실시하여 단어구름으로 시각화 하고 단어 출현 빈도수를 기반하여 어떤 단어 들이 주요 단어인지 설명하시오.####
#install.packages("KoNLP") # 형태소 분석 패키지
#install.packages("tm") # 텍스트 분석 패키지
#install.packages("wordcloud") # 워드클라우드 시각화 패키지
#install.packages("Sejong") # 한글 사전 패키지
#install.packages("wordcloud2") # 워드클라우드2 시각화 패키지
# exNouns 함수를 이용하여 단어 추출
lincoln_word <- sapply(lincoln_l, exNouns) 
lincoln_word

# 전처리
# 말뭉치 생성하기
myCorpus <- Corpus(VectorSource(lincoln_word)) 

# 문장부호 제거
lincoln_pre <-  tm_map(myCorpus,removePunctuation) # tm_map() 공백 제거 및 불필요한 불용어 제거에 사용하는 함수

# 수치제거
lincoln_pre <-  tm_map(lincoln_pre,removeNumbers)

# 전처리된 단어들 2~8음절인 단어만 추려내기
mylincoln_pre <- TermDocumentMatrix(lincoln_pre,control = list(wordLengths = c(4,16)))
# TermDocumentMatrix() : 여러 문서들의 집단에서 단어를 추출한 뒤,
# 행에 출현했던 단어 리스트를 나열하고, 열에 각 문서들을 나열할때 사용된다.

# matrix형을 dataframe형으로 변환
mylincoln_pre_df <- as.data.frame(as.matrix(mylincoln_pre))
dim(mylincoln_pre_df)

# 단어구름으로 시각화 하기위해 빈도수 구하기
# 워드클라우드1
lincoln_word <- sort(rowSums(mylincoln_pre_df),decreasing = TRUE) # 내림차순으로 순서정렬
wordcount <- table(lincoln_word) # 빈도수 wordcount 에저장
myName <- names(lincoln_word) # 변수명 myName에 저장
word.df <- data.frame(word = myName, freq = lincoln_word) # 빈도수와 변수명을 데이터프레임 word.df로 생성
pal <- brewer.pal(10, "Paired") # 단어의 색깔 지정
wordcloud(word.df$word, word.df$freq, scale = c(3,0.5), 
          min.freq = 2, random.order = F, 
          rot.per = .1, colors = pal)
# min.freq : 최소 빈도
# random.order = F : 최빈단어가 중간에 나오도록 설정(F)
# rot.per : 단어 회전도
# scale : 글자크기
# colors : 색상지정


# 워드 클라우드2
#install.packages("wordcloud2")
#library(wordcloud2)
wc <- wordcloud2(data = word.df,size = 1.5,color = "random-light",backgroundColor = "black",rotateRatio = 0.75)
# size : 글자 크기
# color : 글자색
# backgroundColor : 배경색
# rotateRatio : 글자 회전도

#### (2)제공된 데이터를 이용하여 연관어 분석을 실시하여 연관어를 시각화 하고 시각화 결과에 대해 설명하시오 ####
# 패키지 설치
#install.packages("backports") # 오류발생시 설치하는 패키지
#library(backports)
#install.packages("arules") # 연관성 분석 패키지
#install.packages("tm") # 텍스트 분석 패키지
#install.packages("wordcloud2") # 워드클라우드2 시각화 패키지

# 줄별로 단어 추출
lword <- Map(extractNoun, Lincoln_data) # Map() : 리스트 형태로 반환해주는 함수
length(lword)
# 중복단어 제거
lword <- unique(lword)

# 단어 필터링 함수 정의(2 ~ 4 글자의 한글만 추출하는 필터)
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) { Filter(filter1, x) }

# 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword

# 트랜잭션 생성하기 
wordtran <- as(lword, "transactions")
wordtran

# 연관규칙 발견
tranrules <- apriori(wordtran, 
                     parameter = list(supp = 0.15, conf = 0.99)) 
tranrules
# supp = : 지지도 , conf = : 신뢰도

# 연관규칙 생성 결과보기
detach(package:tm, unload = TRUE)
inspect(tranrules) # inspect() 연관도 분석 함수

# 연관단어 시각화를 위한 자료구조 변경
l_rules <- labels(l_wordtran_rule, ruleSep = " ")
# levels에 대한 문자형 벡터를 지정, 벡터에 있는 각각 원소의 값을 다른 문자형 유형으로 변경

# 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- labels(tranrules, ruleSep = " ")
rules
# 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)
# 연관어 시각화를 위한 igraph 패키지 설치와 실행
#install.packages("igraph")
#library(igraph)

# edgelist 보기
ruleg <- graph.edgelist(rulemat[c(1:22), ], directed = F)
ruleg
# edgelist 시각화
par(family="NanumGothicBold")
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2,
            vertex.label.color = 'black',
            vertex.size = 20, 
            vertex.color = 'pink',
            vertex.frame.color = 'red')


#### 뉴스(https://news.daum.net/) 전처리 ####
#install.packages("httr") # http에 요청하고 응답에 필요한 패키지
#library(httr)
#install.packages("XML") # XML 문서처리 패키지
#library(XML)
#install.packages(tm) # TermdocumentMatrix()사용
#library(tm)

# 웹 문서 요청
url <- "http://news.daum.net/"
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

# HTML 파싱
html <- htmlTreeParse(web,useInternalNodes = T,trim =T,encoding = "UTF-8")
rootNode <- xmlRoot(html)

# 태그자료 수집
news <- xpathApply(rootNode,"//a[@class = 'link_txt']",xmlValue)

# 전처리
# gsub() : 자료를 찾아 바꾸는 함수
news_pre <- gsub("[\r\n\t]", ' ', news) #줄바꿈
news_pre <- gsub('[[:punct:]]', ' ', news_pre) #특수문자
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
news_pre <- gsub('\\d+', ' ', news_pre) #숫자 
news_pre <- gsub('[a-z]+', ' ', news_pre) #소문자
news_pre <- gsub('[A-Z]+', ' ', news_pre) #대문자
news_pre <- gsub('\\s+', ' ', news_pre) #빈칸
news_pre # 1~ 46번까지만 기사내용임

# 1~46까지의 기사 내용만 변수에 저장
news_data <- c(news_pre[1:46])

# 세종 사전에 단어 추가
user_dic <- data.frame(term = c("이재명", "무단횡단"), tag = 'ncn')
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

# 수집한 데이터 저장하고 읽기
setwd('C:/Rwork/Data')
write.csv(news_data, "news_data.csv", quote = F)
news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)
str(news_data)
names(news_data) <- c("no", "news_text")
head(news_data)
news_text <- news_data$news_text
news_text

##### (3) 다음 포털사이트의 실시간 뉴스(https://news.daum.net/)를 수집하고 실시간 토픽분석을 실행하여 단어구름으로 시각화 하고,
#####     분석 시점에서 주요 이슈가 무엇인지 설명하시오.####
#install.packages("tm") # 텍스트 분석 패키지
#install.packages("wordcloud2") # 워드클라우드2 시각화 패키지

# 전처리한 파일 토픽분석하기
exNouns <- function(x) { paste(extractNoun(x), collapse = " ")}
news_word <- sapply(news_text, exNouns)
news_word

# 말뭉치 생성후 집계행렬 만들기
newsCorpus <- Corpus(VectorSource(news_word))

# 집계행렬 생성
news_tdm <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 16)))

# 데이터프레임 변경
tdm.df <- as.data.frame(as.matrix(news_tdm))

# 단어구름 시각화 위한 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)

# 변수와 빈도수 데이터 프레임 만들기
news_name <- names(wordResult)
news.df <- data.frame(word = news_name, freq=wordResult)

# 클라우드 생성
news_cloud2 <- wordcloud2(data = news.df,size = 0.75,color = "random-light",backgroundColor = "black",rotateRatio = 0.75)
news_cloud2
# size : 글자 크기
# color : 글자색
# backgroundColor : 배경색
# rotateRatio : 글자 회전도
