# 9조 1번째 실행코드

############################################################
# 텍스트 분석기법 9조
# 지도: 박현정 교수님
# 미세먼지로 인한 온라인 저널리즘과 대중국정서 분석
# 이화여자대학교 대학원 고현경, 김남희, 송은정, 최지원
# project on GitHub - https://github.com/songej/ewha-dust
############################################################

### 작업 설정

# 작업 디렉토리 확인 및 지정
# getwd()
# setwd("c:\\text_anal")

# 패키지 및 옵션 설정
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_131")
if(!require(rJava)) {install.packages("rJava"); library(rJava)} # 자바기반 패키지 지원
if(!require(devtools)) {install.packages("devtools")} # 외부 패키지 지원
if(!require(tm)) {install.packages("tm"); library(tm)} # 자연어 처리
if(!require(KoNLP)) {install.packages("KoNLP"); library(KoNLP)} # 한국어 자연어 처리
if(!require(stringr)) {install.packages("stringr"); library(stringr)} # 문자열 처리
if(!require(rvest)) {install.packages("rvest"); library(rvest)} # repair_encoding 함수 포함
if(!require(slam)) {install.packages("slam"); library(slam)} # TDM 계산
if(!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)} # 토픽모델 처리
if(!require(lda)) {install.packages("lda"); library(lda)} # LDA 토픽분석
if(!require(LDAvis)) {install.packages("LDAvis"); library(LDAvis)} # 토픽모델 시각화
if(!require(servr)) {install.packages("servr"); library(servr)} # 토픽모델 시각화 웹브라우저 출력
if(!require(qgraph)) {install.packages("qgraph"); library(qgraph)} # 네트워크 시각화
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)} # 그래프 시각화
if(!require(wordcloud)) {install.packages("wordcloud"); library(wordcloud)} # 워드 클라우드
if(!require(treemap)) {install.packages("treemap"); library(treemap)} # 트리맵
if(!require(N2H4)) {devtools::install_github("forkonlp/N2H4"); library(N2H4)} # 네이버 크롤링
options(encoding="UTF-8") # 한글 인코딩

##### Text Mining #1. 미세먼지 관련 네이버 뉴스 기사 분석

### 데이터 불러오기

# csv 파일 데이터 naver.news 변수 저장 및 내용 확인
naver.news <- read.csv("naverNews.csv", header=T, stringsAsFactors=FALSE, fileEncoding="CP949")
head(naver.news, 3)
# 댓글 내용 데이터 추출하여 news 변수 저장 및 내용 확인
news <- subset(naver.news, select=c(content))
head(news, 3)

### Token Normalization

# 정규표현식으로 완성형 한글 이외 제거
news.clean <- sapply(news, function(news) gsub("[^가-힣]", " ", news))
# news.clean 변수에 저장된 내용 확인
head(news.clean, 3)

### Noun Processing

# 세종한글사전 사용
useSejongDic()
# mergeNews.txt 파일의 데이터를 세종한글사전에 사용자 단어로 추가
kMergeDust <- data.frame(readLines("mergeNews.txt"), "ncn")
colnames(kMergeDust) <- c("term", "tag")
# KoNLP 패키지 업데이트로 mergeUserDic 대신 buildDictionary 함수 활용
buildDictionary(ext_dic=c("sejong"), user_dic=kMergeDust, replace_usr_dic=T)

### Term-Document Matrix

# tm 패키지에서 멀티프로세서 활용시 에러를 방지하기 위해 단일프로세서만 활용
options(mc.cores=1)
# 한글 말뭉치 변환 (tm 패키지 업데이트로 Corpus 대신 VCorpus 함수 활용)
news.cps <- VCorpus(VectorSource(news.clean))

# Token Normalization by tm_map fuction
news.cps.clean <- news.cps
news.cps.clean <- tm_map(news.cps.clean, stripWhitespace) # 여러개의 공백을 하나의 공백으로 변환
news.cps.clean <- tm_map(news.cps.clean, removeNumbers) # 숫자 제거
news.cps.clean <- tm_map(news.cps.clean, removePunctuation) # 문장부호 제거

# 기본 불용어 및 사용자 불용어 목록 변수에 저장
news.cps.stopwords <- c(stopwords('english'), readLines("gsubNews.txt"))
# 불용어 목록 변수 활용하여 불용어 제거
news.cps.clean <- tm_map(news.cps.clean, removeWords, news.cps.stopwords)

# Noun Processing

# 명사 추출 사용자 함수 설정
NewsNoun <- function(doc) {
  d <- as.character(doc)
  extractNoun(d) }

# 단어-문서 행렬 생성
news.TDM <- TermDocumentMatrix(news.cps.clean, control=list(tokenize=NewsNoun, removePunctuation=T, removeNumbers=T, wordLengths=c(2, 5), weighting=weightTf))
# 단어-문서 행렬 차원확인
dim(news.TDM)
# 단어-문서 행렬 관찰가능 형태변환
news.TDM.matrix <- as.matrix(news.TDM)

# TDM 행합계로 단어 빈도수 계산
news.wordcount <- as.array(rollup(news.TDM, 2))
# 단어 빈도수 내림차 정렬
news.wordorder <- order(news.wordcount, decreasing = T)
# 상위 빈도 30개 단어 정렬 및 추출
news.freqword <- news.wordorder[1:30]
row.names(news.TDM[news.freqword,])

### 토픽 분석 준비

# 토픽모델 파라미터 지정
kK <- 5 # 토픽 수
kI <- 5000 # 정교화 정도
kB <- 1000 # 정교화 과정에서 앞부분 데이터를 버리는 정도
kAlpha <- 0.01 # 문서 분포 정도
kEta <- 0.01 # 토픽 내 단어 분포 정도
kW <- 20 # 토픽별 추출단어 수

# LDA 적용을 위한 TDM 데이터 변환
news.DTM <- as.DocumentTermMatrix(news.TDM[news.freqword,])
LDA.form <- dtm2ldaformat(news.DTM, omit_empty = T)

### 토픽 분석
# 토픽분석 실행시간 확인
LDA.t1 <- Sys.time() # 토픽분석 시작시간
# 토픽분석 실시
LDA.result <- lda.collapsed.gibbs.sampler(documents = LDA.form$documents,
  K = kK,
  vocab = LDA.form$vocab,
  num.iterations = kI,
  burnin = kB,
  alpha = kAlpha,
  eta = kEta)
LDA.t2 <- Sys.time() # 토픽분석 종료시간
LDA.t2 - LDA.t1 # 토픽분석 소요시간

### 토픽 분석 시각화

# 단어 출처 토픽 표
LDA.result$topics
# 토픽별 총 단어 수
LDA.result$topic_sums
# 토픽별 상위 단어 표
LDA.top.words <- top.topic.words(LDA.result$topics, kW, by.score=TRUE)
print(LDA.top.words)
write.csv(LDA.top.words, file="LDAtopwords.csv", fileEncoding="CP949")

# 단어 빈도수 목차 문서 정보 등 저장
LDA.table <- table(unlist(LDA.form$vocab))
LDA.table <- sort(LDA.table, decreasing=T)
LDA.freq <- as.integer(LDA.table)
LDA.vocab <- names(LDA.table)
LDA.doc.length <- sapply(LDA.form$documents, function(x) sum(x[2, ]))

# 토픽분석 시각화 파라미터 지정
LDA.theta <- t(apply(LDA.result$document_sums + kAlpha, 2, function(x) x/sum(x)))
LDA.phi <- t(apply(t(LDA.result$topics) + kEta, 2, function(x) x/sum(x)))
LDA.topic <- list(phi=LDA.phi, theta=LDA.theta,
  doc.length=LDA.doc.length, vocab=LDA.vocab, term.frequency=LDA.freq)

# 토픽분석 시각화 JSON 오브젝트 생성
json <- createJSON(LDA.phi, LDA.theta, LDA.doc.length, LDA.form$vocab, LDA.freq)
serVis(json, out.dir="LDA", open.browser=TRUE)
