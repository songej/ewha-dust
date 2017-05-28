# 9조 3번째 실행코드

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

##### Text Mining #3. 미세먼지 관련 네이버 뉴스 댓글 분석

### 데이터 불러오기

# csv 파일 데이터 naver.news.comment 변수 저장 및 내용 확인
naver.news.comment <- read.csv("naverNewsComment.csv", header=T, stringsAsFactors=FALSE, fileEncoding="CP949")
head(naver.news.comment, 5)
# 댓글 내용 데이터 추출하여 dust 변수 저장 및 내용 확인
dust <- subset(naver.news.comment, select=c(contents))
head(dust, 5)

### Tokenization

dust.split <- sapply(dust, str_split, "\\s+")

### 예비 분석

# 리스트 형식 제거
dust.split.words <- unlist(dust.split)
# 테이블 형식 변환
dust.split.words.freq <- table(dust.split.words)
# 상위 빈도 단어 50개 출력
head(sort(dust.split.words.freq, decreasing=T), 50)

### Token Normalization

# 정규표현식으로 완성형 한글 이외 제거
dust.clean <- sapply(dust, function(dust) gsub("[^가-힣]", " ", dust))
# dust.clean 변수에 저장된 내용 확인
head(dust.clean, 5)

### Noun Processing

# 세종한글사전 사용
useSejongDic()
# mergeDust.txt 파일의 데이터를 세종한글사전에 사용자 단어로 추가
kMergeDust <- data.frame(readLines("mergeDust.txt"), "ncn")
colnames(kMergeDust) <- c("term", "tag")
# KoNLP 패키지 업데이트로 mergeUserDic 대신 buildDictionary 함수 활용
buildDictionary(ext_dic=c("sejong"), user_dic=kMergeDust, replace_usr_dic=T)

### Term-Document Matrix

# tm 패키지에서 멀티프로세서 활용시 에러를 방지하기 위해 단일프로세서만 활용
options(mc.cores=1)
# 한글 말뭉치 변환 (tm 패키지 업데이트로 Corpus 대신 VCorpus 함수 활용)
dust.cps <- VCorpus(VectorSource(dust.clean))

# Token Normalization by tm_map fuction
dust.cps.clean <- dust.cps
dust.cps.clean <- tm_map(dust.cps.clean, stripWhitespace) #여러개의 공백을 하나의 공백으로 변환
dust.cps.clean <- tm_map(dust.cps.clean, removeNumbers) #숫자 제거
dust.cps.clean <- tm_map(dust.cps.clean, removePunctuation) #문장부호 제거

# 기본 불용어 및 사용자 불용어 목록 변수에 저장
dust.cps.stopwords <- c(stopwords('english'), readLines("gsubDust.txt"))
# 불용어 목록 변수 활용하여 불용어 제거
dust.cps.clean <- tm_map(dust.cps.clean, removeWords, dust.cps.stopwords)

# Noun Processing 1

# 명사 추출 사용자 함수 설정
DustNoun <- function(doc) {
  d <- as.character(doc)
  extractNoun(d) }

# 단어-문서 행렬 생성
TDM <- TermDocumentMatrix(dust.cps.clean, control=list(tokenize=DustNoun, removePunctuation=T, removeNumbers=T, wordLengths=c(2, 5), weighting=weightTf))
# 단어-문서 행렬 차원확인
dim(TDM)
# 단어-문서 행렬 관찰가능 형태변환
TDM.matrix <- as.matrix(TDM)

# 동시출현 행렬 생성
TDM.wordcount <- rowSums(TDM.matrix) # 행렬의 행의 수 합계로 문서 출현수 구함
TDM.wordorder <- order(TDM.wordcount, decreasing=T) # 많은 문서에 출현한 단어순 정렬
TDM.wordfreq <- TDM.matrix[TDM.wordorder[1:10],] # 상위 10개 단어 행렬 추출
TDM.comatrix <- TDM.wordfreq %*% t(TDM.wordfreq) # 상위 10개 단어 동시출현 행렬
TDM.comatrix # 동시출현 행렬 확인
# 동시출현 행렬 연관분석 시각화
qgraph(TDM.comatrix, labels=rownames(TDM.comatrix), diag=F, layout='spring', label.cex=2, label.norm=3, edge.color='blue', esize=100, vsize=diag(log(TDM.comatrix)*2))

### Word Clustering

# 일정 수치 이하 빈도의 키워드 제거 후 행렬 변환
cluster.matrix <- as.matrix(removeSparseTerms(TDM, sparse=0.96))
# 데이터 표준화 후 행렬 요소간 거리 산출
cluster.dist <- dist(scale(cluster.matrix))
# 클러스터 분석
cluster.fit <- hclust(cluster.dist, method="ward.D")
# 클러스터 분석 시각화
plot(cluster.fit, cex=1.2)
# 6개의 클러스터링 표시
rect.hclust(cluster.fit, k=6)

### Find Association

# 중국 단어와 15% 이상 연관어 출력
findAssocs(TDM, "중국", 0.15)
a.DF <- data.frame(findAssocs(TDM, "중국", 0.15))
a.term <- rownames(a.DF)
a.assocs <- a.DF[,1]
assocs.DF <- data.frame(term=a.term, assocs=a.assocs)
# 중국 단어와 15% 이상 연관어 시각화
ggplot(assocs.DF, aes(x=assocs, y=reorder(term, assocs))) +
xlab("Assocs") + ylab("Terms") +
geom_point(size=3) +
geom_segment(aes(yend=term), xend=0, colour="grey60") +
theme_bw() + theme(panel.grid.major.y=element_blank())

# Noun Processing 2

# 댓글 내용에서 명사 추출하여 dust.N1 변수에 저장
dust.N1 <- sapply(dust.clean, extractNoun, USE.NAMES=F, autoSpacing=T)
# 글자 수 필터링을 위해 unlist 작업하여 dust.N2 변수에 저장
dust.N2 <- unlist(dust.N1)
# 2글자 이상 5 글자 이하 단어만 추출하여 저장
dust.N2 <- Filter(function(x) {nchar(x)>=2 & nchar(x)<=5}, dust.N2)

### Stemming & Lemmatization

# gsub 함수로 의미정보 변환
dust.N2 <-gsub("미세.*","미세먼지",dust.N2)
dust.N2 <-gsub(".*먼지","미세먼지",dust.N2)
dust.N2 <-gsub(".*세먼.*","미세먼지",dust.N2)
dust.N2 <-gsub(".*마스.*","마스크",dust.N2)
dust.N2 <-gsub(".*스크.*","마스크",dust.N2)
dust.N2 <-gsub(".*고등.*","고등어",dust.N2)
dust.N2 <-gsub(".*등어.*","고등어",dust.N2)
dust.N2 <-gsub(".*스모.*","스모그",dust.N2)
dust.N2 <-gsub(".*모그.*","스모그",dust.N2)
dust.N2 <-gsub("샤드","사드",dust.N2)
dust.N2 <-gsub(".*사드.*","사드",dust.N2)
dust.N2 <-gsub(".*서해.*","서해",dust.N2)
dust.N2 <-gsub("중궈","중국",dust.N2)
dust.N2 <-gsub(".*중국.*","중국",dust.N2)
dust.N2 <-gsub(".*짱게.*","짱개",dust.N2)
dust.N2 <-gsub(".*짱께.*","짱개",dust.N2)
dust.N2 <-gsub(".*짱꼴.*","짱개",dust.N2)
dust.N2 <-gsub(".*짱깨.*","짱개",dust.N2)
dust.N2 <-gsub(".*떼놈.*","떼놈",dust.N2)
dust.N2 <-gsub(".*산둥반도.*","산둥반도",dust.N2)
dust.N2 <-gsub(".*산둥반.*","산둥반도",dust.N2)
dust.N2 <-gsub(".*짱개.*","짱개",dust.N2)
dust.N2 <-gsub(".*민국.*","대한민국",dust.N2)
dust.N2 <-gsub(".*대한.*","대한민국",dust.N2)
dust.N2 <-gsub(".*대대한.*","대한민국",dust.N2)
dust.N2 <-gsub(".*리국가.*","우리국가",dust.N2)
dust.N2 <-gsub(".*한국.*","한국",dust.N2)
dust.N2 <-gsub(".*헬조.*","헬조선",dust.N2)
dust.N2 <-gsub(".*서울.*","서울",dust.N2)
dust.N2 <-gsub(".*베이징.*","베이징",dust.N2)
dust.N2 <-gsub(".*북한.*","북한",dust.N2)
dust.N2 <-gsub(".*대통.*","대통령",dust.N2)
dust.N2 <-gsub(".*통령.*","대통령",dust.N2)
dust.N2 <-gsub("머니","뭐니",dust.N2)
dust.N2 <-gsub("강원도.*","강원도",dust.N2)
dust.N2 <-gsub("촋불시위","촛불시위",dust.N2)
dust.N2 <-gsub("한국","대한민국",dust.N2)

head(dust.N2, 50) # 전처리 중간 점검

### Stop Words

# gsubDust.txt 파일의 데이터를 사용자 불용어 목록으로 활용하여 불용어 일괄 제거
gsub.dust <- readLines("gsubDust.txt")
gsub.dust.cnt <- length(gsub.dust)
gsub.dust.cnt
for( i in 1:gsub.dust.cnt ) { dust.N2 <- gsub((gsub.dust[i]), "", dust.N2) }

# 불용어 제거로 생긴 공백 제거
dust.N2 <- str_trim(dust.N2)
# 변수에 저장된 내용을 txt 파일로 저장
write(unlist(dust.N2),"dustN2.txt")
# 테이블 형태로 불러들여 공백 제거
dust.N3 <- read.table("dustN2.txt")
# dust.N3 변수에 저장된 내용 확인
head(dust.N3, 5)

### Word Cloud

# 명사 단어수 조회
nrow(dust.N3)
dust.wordcount <- table(dust.N3)
# 상위 50개 단어 내림차순 정렬 및 확인
head(sort(dust.wordcount, decreasing=T), 50)
dust.palete <- brewer.pal(8,"Set2") # 단어 색상 팔레트 지정
windowsFonts(fontface=windowsFont("HY헤드라인M")) # 글꼴 지정
# 워드클라우드 출력
x11()
wordcloud(names(dust.wordcount), freq=dust.wordcount, scale=c(4, 1), rot.per=0.1, min.freq=40, random.order=F, random.color=T, colors=dust.palete, family="fontface")

### Tree Map

# dust.wordcount 변수를 빈도수 내림차 정렬하여 데이터 프레임 형태로 저장
dust.wordcouont.DF <- data.frame(sort(dust.wordcount, decreasing=T))
# dust.wordcouont.DF 데이터 프레임 라벨명 지정
names(dust.wordcouont.DF) <-c('Word','Freq')
# 빈도수 80 이상 단어 추출하여 dust.wordcouont.freq80 변수에 저장
dust.wordcouont.freq80 <- subset(dust.wordcouont.DF, Freq>80)
# 트리맵 작성
treemap(dust.wordcouont.freq80, title="Word Frequency TreeMap", index=c("Freq", "Word"),
type="value", vSize="Freq", vColor="Freq",
force.print.labels=T,
fontsize.labels=c(12, 20), align.labels=list(c("right", "center"), c("left", "top")), bg.labels="gray", lowerbound.cex.labels=0.1)

### Top 20 Words

# 상위 20개 단어 추출하여 dust.wordcouont.top20 변수에 저장
dust.wordcouont.top20 <- data.frame(head(sort(dust.wordcount, decreasing=T), 20))
colnames(dust.wordcouont.top20) <- c("term", "freq")
# Top 20 Words 시각화
ggplot(dust.wordcouont.top20, aes(x=freq, y=reorder(term, freq), size=freq)) +
xlab("Top Freq") + ylab("Terms") +
geom_point(shape=21, colour="black", fill="red") +
scale_size_area(max_size=10) +
scale_x_log10() +
theme_bw()
