############################################################
# 텍스트 분석기법 9조
# 지도: 박현정 교수님
# 팀 프로젝트 1. 텍스트 전처리
# 이화여자대학교 대학원 고현경, 김남희, 송은정, 최지원
# project on GitHub - https://github.com/songej/ewha-dust
############################################################

##### 전처리 준비 #####

### 작업 환경 설정

# 작업 디렉토리 확인 및 지정
# getwd()
# setwd("c:\\text_anal")

# 패키지 설치 및 불러오기
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_131")
if(!require(rJava)) {install.packages("rJava"); library(rJava)} # 자바기반 패키지 지원
if(!require(KoNLP)) {install.packages("KoNLP"); library(KoNLP)} # 한국어 자연어 처리
if(!require(stringr)) {install.packages("stringr"); library(stringr)} # 문자열 처리
if(!require(rvest)) {install.packages("rvest"); library(rvest)} # repair_encoding 함수 포함
if(!require(tm)) {install.packages("tm"); library(tm)} # 자연어 처리
if(!require(wordcloud)) {install.packages("wordcloud"); library(wordcloud)} # 워드 클라우드
if(!require(qgraph)) {install.packages("qgraph"); library(qgraph)} # 네트워크 그래프 시각화

##### 형태소 분석 #####

### 토큰 정규화 (Token Normalization)

# 세종한글사전 사용
useSejongDic()
# mergeDust.txt 파일의 데이터를 세종한글사전에 사용자 단어로 추가 (KoNLP 패키지 업데이트로 mergeUserDic 대신 buildDictionary 함수 활용)
buildDictionary(ext_dic=c('sejong'), user_dic=data.frame(readLines("mergeDust.txt"), "ncn"), replace_usr_dic=T)

# csv 파일 데이터를 dust 변수에 저장
dust <- read.csv("naverNewsComment.csv", header=T, stringsAsFactors=FALSE)
# 변수에 저장된 내용 확인
head(dust, 5)

# 댓글 내용 데이터만 추출하여 dustComm 변수에 저장
dustComm <- subset(dust, select=c(contents))
# 변수에 저장된 내용 확인
head(dustComm, 5)

### 명사 추출 (Noun Processing)

# 댓글 내용에서 명사 추출하여 새로운 변수에 저장
dustCommN1 <- sapply(dustComm, extractNoun, USE.NAMES=F, autoSpacing=TRUE)

# 글자 수 필터링을 위해 unlist 작업하여 dustCommN2 변수에 저장
dustCommN2 <- unlist(dustCommN1)
# 2글자 이상 10 글자 이하 단어만 추출하여 저장
dustCommN2 <- Filter(function(x) {nchar(x)>=2 & nchar(x)<=10}, dustCommN2)
# 변수에 저장된 내용 확인
head(dustCommN2, 5)

### 토큰화 (Tokenization)

# 정규표현식으로 토큰화
dustCommN2 <- gsub("[:cntrl:]","",dustCommN2) #제어문자 제거
dustCommN2 <- gsub("[:punct:]","",dustCommN2) #특수문자 제거
dustCommN2 <- gsub("\\d+","",dustCommN2) #숫자 제거
dustCommN2 <- gsub("[:blank:]","",dustCommN2) #공백 제거
dustCommN2 <- gsub("[:space:]","",dustCommN2) #공백문자 제거
dustCommN2 <- str_replace_all(dustCommN2,"[^[:alpha:]]","") # 한글영어 이외 제거
dustCommN2 <-gsub("[^가-힣]","",dustCommN2) # 한글 이외 모든문자와 ㅋㅎㅇ등도 제거

head(dustCommN2, 50) # 전처리 중간 점검

##### 의미정보 변환 및 추출 #####

### 어간처리 및 원형복원 (Stemming & Lemmatization)

# gsub 함수로 의미정보 변환
dustCommN2 <-gsub("미세.*","미세먼지",dustCommN2)
dustCommN2 <-gsub(".*먼지","미세먼지",dustCommN2)
dustCommN2 <-gsub(".*세먼.*","미세먼지",dustCommN2)
dustCommN2 <-gsub(".*마스.*","마스크",dustCommN2)
dustCommN2 <-gsub(".*스크.*","마스크",dustCommN2)
dustCommN2 <-gsub(".*고등.*","고등어",dustCommN2)
dustCommN2 <-gsub(".*등어.*","고등어",dustCommN2)
dustCommN2 <-gsub(".*스모.*","스모그",dustCommN2)
dustCommN2 <-gsub(".*모그.*","스모그",dustCommN2)
dustCommN2 <-gsub("샤드","사드",dustCommN2)
dustCommN2 <-gsub(".*사드.*","사드",dustCommN2)
dustCommN2 <-gsub(".*서해.*","서해",dustCommN2)
dustCommN2 <-gsub("중궈","중국",dustCommN2)
dustCommN2 <-gsub(".*중국.*","중국",dustCommN2)
dustCommN2 <-gsub(".*짱게.*","짱개",dustCommN2)
dustCommN2 <-gsub(".*짱께.*","짱개",dustCommN2)
dustCommN2 <-gsub(".*짱꼴.*","짱개",dustCommN2)
dustCommN2 <-gsub(".*짱깨.*","짱개",dustCommN2)
dustCommN2 <-gsub(".*떼놈.*","떼놈",dustCommN2)
dustCommN2 <-gsub(".*산둥반도.*","산둥반도",dustCommN2)
dustCommN2 <-gsub(".*산둥반.*","산둥반도",dustCommN2)
dustCommN2 <-gsub(".*짱개.*","짱개",dustCommN2)
dustCommN2 <-gsub(".*민국.*","대한민국",dustCommN2)
dustCommN2 <-gsub(".*대한.*","대한민국",dustCommN2)
dustCommN2 <-gsub(".*대대한.*","대한민국",dustCommN2)
dustCommN2 <-gsub(".*리국가.*","우리국가",dustCommN2)
dustCommN2 <-gsub(".*한국.*","한국",dustCommN2)
dustCommN2 <-gsub(".*헬조.*","헬조선",dustCommN2)
dustCommN2 <-gsub(".*서울.*","서울",dustCommN2)
dustCommN2 <-gsub(".*베이징.*","베이징",dustCommN2)
dustCommN2 <-gsub(".*북한.*","북한",dustCommN2)
dustCommN2 <-gsub(".*대통.*","대통령",dustCommN2)
dustCommN2 <-gsub(".*통령.*","대통령",dustCommN2)
dustCommN2 <-gsub("나라","국가",dustCommN2)
dustCommN2 <-gsub("머니","뭐니",dustCommN2)
dustCommN2 <-gsub("강원도.*","강원도",dustCommN2)
dustCommN2 <-gsub("촋불시위","촛불시위",dustCommN2)
dustCommN2 <-gsub("한국","대한민국",dustCommN2)

head(dustCommN2, 50) # 전처리 중간 점검

### 불용어 제거 (Stop Words)

# gsubDust.txt 파일의 데이터를 사용자 불용어 목록으로 활용하여 불용어 일괄 제거
gsubDust <- readLines("gsubDust.txt", encoding="UTF-8")
head(gsubDust, 10)
dustGsubCnt <- length(gsubDust)
dustGsubCnt
for( i in 1:dustGsubCnt ) { dustCommN2 <- gsub((gsubDust[i]), "", dustCommN2) }

head(dustCommN2, 50) # 전처리 중간 점검

# 불용어 제거로 생긴 공백 제거
dustCommN2 <- str_trim(dustCommN2)
# 2글자 이상 10 글자 이하 단어만 추출
dustCommN2 <- Filter(function(x) {nchar(x)>=2 & nchar(x)<=10}, dustCommN2)
head(dustCommN2, 5)
# 변수에 저장된 내용을 txt 파일로 저장
write(unlist(dustCommN2),"dustCommN2.txt")
# 테이블 형태로 불러들여 공백 제거
dustCommN3 <- read.table("dustCommN2.txt")
# 변수에 저장된 내용 확인
head(dustCommN3, 5)

##### 정보 시각화 #####

### 워드 클라우드 (Word Cloud)

# 명사 단어수 조회
nrow(dustCommN3)
wordcount <- table(dustCommN3)
# 상위 50개 단어를 내림차순으로 정렬하여 확인
head(sort(wordcount, decreasing=T), 50)

palete <- brewer.pal(8,"Set2") # 단어 색상 팔레트 지정
windowsFonts(head=windowsFont("HY헤드라인M")) # 글꼴 지정
# 워드클라우드 출력
wordcloud(names(wordcount), freq=wordcount, scale=c(4, 1), rot.per=0.2, min.freq=30, random.order=F, random.color=T, colors=palete, family="head")

### 단어-문서 행렬 (Term-Document Matrix)

# tm 패키지에서 멀티프로세서 활용시 에러를 방지하기 위해 단일프로세서만 활용
options(mc.cores=1)
# 댓글 내용 데이터를 변수에 저장
tmDust <- dustComm[["contents"]]

# 한글 말뭉치 변환 (tm 패키지 업데이트로 Corpus 대신 VCorpus 함수 활용)
tmDustCps <- VCorpus(VectorSource(tmDust))

# tm_map 함수로 의미정보 살리기
tmDustCpsClean <- tmDustCps
tmDustCpsClean <- tm_map(tmDustCpsClean, stripWhitespace) #여러개의 공백을 하나의 공백으로 변환
tmDustCpsClean <- tm_map(tmDustCpsClean, removeNumbers) #숫자 제거
tmDustCpsClean <- tm_map(tmDustCpsClean, removePunctuation) #문장부호 제거

# 기본 불용어 및 사용자 불용어 목록 변수에 저장
tmStopword <- c(stopwords('english'), readLines("gsubDust.txt", encoding="UTF-8"))
# 불용어 목록 변수 활용하여 불용어 제거
tmDustCpsClean <- tm_map(tmDustCpsClean, removeWords, tmStopword)

# 명사 추출 사용자 함수 설정
tmDustNoun <- function(doc) {
  d <- as.character(doc)
  extractNoun(d) }

# 단어문서행렬 생성
tmTDM <- TermDocumentMatrix(tmDustCpsClean, control=list(tokenize=tmDustNoun, removePunctuation=T, removeNumbers=T, wordLengths=c(2, 10), weighting=weightBin))
# 단어문서행렬의 차원 확인
dim(tmTDM)
# 단어문서행렬을 실제 관찰가능한 행렬로 변환
tmTDMmatrix <- as.matrix(tmTDM)

# 상위 10개 단어에 대한 동시출현 행렬 생성
tmWordCount <- rowSums(tmTDMmatrix) # 행렬의 행의 수 합계로 문서 출현수 구함
tmWordOrder <- order(tmWordCount, decreasing=T) # 많은 문서에 출현한 단어순 정렬
tmWordFreq <- tmTDMmatrix[tmWordOrder[1:10],] # 상위 10개 단어 행렬 추출
coMatrix <- tmWordFreq %*% t(tmWordFreq) # 상위 10개 단어 동시출현 행렬
coMatrix # 동시출현 행렬 확인
# 동시출현 행렬 시각화
qgraph(coMatrix, labels=rownames(coMatrix), diag=F, layout='spring', label.cex=2, label.norm=3, edge.color='blue', esize=170, vsize=diag(coMatrix)*0.07)

# 중국 단어와 0.1 이상의 상관관계가 있는 단어 출력
findAssocs(tmTDM, "중국", 0.1)
