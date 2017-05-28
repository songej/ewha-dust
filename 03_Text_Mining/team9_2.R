# 9조 2번째 실행코드

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

##### Text Mining #2. 미세먼지 관련 네이버 뉴스 트렌드 분석

# 네이버 뉴스 수집 기간 지정
kTrend.d1 <- "2017-01-01"
kTrend.d2 <- "2017-05-20"
# 네이버 뉴스 수집 키워드 지정
kTrend.keyword <- c("미세먼지", "중국", "사드")
trends <- c()
# 네이버 뉴스 수집 (시간 소요)
for(q in kTrend.keyword) {
  print(q)
  trends.tem <- getNewsTrend(q, kTrend.d1, kTrend.d2)
  trends <- rbind(trends, cbind(data.frame(q=q, trends.tem)))
}
# 네이버 뉴스 트렌드 시각화
trend.graph <- ggplot(data=trends, mapping=aes(x=date, y=cnt, group=q, color=q)) + geom_line(size=1.2)
trend.graph + labs(title="네이버 뉴스 트렌드", caption="2017.01.01~05.20", x="날짜", y="기사량")
