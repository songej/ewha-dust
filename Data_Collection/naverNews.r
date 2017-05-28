if(!require(httr)) {install.packages("httr"); library(httr)}
if(!require(rvest)) {install.packages("rvest"); library(rvest)}
if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(selectr)) {install.packages("selectr"); library(selectr)}
if(!require('devtools')) install.packages('devtools')
if(!require(N2H4)) {devtools::install_github("forkonlp/N2H4"); library(N2H4)}

############################################################

# 뉴스 주소 추출
kBase.url <- "http://news.naver.com/main/hotissue/sectionList.nhn?mid=hot&sid1=102&cid=1043582&page="
all.url.html <- c()
for(page in 1:20){
  print(page)
  url <- paste(kBase.url, page, sep = "")
  r <- GET(url)
  h <- read_html(r)
  url.html <- html_nodes(h, '.t')
  if(length(url.html) == 0){ break }
  all.url.html <- c(all.url.html, url.html)
}
capture.output(all.url.html, file="all_url_html.txt")

############################################################

# 네이버 뉴스 크롤링
urls <- readLines("dust_news_urls.txt")
urls.cnt <- length(urls)
news.data <- c()
for(i in 1:urls.cnt){
  tryi <- 0
  tem <- try(getContent(urls[i]), silent = TRUE)
  while(tryi <= 5 && class(tem)=="try-error"){
    tem <- try(getContent(urls[i]), silent = TRUE)
    tryi <- tryi + 1
    print(paste0("try again: ",urls[i]))
  }
  if(class(tem$datetime)[1]=="POSIXct"){
    news.data<-rbind(news.data,tem)
  }
}
write.csv(news.data, file=paste0("dust_news_data_",i,".csv"), row.names = F)
