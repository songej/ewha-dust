############################################################
# 텍스트 분석기법 9조
# 지도: 박현정 교수님
# 팀 프로젝트 1. 텍스트 전처리
# 이화여자대학교 대학원 고현경, 김남희, 송은정, 최지원
# project on GitHub - https://github.com/songej/ewha-dust
############################################################

install.packages("selectr")

if (!require('devtools')) install.packages('devtools')
devtools::install_github('forkonlp/N2H4')

library(N2H4)

url = "http://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=001&oid=001&aid=0009123088"

commP1 <- getComment(turl=url, pageSize=100, page=1, sort=c("favorite","reply","old","new"))
cList1 <- commP1$result$commentList
cList1 <- as.data.frame(cList1)

commP2 <- getComment(turl=url, pageSize=100, page=2, sort=c("favorite","reply","old","new"))
cList2 <- commP2$result$commentList
cList2 <- as.data.frame(cList2)

commP3 <- getComment(turl=url, pageSize=100, page=3, sort=c("favorite","reply","old","new"))
cList3 <- commP3$result$commentList
cList3 <- as.data.frame(cList3)

commP4 <- getComment(turl=url, pageSize=100, page=4, sort=c("favorite","reply","old","new"))
cList4 <- commP4$result$commentList
cList4 <- as.data.frame(cList4)

commP5 <- getComment(turl=url, pageSize=100, page=5, sort=c("favorite","reply","old","new"))
cList5 <- commP5$result$commentList
cList5 <- as.data.frame(cList5)

commP6 <- getComment(turl=url, pageSize=100, page=6, sort=c("favorite","reply","old","new"))
cList6 <- commP6$result$commentList
cList6 <- as.data.frame(cList6)

commP7 <- getComment(turl=url, pageSize=100, page=7, sort=c("favorite","reply","old","new"))
cList7 <- commP7$result$commentList
cList7 <- as.data.frame(cList7)

commP8 <- getComment(turl=url, pageSize=100, page=8, sort=c("favorite","reply","old","new"))
cList8 <- commP8$result$commentList
cList8 <- as.data.frame(cList8)

commP9 <- getComment(turl=url, pageSize=100, page=9, sort=c("favorite","reply","old","new"))
cList9 <- commP9$result$commentList
cList9 <- as.data.frame(cList9)

commP10 <- getComment(turl=url, pageSize=100, page=10, sort=c("favorite","reply","old","new"))
cList10 <- commP10$result$commentList
cList10 <- as.data.frame(cList10)

commP11 <- getComment(turl=url, pageSize=100, page=11, sort=c("favorite","reply","old","new"))
cList11 <- commP11$result$commentList
cList11 <- as.data.frame(cList11)

commP12 <- getComment(turl=url, pageSize=100, page=12, sort=c("favorite","reply","old","new"))
cList12 <- commP12$result$commentList
cList12 <- as.data.frame(cList12)

commP13 <- getComment(turl=url, pageSize=100, page=13, sort=c("favorite","reply","old","new"))
cList13 <- commP13$result$commentList
cList13 <- as.data.frame(cList13)

commP14 <- getComment(turl=url, pageSize=100, page=14, sort=c("favorite","reply","old","new"))
cList14 <- commP14$result$commentList
cList14 <- as.data.frame(cList14)

commP15 <- getComment(turl=url, pageSize=100, page=15, sort=c("favorite","reply","old","new"))
cList15 <- commP15$result$commentList
cList15 <- as.data.frame(cList15)

commP16 <- getComment(turl=url, pageSize=100, page=16, sort=c("favorite","reply","old","new"))
cList16 <- commP16$result$commentList
cList16 <- as.data.frame(cList16)

commP17 <- getComment(turl=url, pageSize=100, page=17, sort=c("favorite","reply","old","new"))
cList17 <- commP17$result$commentList
cList17 <- as.data.frame(cList17)

commP18 <- getComment(turl=url, pageSize=100, page=18, sort=c("favorite","reply","old","new"))
cList18 <- commP18$result$commentList
cList18 <- as.data.frame(cList18)

commP19 <- getComment(turl=url, pageSize=100, page=19, sort=c("favorite","reply","old","new"))
cList19 <- commP19$result$commentList
cList19 <- as.data.frame(cList19)

commP20 <- getComment(turl=url, pageSize=100, page=20, sort=c("favorite","reply","old","new"))
cList20 <- commP20$result$commentList
cList20 <- as.data.frame(cList20)

commP21 <- getComment(turl=url, pageSize=100, page=21, sort=c("favorite","reply","old","new"))
cList21 <- commP21$result$commentList
cList21 <- as.data.frame(cList21)

commP22 <- getComment(turl=url, pageSize=100, page=22, sort=c("favorite","reply","old","new"))
cList22 <- commP22$result$commentList
cList22 <- as.data.frame(cList22)

commP23 <- getComment(turl=url, pageSize=100, page=23, sort=c("favorite","reply","old","new"))
cList23 <- commP23$result$commentList
cList23 <- as.data.frame(cList23)

cList <- rbind(cList1,cList2,cList3,cList4,cList5,cList6,cList7,cList8,cList9,cList10,cList11,cList12,cList13,cList14,cList15,cList16,cList17,cList18,cList19,cList20,cList21,cList22,cList23)

naverNC <- subset(
cList,select=c(commentNo,parentCommentNo,modTime,regTime,userName,userIdNo,contents,sympathyCount,antipathyCount,replyCount))

write.csv(naverNC, paste("naverNewsComment",".csv",sep=""))
