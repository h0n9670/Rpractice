#지역별 순이동에 따른 워드 클라우드
word <- c("강남","강북","노원","수서","여의도")
frequency <- c(651,340,61,240,430)

wordcloud(word, frequency,colors=c("blue","darkblue","red","green"))

#단어들의 색 변환
wordcloud(word,
          frequency,
          random.order = T,
          random.color = T,
          colors=rainbow(length(word))
          )

# 다양한 단어 색 출력을 위한 팔레트 패키지의 활용
pal2 <-  brewer.pal(8,"Dark2")

word <- c("강남","강북","노원","수서","여의도")
frequency <- c(651,340,61,240,430)

wordcloud(word, frequency,colors=pal2)


# 다운로드 사이트 : http://kostat.go.kr/
#   
#   작업 순서
# 
# 1. 데이터 파일 읽기 : 6_101_DT_1B26001_A01_M.csv(국내 인구이동 데이터파일)
# 
# 2. '전국' 지역이 아닌 데이터만 추출('전국' 지역 데이터 제외)
# 
# 3. 행정구역 중 '구' 단위에 해당하는 행 번호 추출
# 4. '구' 지역 데이터 제외
# 
# 5. 순이동 인구수가 0보다 큰지역 추출 
# 
# 6. 단어(행정 구역) 할당
# 
# 8. 워드클라우드 출력
# 
# 
# 
# 필요한 함수들
# 1. 데이터 파일 읽기 : read.csv(file.choose(), header=T)
# 
# 2. '전국' 지역이 아닌 데이터만 추출('전국' 지역 데이터 제외) : data[data$행정구역.시군구.별 != "전국", ]
# 
# 3. 행정구역 중 '구' 단위에 해당하는 행 번호 추출 : grep("구$", data2$행정구역.시군구.별)
# 4. '구' 지역 데이터 제외 : <- data2[-c(x), ]
# 
# 5. 순이동 인구수가 0보다 큰지역 추출 : data3[data3$순이동.명>0, ]
# 
# 6. 단어(행정 구역) 할당 : data4$행정구역.시군구.별
# 
# 7. 행정구역별 빈도 : data4$순이동.명
# 
# 8. 워드클라우드 출력 : wordcloud()

data = read.csv(file.choose(),header=T)
data2 = data[data$행정구역.시군구.별!="전국",]
x = grep("구$",data2$행정구역.시군구.별)
data3 <- data2[-c(x),]
data4 = data3[data3$순이동.명>0]
word <-data4$행정구역.시군구.별
frequency <- data4$순이동.명
pal2 <-  brewer.pal(length(frequency),"Dark2")

wordcloud(word,frequency,random.order = T,
          random.color = T,colors=pal2)




useSejongDic()
pal2 <- brewer.pal(8,"Dark2")

text <- readLines(file.choose())
text

noun <- sapply(text, extractNoun, USE.NAMES=F) #행렬로 반환
noun

noun2 <- unlist(noun)
noun2

#p.221
word_count <- table(noun2)
word_count

wordcloud(names(word_count),
          freq=word_count,
          scale=c(6,0.3),
          min.freq=3,
          random.order = F,
          rot.per=.1,
          colors=pal2
          )

text1 <- readLines(file.choose())
text1

nounn <- sapply(text1, extractNoun, USE.NAMES=F) #행렬로 반환
nounn

nounn2 <- unlist(nounn)
nounn2

#p.221
word_countt <- table(nounn2)
word_countt

wordcloud(names(word_countt),
          freq=word_countt,
          scale=c(6,0.3),
          min.freq=3,
          random.order = F,
          rot.per=.1,
          colors=pal2
)

