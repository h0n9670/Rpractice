library("rJava")
library("memoise")
library("KoNLP")
library(stringr)
library(dplyr)
library(RColorBrewer)
library(wordcloud)

trouble=readLines("./remake.txt")
head(trouble)

txt <- str_replace_all(trouble, "\\W"," ")

nouns <- extractNoun(txt)
class(nouns)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
class(wordcount)
dim(wordcount)
View(wordcount)
class(nouns)
class(unlist(nouns))

#자주 사용된 단어 빈도표 만들기
# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors=F)
class(df_word)
dim(df_word)
summary(df_word)
df_word

# 변수명 수정
df_word <- rename(df_word, word=Var1,freq=Freq)
class(df_word)
dim(df_word)
summary(df_word)
df_word

#두 글자 이상 단어 추출
df_word <-  filter(df_word, nchar(word)>=2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

#단어 색상 목록 만들기
pal <- brewer.pal(8,"Dark2") #Dark2 색상 목록에서 8개 색상 추출

#워드 클라우드 생성
set.seed(1234)    #난수고정
wordcloud(words = df_word$word,#단어
          freq = df_word$freq,#빈도 freq
          min.freq = 2,#최소 단어 빈도
          max.words = 200,#표현단어 수
          random.order = F,#고빈도 단어 중앙배치
          rot.per=.1,#회전 단어 비율
          scale = c(4,0.3),#단어 크기 범위
          colors = pal) #색상 목록

#상위 20개만 가져오기
wordcloud(words = top_20$word,#단어
          freq = top_20$freq,#빈도 freq
          min.freq = 2,#최소 단어 빈도
          max.words = 200,#표현단어 수
          random.order = F,#고빈도 단어 중앙배치
          rot.per=.1,#회전 단어 비율
          scale = c(4,0.3),#단어 크기 범위
          colors = pal) #색상 목록