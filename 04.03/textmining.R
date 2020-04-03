# 택스트 마이닝(text mining)
# 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법
# SNS 나 웹 사이트에 올라온 글을 분석해 사람들이 어떤 이야기를 나누고 있는지 파악할 때 활용
# 형태소 분석 :문장을 구성하는 어절들이 어떤 품사로 되어 있는 지 분석
# 분석 절차
# - 형태소 분석
# - 명사, 동사, 형용사 등 의미를 지닌 품사 단어 추출
# - 빈도수 만들기
# - 시각화

# 10.1 힙합 가사 택스트 마이닝
# 택스트 마이닝 준비하기

# 패키지 설치 및 로드
# 패키지 설치
library("rJava")
library("memoise")
library("KoNLP")

#데이터 준비
#데이터 불러오기
txt <- readLines("./hiphop.txt")
View(txt)

#특수문자 제거
library(stringr)
#문자열을 다루는 다양한 방법을 가지고 있는 패키지

#특수문자 제거
txt <- str_replace_all(txt, "\\W"," ")
View(txt)

#명사 추출하기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")

#기사에서 명사추출
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

library(dplyr)
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

#패키지 준비하기
#패키지 설치
library(RColorBrewer)
library(wordcloud)

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
