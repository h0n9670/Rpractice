# 10-2 국정원 트위 텍스트 마이닝
# 국정원 계정 트윗 데이터
# - 국정원 대선 개입 사실이 밝혀져 논란이 됐던 2013년 6월 독립 언론 뉴스타파가 인터넷을 통해
# - 국정원 계정으로 작성된 3,744개 트윗

#데이터 로드
twitter <- read.csv("../twitter.csv",
                    header = T,
                    stringsAsFactors=F,
                    fileEncoding = "UTF-8")
View(twitter)
#변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)

head(twitter)
View(twitter)

#특수문자 제거
twitter$tw <- str_replace_all(twitter$tw,"\\W"," ")

#단어 빈도표 만들기
# 트윗에서 명사 추출
nouns <- extractNoun(twitter$tw)
class(nouns)
View(nouns)

wordlist <- table(unlist(nouns))
View(wordlist)

df_word <-  as.data.frame(wordlist,stringsAsFactors =F)

df_word <-  rename(df_word, word = Var1, freq = Freq)

df_word <-  filter(df_word, nchar(word)>=3)
View(df_word)
top_40 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(40)
top_40

#단어 빈도 막대 그래프 만들기
library(ggplot2)

order <- arrange(top_40,freq)$word            #빈도 순서 변수 생성

ggplot(data = top_40,aes(x = word, y = freq))+
  ylim(0,2500)+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limit = order)+        # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label=freq),hjust = -0.3)  #빈도 표시

library(devtools)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(base64enc)
library(tm)
library(wordcloud2)

# 3.워드클라우드 그리기(기본)
wordcloud2(top_40)

#3.1 wordcloud2 크기, 색변경(size,, color)
wordcloud2(top_40, size=0.5, col ="random-dark")

#3.2 키워드 회전 정도 조절(rotateRatio)
wordcloud2(top_40,size=0.5, col="random-dark",rotateRatio = 0)

##3.3 배경 색 검정(backgroundColor)
wordcloud2(top_40,size=0.5, col="random-dark",backgroundColor = "red")

