x <- c(1,2,3,4,1,2,3,1,2,1)
qplot(x)

qplot(data = mpg, x=hwy)

qplot(data = mpg, x = cty)

qplot(data = mpg, x = drv, y = hwy)

qplot(data = mpg, x = drv, y = hwy,geom = "line")

qplot(data = mpg, x = drv, y = hwy,geom = "boxplot")

qplot(data = mpg, x = drv, y = hwy,geom = "boxplot", colour = drv)

?qplot

mpg

install.packages("dplyr")
library(dplyr)

df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(2, 3, 2))
df_raw 

df_new <- df_raw  # 복사본 생성
df_new            # 출력


df_new <- rename(df_new, v2 = var2)  # var2 를 v2 로 수정
df_new

df <- data.frame(var1 = c(4, 3, 8), var2 = c(2, 6, 1)) 
df

df$var_sum <- df$var1 + df$var2  # var_sum 파생변수 생성
df 

df$var_mean <- (df$var1 + df$var2)/2  # var_mean 파생변수 생성
df

mpg$total <- (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성
head(mpg) 
mean(mpg$total) 

summary(mpg$total)  # 요약 통계량 산출
hist(mpg$total)     # 히스토그램 생성

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20)
table(mpg$test)

?ifelse()

qplot(mpg$test)

mpg$grade <- ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B", "C")) 
head(mpg, 20)

#####################################################
#문제 1. ggplot2 의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악하세요.
mw <- as.data.frame(ggplot2::midwest)
head(mw,20)
View(mw)

#문제 2. poptotal(전체 인구)을 total 로, popasian(아시아 인구)을 asian 으로 변수명을 수정하세요.

mw <- rename(mw,c(total = poptotal,asian = popasian))
head(mw)

#문제 3. total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요. 

mw$asian_per_total <- mw$asian/mw$total*100
head(mw$asian_per_total)
hist(mw$asian_per_total)

#문제 4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수를 만들어 보세요. 

mean(mw$asian_per_total)
mw$test <- ifelse(mw$asian_per_total>mean(mw$asian_per_total),"large","small")

#문제 5. "large"와 "small"에 해당하는 지역이 얼마나 되는지, 빈도표와 빈도 막대 그래프를 만들어 확인해 보세요. 
table(mw$test)
qplot(mw$test)

############################################################################

#filter() 행 추출 
#select() 열(변수) 추출 
#arrange() 정렬 
#mutate() 변수 추가 
#summarise() 통계치 산출 
#group_by() 집단별로 나누기 
#left_join() 데이터 합치기(열) 
#bind_rows() 데이터 합치기(행)

#조건에 맞는 데이터만 추출하기

exam <- read.csv(file.choose(), header=T)
exam

exam %>% filter(class == 1)
exam %>% filter(class == 2)
exam %>% filter(class != 1)
exam %>% filter(class != 3)
# 수학 점수가 50 점을 초과한 경우
exam %>% filter(math > 50)
# 수학 점수가 50 점 미만인 경우
exam %>% filter(math < 50)
# 영어점수가 80 점 이상인 경우
exam %>% filter(english >= 80)
# 1 반 이면서 수학 점수가 50 점 이상인 경우
exam %>% filter(class == 1 & math >= 50)
exam %>% filter(class == 1 | class == 3 | class == 5)  # 1, 3, 5 반에 해당되면 추출
exam %>% filter(class %in% c(1,3,5))  # 1, 3, 5 반에 해당하면 추출출


##논리 연산자
#< 작다 
#<= 작거나 같다 
#> 크다 
#>= 크거나 같다 
#== 같다 
#!= 같지 않다 
#│ 또는 
#& 그리고 
#%in% 매칭 확인

##산술연산자
# + 더하기 
# - 빼기 
# * 곱하기 
# / 나누기 
# ^ , ** 제곱 
# %/% 나눗셈의 몫 
# %% 나눗셈의 나머지

exam %>% select(math)  # math 추출
exam %>% select(class, math, english)  # class, math, english 변수 추출
exam %>% select(-math, -english)  # math, english 제외
exam %>% filter(class == 1) %>% select(english)
exam %>%   select(id, math) %>% head    # 앞부분 6 행까지 추출

##차순 정리
exam %>% arrange(math)  # math 오름차순 정렬
exam %>% arrange(desc(math))  # math 내림차순 정렬
exam %>% arrange(class, math)  # class 및 math 오름차순 정렬


##파생변수 추가
exam %>%   mutate(total = math + english + science) %>%  head
exam %>%   mutate(total = math + english + science,          # 총합 변수 추가
                  mean = (math + english + science)/3) %>%   # 총평균 변수 추가
  head                                           

exam %>%   mutate(test = ifelse(science >= 60, "pass", "fail")) %>%   head

##집단별로 요약하기
exam %>% summarise(mean_math = mean(math))  # math 평균 산출

exam %>%   group_by(class) %>%                # class 별로 분리
  summarise(mean_math = mean(math))  # math 평균 산출

exam %>%   group_by(class) %>%                   # class 별로 분리
  summarise(mean_math = mean(math),     # math 평균
            sum_math = sum(math),       # math 합계
            median_math = median(math), # math 중앙값
            n = n())                    # 학생 

mpg %>%   group_by(manufacturer) %>%           # 회사별로 분리
  filter(class == "suv") %>%           # suv 추출
  mutate(tot = (cty+hwy)/2) %>%        # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>%  # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>%          # 내림차순 정렬
  head(5)                              # 1~5 위까지 출력

##요약통계량 함수
# mean() 평균 
# sd() 표준편차 
# sum() 합계 
# median() 중앙값 
# min() 
# 최솟값 
# max() 최댓값 
# n() 빈도

##데이터 합치기
#가로로합치기
# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5), midterm = c(60, 80, 70, 90, 85)) 

# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5), final = c(70, 83, 65, 95, 80))

total <- left_join(test1, test2, by = "id")  # id 기준으로 합쳐 total 에 할당
total                                        # total 출력

#교사 데이터 생성
name <- data.frame(class = c(1, 2, 3, 4, 5), teacher = c("kim", "lee", "park", "choi", "jung")) 
name

exam_new <- left_join(exam, name, by = "class")
exam_new                    

#세로로 합치기
# 학생 1~5 번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5), test = c(60, 80, 70, 90, 85)) 

# 학생 6~10 번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10), test = c(70, 83, 65, 95, 80))

group_all <- bind_rows(group_a, group_b)  # 데이터 합쳐서 group_all 에 할당
group_all                                 # group_all 출력

## 빠진 데이터를 찾아라! - 결측치 정제하기
#결측치(Missing Value)
df <- data.frame(sex = c("M", "F", NA, "M", "F"), score = c(5, 4, 3, 4, NA))
df

#결측치 확인
is.na(df)         # 결측치 확인
table(is.na(df))  # 결측치 빈도 출력
table(is.na(df$sex))    # sex 결측치 빈도 출력
table(is.na(df$score))  # score 결측치 빈도 출력

#결측치 포함한 상태로 연산
mean(df$score)  # 평균 산출
sum(df$score)   # 합계 산출

#결측치 제거
df %>% filter(is.na(score))   # score 가 NA 인 데이터만 출력
df %>% filter(!is.na(score))  # score 결측치 제거
df_nomiss <- df %>% filter(!is.na(score))  # score 결측치 제거
mean(df_nomiss$score)                      # score 평균 산출
sum(df_nomiss$score)                       # score 합계 산출

#여러변수 결측치 제거
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

df_nomiss2 <- na.omit(df)  # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2                 # 출력

#결측치 제외 기능 이용하기
mean(df$score, na.rm = T)  # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T)   # 결측치 제외하고 합계 산출

# 결측치 이용
exam <- read.csv(file.choose(), header = T)            # 데이터 불러오기
exam
exam[c(3, 8, 15), "math"] <- NA             # 3, 8, 15 행의 math 에 NA 할당
exam
exam %>% summarise(mean_math = mean(math))             # 평균 산출
exam %>% summarise(mean_math = mean(math, na.rm = T))  # 결측치 제외하고 평균 산출

exam$math <- ifelse(is.na(exam$math), 55, exam$math)  # math 가 NA 면 55 로 대체
table(is.na(exam$math))                               # 결측치 빈도표 생성

#이상치
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1), score = c(5, 4, 3, 4, 2, 6))
outlier
table(outlier$sex)
table(outlier$score)

# sex 가 3 이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
# sex 가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

#결측치 제외하고 분석
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

#상자그림으로 극단치 기준 정해서 제거하기
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats  # 상자그림 통계치 출력력

##시각화

#배경 설정하기
ggplot(data = mpg, aes(x = displ, y = hwy))

# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

# x 축 범위 3~6 으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

# x 축 범위 3~6, y 축 범위 10~30 으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)

## ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
# 집단별 평균표 만들기
df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy = mean(hwy))
df_mpg

#그래프 생성하기
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

#크기순으로 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

# x 축 범주 변수 , y 축 빈도
ggplot(data = mpg, aes(x = drv)) + geom_bar()

# x 축 연속 변수 , y 축 빈도
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

# • 평균 막대 그래프 : 데이터를 요약한 평균표를 먼저 만든 후 평균표를 이용해 그래프 생성 - geom_col() 
# • 빈도 막대 그래프 : 별도로 표를 만들지 않고 원자료를 이용해 바로 그래프 생성 - geom_bar()

#시계열 그래프 만들기
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

#상자 그림 만들기
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

## 요약하기

# 1. 산점도
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() 

# 축 설정 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) +   geom_point() +   xlim(3, 6) +   ylim(10, 30) 

# 2. 평균 막대 그래프


# 1 단계 . 평균표 만들기
df_mpg <- mpg %>%   group_by(drv) %>%   summarise(mean_hwy = mean(hwy)) 

# 2 단계 . 그래프 생성하기 , 크기순 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col() 

# 3. 빈도 막대 그래프
ggplot(data = mpg, aes(x = drv)) + geom_bar() 


# 4. 선 그래프
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line() 

# 5. 상자 그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

##한국인의 삶을 파악하라
#한국복지패널데이터 분석 준비하기

install.packages("foreign") #foreign 패키지 설치
library(foreign)
library(dplyr)
library(ggplot2)
install.packages("readxl")
library(readxl)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

#데이터 검토하기
View(raw_welfare)
str(raw_welfare)
dim(raw_welfare)
summary(raw_welfare)

# 변수명 바꾸기
raw_welfare <-rename(raw_welfare,               
                     sex = h10_g3,
                     birth = h10_g4,                  
                     marriage = h10_g10, 
                     religion = h10_g11,
                     income = p1002_8aq1,
                     code_job = h10_eco9,
                     code_region = h10_reg7
                     )

#데이터 분석 절차
# 1. 변수 검토 및 전처리
# 2. 변수 간 관계 분석

#성별에 따라 월급이 다를까?

#성별
#type  확인
class(raw_welfare$sex)

#이상치 확인
table(raw_welfare$sex)

#이상치 결측 처리
raw_welfare$sex <-  ifelse(raw_welfare$sex == 9, NA, raw_welfare$sex)
table(is.na(raw_welfare$sex))

#성별 항목 이름 부여
raw_welfare$sex <-  ifelse(raw_welfare$sex == 1, "male", "female")
table(raw_welfare$sex)

#시각화
qplot(raw_welfare$sex)

#월급
#타입확인
class(raw_welfare$income)

#이상치 확인
summary(raw_welfare$income)

qplot(raw_welfare$income)
qplot(raw_welfare$income)+xlim(0,1000)

#이상치 결측 처리
raw_welfare$income <- ifelse(raw_welfare$income %in% c(0,9999),NA,raw_welfare$income)
table(is.na(raw_welfare$income))

#성별 월급 평균표 만들기
sex_income <- raw_welfare %>%
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

# 차드 만들기
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 나이와 월급 의 관계(몇 살 때 월급을 가장 많이 받을까?) 

# 변수 검토하기
class(raw_welfare$birth)
summary(raw_welfare$birth)
qplot(raw_welfare$birth)

table(is.na(raw_welfare$birth))

raw_welfare$birth <-  ifelse(raw_welfare$birth == 9999, NA, raw_welfare$birth)
table(is.na(raw_welfare$birth))

# 월급 전처리 생략

# 파생변수 만들기 - 나이
raw_welfare$age <- 2015 - raw_welfare$birth + 1
summary(raw_welfare$age)

# 나이에 따른 평균 급여 표 만들기
age_income <-  raw_welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#어떤 연령대의 웕브이 가장 많을까?

raw_welfare <-  raw_welfare %>% 
  mutate(ageg = ifelse(age < 30, "Young",
                       ifelse(age<=59,"middle",'old')))

table(raw_welfare$ageg)

# 연령대별 우러급 평균표 만들기

ageg_income <-  raw_welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income

ggplot(data = ageg_income, aes(x = ageg, y = mean_income))+geom_col()

# 막대 정렬 : 초년 ,중년, 노년 나이 순

ggplot(data = ageg_income, aes(x = ageg, y = mean_income))+geom_col()+scale_x_discrete(limits = c("Young","middle","old"))

# 성별 월급 차이는 연령대 별로 다를까?

#연령대 및 성별 월급 평균표 만들기
sex_income <-  raw_welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex))+geom_col()+scale_x_discrete(limits = c("Young","middle","old"))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits = c("Young","middle","old"))

# 성별 연령별 월급 평균표 만들기
sex_age <-  raw_welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex))+geom_line()
