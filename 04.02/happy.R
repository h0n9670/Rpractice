##한국인의 삶을 파악하라
#한국복지패널데이터 분석 준비하기

install.packages("foreign") #foreign 패키지 설치
library(foreign)
library(dplyr)
library(ggplot2)
install.packages("readxl")
library(readxl)

welfare <- read.spss(file = "../Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

#데이터 검토하기
View(welfare)
str(welfare)
dim(welfare)
summary(welfare)

# 변수명 바꾸기
welfare <-rename(welfare,               
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
class(welfare$sex)

#이상치 확인
table(welfare$sex)

#이상치 결측 처리
welfare$sex <-  ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))

#성별 항목 이름 부여
welfare$sex <-  ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

#시각화
qplot(welfare$sex)

#월급
#타입확인
class(welfare$income)

#이상치 확인
summary(welfare$income)

qplot(welfare$income)
qplot(welfare$income)+xlim(0,1000)

#이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))

#성별 월급 평균표 만들기
sex_income <- welfare %>%
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

# 차드 만들기
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 나이와 월급 의 관계(몇 살 때 월급을 가장 많이 받을까?) 

# 변수 검토하기
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

table(is.na(welfare$birth))

welfare$birth <-  ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 월급 전처리 생략

# 파생변수 만들기 - 나이
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)

# 나이에 따른 평균 급여 표 만들기
age_income <-  welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#어떤 연령대의 웕브이 가장 많을까?

welfare <-  welfare %>% 
  mutate(ageg = ifelse(age < 30, "Young",
                       ifelse(age<=59,"middle",'old')))

table(welfare$ageg)

# 연령대별 우러급 평균표 만들기

ageg_income <-  welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income

ggplot(data = ageg_income, aes(x = ageg, y = mean_income))+geom_col()

# 막대 정렬 : 초년 ,중년, 노년 나이 순

ggplot(data = ageg_income, aes(x = ageg, y = mean_income))+geom_col()+scale_x_discrete(limits = c("Young","middle","old"))

# 성별 월급 차이는 연령대 별로 다를까?

#연령대 및 성별 월급 평균표 만들기
sex_income <-  welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex))+geom_col()+scale_x_discrete(limits = c("Young","middle","old"))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits = c("Young","middle","old"))

# 성별 연령별 월급 평균표 만들기
sex_age <-  welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex))+geom_line()

class(welfare$code_jab)

#직업분류코드 목록 불러오기
list_job <-  read_excel("../Koweps_Codebook.xlsx",col_names = T,sheet=2)
head(list_job)
## A tibble: 6 x 2
# code_job job                                
# <dbl> <chr>                              
#   1      111 의회의원 고위공무원 및 공공단체임원
# 2      112 기업고위임원                       
# 3      120 행정 및 경영지원 관리자            
# 4      131 연구 교육 및 법률 관련 관리자      
# 5      132 보험 및 금융 관리자                
# 6      133 보건 및 사회복지 관련 관리자  

dim(list_job)
# [1] 149   2

welfare <- left_join(welfare,list_job,id="code_job")
## Joining, bt="code_job"

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)
# code_job                                job
# 1       942                   경비원 및 검표원
# 2       762                             전기공
# 3       530 방문 노점 및 통신 판매 관련 종사자
# 4       999        기타 서비스관련 단순 종사원
# 5       312                    경영관련 사무원
# 6       254             문리 기술 및 예능 강사
# 7       510                        영업 종사자
# 8       530 방문 노점 및 통신 판매 관련 종사자
# 9       286   스포츠 및 레크레이션 관련 전문가
# 10      521                   매장 판매 종사자

#직업별 월급 차이 분석하기
#1. 직업별 월급 평균표 만들기
job_income<-welfare%>%
  filter(!is.na(job)&!is.na(income))%>%
  group_by(job)%>%
  summarise(mean_income=mean(income))

head(job_income)

#상위 10개 추출
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)########################################

ggplot(data = top10,aes(x=reorder(job, mean_income),y=mean_income))+
  geom_col()+
  coord_flip()

#하위 10개 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

ggplot(data = bottom10,aes(x=reorder(job, -mean_income),y=mean_income))+
  geom_col()+
  coord_flip()+
  ylim(0,850)

#남성 직업 빈도 상위 10개 추출
job_male <- welfare %>%
  filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

job_male

#여성 직업 빈도 상위 10개 추출
job_female<-welfare%>%
  filter(!is.na(job)&sex=="female")%>%
  group_by(job)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)

job_female

#종교 유무에 따른 이혼율

# 종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, "yes","no")
table(welfare$religion)

qplot(welfare$religion)

# 혼인 상태 변수 검토 및 전처리하기

class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                ifelse(welfare$marriage == 3, "divorece",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

religion_marriage

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100,1))

religion_marriage

#  이혼율 표 만들기
divorce <- religion_marriage %>% 
  filter(group_marriage == "divorece") %>% 
  select(religion,pct)

divorce##################3

ggplot(data = divorce,aes(x=religion, y=pct))+geom_col()

#연령대별 이혼율표
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))

ageg_marriage

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg,group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100,1))

ageg_marriage

#초년 제외, 이혼 추출
ageg_divorce <- ageg_marriage %>% 
  filter(ageg !="Young" & group_marriage=="divorece") %>% 
  select(ageg, pct)

ageg_divorce

ggplot(data = ageg_divorce, aes(x=ageg, y = pct)) + geom_col()

# 연령대, 종교유무, 결혼상태별 비율표 만들기
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg !="Young") %>% 
  group_by(ageg,religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
ageg_religion_marriage

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

ageg_religion_marriage

#연령대 및 종교 유무별 이혼율 표
df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == "divorece") %>% 
  select(ageg, religion, pct)

df_divorce