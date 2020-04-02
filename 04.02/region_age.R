#지역별 연령대 비율
#노년층이 많은 지역은 어디일까?

#분석 절차
# 변수 검토 및 전처리
# 지역
# 연령대

#변수 간 관계 분석
#지역별 연령대 비율표 만들기
#그래프 만들기

# 지역(code_region)별 연령대 비율 분석하기
# 
# 1."서울"
# 2."수도권(인천/경기)"
# 3."부산/경남/울산",
# 4."대구/경북",
# 5."대전/충남",
# 6."강원/충북",
# 7."광주/전남/전북/제주도"

install.packages("foreign") #foreign 패키지 설치
library(foreign)
library(dplyr)
library(ggplot2)
install.packages("readxl")
library(readxl)

welfare <- read.spss(file = "../Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

welfare <-rename(welfare,               
                 sex = h10_g3,
                 birth = h10_g4,                  
                 marriage = h10_g10, 
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7
)

welfare_r_b <- welfare[c("code_region","birth")]

#code_region 가공

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

welfare_r_b <- left_join(welfare_r_b, list_region, id = "code_region")

# #이상치 제거
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==9999,NA ,welfare_r_b$code_region)
# 
# #NA값 확인
# table(is.na(welfare_r_b$code_region))
# 
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==1,"서울", welfare_r_b$code_region)
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==2,"수도권(인천,경기)", welfare_r_b$code_region)
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==3,"부산/경남/울산", welfare_r_b$code_region)
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==4,"대구/경북", welfare_r_b$code_region)
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==5,"대전/충남", welfare_r_b$code_region)
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==6,"강원/충북", welfare_r_b$code_region)
# welfare_r_b$code_region <- ifelse(welfare_r_b$code_region ==7,"광주/전남/전북/제주도", welfare_r_b$code_region)
# 
# welfare_r_b$code_region

#birth가공

welfare_r_b$birth <-  ifelse(welfare_r_b$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 파생변수 만들기 - 나이
welfare_r_b$age <- 2015 - welfare_r_b$birth + 1
summary(welfare_r_b$age)

# 연령대 설정
welfare_r_b <-  welfare_r_b %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age<=59,"middle",'old')))

# ageg_region <- welfare_r_b %>%
#   count(code_region, ageg) %>% 
#   group_by(code_region) %>%
#   mutate(pct = round(n/sum(n)*100,1)) %>% 
#   arrange(code_region,pct)
# 
# 
# ageg_region
# 
# ggplot(data = ageg_region, aes(x = code_region, y = pct, fill=ageg))+geom_col()+coord_flip()

region_ageg <-  welfare_r_b %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

region_ageg

#count
region_ageg <-  welfare_r_b %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100,1))

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg))+geom_col()+coord_flip()

#노년층 비율 내림차순 정렬
list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)

list_order_old

# 지역명 순서 변수 만들기
order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y=pct, fill = ageg))+geom_col()+coord_flip()+scale_x_discrete(limits = order)
