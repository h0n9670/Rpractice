 
library(devtools)
library(ggmap)
library(dplyr) 

crime_address <- read.csv("./crime_in_Seoul_address.csv")

head(crime_address)

str(crime_address)

crime_address_code <- as.character(crime_address$주소)

googleAPIkey <- "AIzaSyDnC8_26g3cfLwpjyKBIEaxpstMdJDA9bM"
register_google(googleAPIkey)

crime_address_code <- geocode(crime_address_code)
View(crime_address_code)

crime_address_code <- as.character((crime_address$주소)) %>% enc2utf8() %>% geocode()

head(crime_address_code)

crime_address_final <- cbind(crime_address, crime_address_code)

head(crime_address_final)

seoul_map <- get_googlemap("seoul", maptype = "roadmap",zoom = 12)

ggmap(seoul_map)+
  geom_point(data = crime_address_final,
             aes(x=lon,y=lat),
             colour = "red",
             size = 3)+
  geom_text(data = crime_address_final,
            aes(label = 관서명, vjust=-1))

#-----------------------------------------------------------------------------
crime <- read.csv("./crime_in_Seoul.csv")

head(crime)

str(crime)
crime$절도.발생 <- gsub(",","", crime$절도.발생)
crime$폭력.발생 <- gsub(",","", crime$폭력.발생)
crime$폭력.검거 <- gsub(",","", crime$폭력.검거)

crime <- crime %>% 
  mutate(total_arrest=살인.검거+강도.검거+강간.검거+as.integer(절도.검거)+as.integer(폭력.검거))

crime <- crime %>% 
  mutate(total_occur=살인.발생+강도.발생+강간.발생+as.integer(절도.발생)+as.integer(폭력.발생))
head(crime)

crime <- crime %>% 
  mutate(arrest_rate=round(total_arrest/total_occur*100))
head(crime)


seoul_occur <- sum(crime$total_occur)

crime$occur_rate <- round(crime$total_occur/seoul_occur*100,3)

head(crime$occur_rate)

crime <- left_join(crime,crime_address_final,by="관서명")

head(crime)
ggmap(seoul_map)+
  geom_point(data = crime,
             aes(x=lon,y=lat),
             colour = "red",
             size = 3)+
  geom_text(data = crime,
            aes(label = 관서명, vjust=-1))+
  geom_text(data = crime,
            aes(label = occur_rate, vjust=1),colour='blue',size =3)+
  geom_text(data = crime,
            aes(label = arrest_rate, vjust=2),colour='brown',size=3)
#-------------------------------------------------------------------------------