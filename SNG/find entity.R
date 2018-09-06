# why we have only 24,000 entities?

library(dplyr)
library(tidyr)
library(leaps)
library(reshape2)
library(glmnet)
library(BMA)
library(bestglm)

setwd("/Users/wangyuexi/Desktop/Yuexi/data")
#entity
entity<-read.csv("1_Entity.csv")
names(entity)
entity<-entity[, c(1, 3, 6)]
n_occur <- data.frame(table(entity$Entity.ID))
entity[entity$Entity.ID %in% n_occur$Var1[n_occur$Freq > 1],]
entity<-entity[which(entity$Entity.ID!=''),]
entity$degree<-entity$Primary.Relationship.Type
entity<-mutate(entity, Degree.Category = ifelse(degree == "AG", "Graduate",
                                                ifelse(degree == "AR", "Associate",
                                                       ifelse(degree == "AU", "Undergraduate",
                                                              "Other"))))

entity<-entity[, c(-2, -4)]


### athletics
athletics <- read.csv("2_Athletics.csv")                  # athletics information, including dates
athletics<-athletics%>%distinct(Entity.ID)                # assumes that if in dataset, they are an athlete
athletics<-cbind(athletics, athlete=rep(1, nrow(athletics)))
#merge athletics and entity
entity$athlete<-0
entity$athlete[entity$Entity.ID %in% athletics$Entity.ID] <- 1

###degree
Degree<-read.csv("4_Degree.csv")
names(Degree)
Degree<-Degree[, c(1,2,3)]
degree.2<-cbind(Degree, d=rep(1, nrow(Degree)))%>%
  group_by(Entity.ID)%>%
  summarise(count.Degree=sum(d, na.rm=T), 
            year=max(Degree.Year, na.rm=T),
            School=School.of.Graduation[1])

degree_entity<-merge(degree.2, entity, by="Entity.ID")

###giving data
givingdata<-read.csv("3_givingdata.csv")
names(givingdata)
givingdata<-givingdata[, c(2, 3, 5)]
givingdata$Transaction.Date<-substr(givingdata$Transaction.Date, 7, 20)
giving_allinfo<-merge(givingdata, degree_entity, by="Entity.ID")


giving_allinfo %>% group_by(Entity.ID) %>% summarize(count=n())

giving_allinfo$Transaction.Date<-as.numeric(giving_allinfo$Transaction.Date)
giving_allinfo$donate_year<-giving_allinfo$Transaction.Date - giving_allinfo$year

##remove NA's
giving_allinfo<-na.omit(giving_allinfo)
#subset first 10 year
between<-giving_allinfo[which(giving_allinfo$Transaction.Date >= 1986 & giving_allinfo$Transaction.Date <= 2007),]
between %>% group_by(Entity.ID) %>% summarize(count=n())

before1986<-giving_allinfo[which(giving_allinfo$Transaction.Date < 1986), ]
before1986 %>% group_by(Entity.ID) %>% summarize(count=n())

after2007<-giving_allinfo[which(giving_allinfo$Transaction.Date >2007), ]
after2007 %>% group_by(Entity.ID) %>% summarize(count=n())

top10year <- between[which(between$donate_year >0 & between$donate_year <= 10),]
top10year %>% group_by(Entity.ID) %>% summarize(count=n())

attach(top10year)
top10year$year[year <= 1990] <- "Elder"
top10year$year[year <= 2000 & year > 1990] <- "Middle Aged"
top10year$year[year <=2007 & year > 2000] <- "Young"
detach(top10year)

top10.year_clean <- top10year %>%
  group_by(Entity.ID) %>%
  summarize(totalAmt = sum(Legal.Amount, na.rm = TRUE), count.Degree=count.Degree[1], 
            School=School[1], year=year[1], Gender=Gender[1], athlete=athlete[1], Degree.Category=Degree.Category[1]) %>%
  mutate(logSum = log(totalAmt))


### participation
ParticipationHistory<-read.csv("7_ParticipationHistory.csv")
names(ParticipationHistory)
part<-dcast(ParticipationHistory, Entity.ID ~ Participation.Category)     # turns raw values into column names
part<-part%>%mutate(Part.Level=apply(part[,-1], 1, sum), Greek=FRTTY+SOROR)
part<-part[, c(1, 19)]
final<-merge(top10.year_clean,part,by="Entity.ID", all=T)
final$Greek[is.na(final$Greek)] <- 0
final %>% group_by(Entity.ID) %>% summarize(count=n())

final<-na.omit(final)
final<-final[, 3:10]
final$count.Degree<-as.numeric(final$count.Degree)
final$Greek<-as.numeric(final$Greek)
final$athlete<-as.character(final$athlete)



top10.year_clean<-final

top10.year_clean$athlete<- ifelse(top10.year_clean$athlete == 0, "No", "Yes")
top10.year_clean<-top10.year_clean[which(top10.year_clean$Gender!="N"),]

top10.year_clean<-top10.year_clean[which(top10.year_clean$logSum!=-Inf),]
## write csv
write.csv(top10.year_clean, "top10.year_clean.csv")

