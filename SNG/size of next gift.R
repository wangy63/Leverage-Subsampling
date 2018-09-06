library(data.table)

entity<-read.csv("1_Entity.csv")
names(entity)
entity_clean<-entity[, c(1, 3, 6, 7)]
athletics <- read.csv("2_Athletics.csv")
names(athletics)
athletics_clean<-athletics[, c(1, 2)]
entity_clean$athlete<-0
entity_clean$athlete[entity$Entity.ID %in% athletics$Entity.ID] <- 1


givingdata<-read.csv("3_givingdata.csv")
names(givingdata)
giving_clean<-givingdata[, c(2, 3, 5)]
giving_clean$Transaction.year <- substr(giving_clean$Transaction.Date,7,10)

Degree<-read.csv("4_Degree.csv")
names(Degree)
degree_clean<-Degree[, c(1, 2, 3, 4)]
degree_clean<-cbind(degree_clean, d=rep(1, nrow(degree_clean)))%>%
  group_by(Entity.ID)%>%
  summarise(count.Degree=sum(d, na.rm=T), 
            year=max(Degree.Year, na.rm=T),
            School=School.of.Graduation[1])

donate_year<-merge(degree_clean, giving_clean, by="Entity.ID")
donate_year<-donate_year[, c(1, 2, 3, 4, 5, 7)]

donate_year<-cbind(donate_year, d=rep(1, nrow(donate_year)))%>%
  group_by(Entity.ID, Transaction.year)%>%
  summarise(Amount=sum(Legal.Amount, na.rm=T),
            count.Degree=count.Degree[1],
            School=School[1], 
            year=year[1])
                         
donate_year<-merge(donate_year, entity_clean, by="Entity.ID", all=T)

contact <- read.csv("5_ContactInformation.csv")
names(contact)
contact_clean<-contact[, c(1, 2, 3, 6)]
contact_clean <- contact_clean %>%
  filter(Preferred.Indicator == 1 & Entity.ID != "")

donate_year<-merge(donate_year, contact_clean, by="Entity.ID", all=T)

part <- read.csv("7_ParticipationHistory.csv")            # participation in activities, including dates
part<-dcast(part, Entity.ID ~ Participation.Category)     # turns raw values into column names
part<-part%>%mutate(Part.Level=apply(part[,-1], 1, sum), Greek=FRTTY+SOROR)
# Part.Level includes all not just these four/five
part<-filter(part, Greek<4)%>%select(Entity.ID, SRVCE, ALUEV, CHAPT, REUN, FRTTY, SOROR, Greek, Part.Level)
donate_year<-merge(donate_year, part, by="Entity.ID", all=T)
donate_year[is.na(donate_year)] <- 0
donate_year<-donate_year[which(donate_year$Transaction.year!=0), ]
#us.state <- read.csv("state_table.csv")
#us.state <- us.state %>% select(name, abbreviation, census_region_name, census_division_name)
#colnames(us.state)<- c("State.Full", "State", "Region", "SubRegion")
#us.state<-data.table(us.state[, c(1,2)])
#colnames(us.state)<-c("State", "Acronym")
final<-donate_year
final$Transaction.year<-as.numeric(final$Transaction.year)
final$year<-as.numeric(final$year)
final$GradToGive<-final$Transaction.year-final$year


###################################################

groupfinal <- final %>% 
  arrange(Entity.ID,Transaction.year)%>%
  group_by(Entity.ID) %>%
  mutate(rank=row_number())

groupfinal$lastGave<-NA
for (i in 1: nrow(groupfinal)){
  if (groupfinal$rank[i]!=1){
  print(i)
  k<-i-1
groupfinal$lastGave[i]<-groupfinal$Amount[k]
  } else {
    groupfinal$lastGave[i]<-0
}
}

groupfinal$GaveLastYear<-NA

ptm <- proc.time()
for (i in 1: nrow(groupfinal)){
  print(i)
  if (groupfinal$rank[i]!=1){
    k<-i-1
    if (groupfinal$Transaction.year[i]-1==groupfinal$Transaction.year[k]){
      groupfinal$GaveLastYear[i]="Yes"
    }   else {
      groupfinal$GaveLastYear[i]="No"
    } 
  } else {
    groupfinal$GaveLastYear[i]<- "No"
  }
}

proc.time() - ptm

SNGdata<-SNGdata[, c(-1, -24)]
SNGdata$Greek <- ifelse(SNGdata$Greek == 0,0,1)

write.csv(SNGdata, "SNGdata.csv")













