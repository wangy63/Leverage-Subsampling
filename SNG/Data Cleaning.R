# regsubsets

library(dplyr)
library(plyr)
library(reshape2)

# read in data
setwd("C:\\Users\\Chelsea\\Documents\\STA 660\\Project 4\\data\\Spreadsheets")

entity <- read.csv("1_Entity.csv")                        # relationship information, demographics  
entity<-entity%>%filter(Entity.ID!="")

athletics <- read.csv("2_Athletics.csv")                  # athletics information, including dates
athletics<-athletics%>%distinct(Entity.ID)                # assumes that if in dataset, they are an athlete
athletics<-cbind(athletics, athlete=rep(1, nrow(athletics)))

giving <- read.csv("3_givingdata.csv")                    # donation information, transactions per customer

# Can I spread this data so that one line per entityID
# and multiple columns -- 4 for each degree?
degree <- read.csv("4_Degree.csv")                        # degree information (school, year)


# cbind column of 1s, summarize on entity.ids (sum of 1s = count)
degree.2<-cbind(degree, d=rep(1, nrow(degree)))%>%
  group_by(Entity.ID)%>%
  summarise(count.degree=sum(d, na.rm=T), 
            year.min=min(Degree.Year, na.rm=T), 
            year.max=max(Degree.Year, na.rm=T),
            School=School.of.Graduation[1])

contact <- read.csv("5_ContactInformation.csv")           # address information
us.state <- read.csv("state_table.csv")
us.state <- us.state %>% select(name, abbreviation, census_region_name, census_division_name)
colnames(us.state)<- c("State.Full", "State", "Region", "SubRegion")

contact <- contact %>%
  filter(Preferred.Indicator == 1 & Entity.ID != "")

# relationship <- read.csv("6_Relationship_Type.csv")       # relationship type

part <- read.csv("7_ParticipationHistory.csv")            # participation in activities, including dates
part<-dcast(part, Entity.ID ~ Participation.Category)     # turns raw values into column names
part<-part%>%mutate(Part.Level=apply(part[,-1], 1, sum), Greek=FRTTY+SOROR)
# Part.Level includes all not just these four/five
part<-filter(part, Greek<4)%>%select(Entity.ID, SRVCE, ALUEV, CHAPT, REUN, FRTTY, SOROR, Greek, Part.Level)
# customer <- read.csv("15_CustomerSpecific_spur.csv")      # customer information (type of Alumni)

# appeals <- read.csv("16_Appeals.csv")                     # appeal codes and names // no need to merge

# contact_res <- read.csv("18_ContactRestrictions.csv")     # restriction information

# trans <- read.csv("19_CodeTranslationTable.csv")          # relationship status(married etc.), customer definitions // no need to merge

#Aggregate Sum given by year for each entity ID
giving$YearGiven <- substr(giving$Transaction.Date,7,10)
sum_given <- aggregate(Credit.Amount ~ Entity.ID+YearGiven, data=giving, sum)

#Create Blank Dataset with all possible years
entity.v <- rep(as.factor(unique(sum_given$Entity.ID)), each=35)
n<- length(unique(sum_given$Entity.ID))
years <- rep(seq(1982, 2016),n)
all_dates <- as.data.frame(entity.v)
all_dates$years <- years

#Now use match function to fill in the years where given entity code did not give money
sumrows <- match(interaction(as.character(all_dates$entity), all_dates$years) ,interaction(as.character(sum_given$Entity.ID), sum_given$YearGiven))
all_dates$Credit.Given <- sum_given[sumrows,"Credit.Amount"] #pulls Credit.Amount from the sum_given data set           
all_dates[is.na(all_dates$Credit.Given), "Credit.Given"]<-0
all_dates<-mutate(all_dates, gave=ifelse(Credit.Given!=0, 1, 0))
nrow(all_dates) # 6539365
# View(all_dates)

all_dates<-all_dates%>%filter(entity!="")
all_dates.2<-merge(all_dates, athletics,by.x="entity", by.y="Entity.ID", all.x=T)
all_dates.2<-merge(all_dates.2, degree.2,by.x="entity", by.y="Entity.ID", all.x=T)
all_dates.2<-merge(all_dates.2, entity, by.x="entity", by.y="Entity.ID", all.x=T)
all_dates.2<-merge(all_dates.2, contact, by.x="entity", by.y="Entity.ID", all.x=T)
all_dates.2<- merge(all_dates.2, us.state, by="State", all.x =T)

all_dates.2<-all_dates.2%>%mutate(GradToGive=years-year.min)%>%
  filter(years>1986, GradToGive>-1)

all_dates.2$School.Coll <- ifelse(all_dates.2$School == "BU", "Farmer",  
                                    ifelse(all_dates.2$School == "AP", "Engineering/Computing",
                                           ifelse(all_dates.2$School == "AS", "Arts & Science",
                                                  ifelse(all_dates.2$School == "EA", "Education/Health","Other")))) 

all_dates.2$School.Coll <- as.factor(all_dates.2$School.Coll)
all_dates.2<-select(all_dates.2, -Household.ID,-Preferred.Class.Year,
                    -X, -X.1, -X.2, -Address.Type, -Preferred.Indicator, 
                    -Primary.Gift.Officer.Assigned)
colnames(all_dates.2)<-c("State", "Entity", "Year", "Credit", "Gave", "Athlete", "Degree.Count",
                         "Degree.Year.1", "Degree.Year.2", "School", "Relationship.Status", "Primary.Relationship",
                         "Gender", "Married", "City", "Country",
                         "State.Full", "Region", "SubRegion", "School.Category", "GradToGive")
all_dates.2<-mutate(all_dates.2, Athlete=ifelse(is.na(Athlete), 0, Athlete))
all_dates.2$Athlete<-factor(all_dates.2$Athlete, levels=c(0,1), labels=c("Non-athlete", "Athlete"))
all_dates.2<-mutate(all_dates.2, Degree.Year.2=ifelse(Degree.Year.1==Degree.Year.2, NA, Degree.Year.2))

all_dates<-all_dates.2%>%arrange(Entity, Year)
years<-as.vector(all_dates$Year)
credit<-as.vector(all_dates$Credit)
GaveLastYear<-c(rep(NA, length(years)))
GaveLastYear.Amt<-c(rep(NA, length(years)))
length(credit)==length(years)

for( i in 2:length(years)){  
  if (years[i] > years[i-1] ){ 
    if (years[i] > years[i-1] & credit[i-1] > 0) {
        GaveLastYear[i]<-1
        GaveLastYear.Amt[i]<-credit[i-1]
      }
      else {
          GaveLastYear[i]<-0
          GaveLastYear.Amt[i]<-0
        }
    }
} 
all_dates<-cbind(all_dates, GaveLastYear, GaveLastYear.Amt)

Jones<-all_dates




years<-as.vector(Jones$Year)
credit<-as.vector(Jones$Credit)
LastGave<-c(rep(NA, length(years)))

last<-NA # Variable to index of last amount to give
for( i in 2:length(years)){  
  # If still with same entity
  if (years[i] > years[i-1] ){ 
    # Replace value of last with last credit given if > 0
    if (credit[i-1] > 0) {
      last<-credit[i-1]
    }
    # last gave at index i = last amount given
    LastGave[i]<-last
  }else{
    # If moved to different entity, reset last
      last<-NA
    }
} 

Jones<-cbind(Jones, LastGave)

Jones<-mutate(Jones, Degree.Category = ifelse(Primary.Relationship == "AG", "Graduate",
                           ifelse(Primary.Relationship == "AR", "Associate",
                                  ifelse(Primary.Relationship == "AU", "Undergraduate",
                                         "Other"))))
Jones.2<-merge(Jones, part, by.x="Entity", by.y="Entity.ID", all.x=T)
Jones.2<-Jones.2%>%mutate(SRVCE=ifelse(is.na(SRVCE), 0, SRVCE), 
                          ALUEV=ifelse(is.na(ALUEV), 0, ALUEV),
                          CHAPT=ifelse(is.na(CHAPT), 0, CHAPT),
                          REUN=ifelse(is.na(REUN), 0, REUN),
                          FRTTY=ifelse(is.na(FRTTY), 0, FRTTY),
                          SOROR=ifelse(is.na(SOROR), 0, SOROR),
                          Greek=ifelse(is.na(Greek), 0, Greek),
                          Part.Level=ifelse(is.na(Part.Level), 0, Part.Level))
Jones<-Jones.2

Jones<-Jones%>%
  mutate(Relationship.Status=as.character(Relationship.Status))%>%
  mutate(Relationship.Status=factor(Relationship.Status))

Jones<-Jones%>%
  mutate(Entity=as.character(Entity), State=as.character(State), 
         Athlete=as.character(Athlete), School=as.character(School), 
         Primary.Relationship=as.character(Primary.Relationship), 
         Gender=as.character(Gender), Married=as.character(Married),
         City = as.character(City), Country=as.character(Country),
         State.Full=as.character(State.Full), Region=as.character(Region),
         SubRegion=as.character(SubRegion), School.Category=as.character(School.Category),
         Degree.Category=as.character(Degree.Category))
# What to do with NA LastGave?
Jones<-Jones%>%
  mutate(Region=ifelse(is.na(Region), "Unknown", Region),
         GaveLastYear=ifelse(is.na(GaveLastYear), 0, GaveLastYear),
         GaveLastYear.Amt=ifelse(is.na(GaveLastYear.Amt), 0, GaveLastYear.Amt))%>%
  mutate(Entity=factor(Entity), State=factor(State), 
         Athlete=factor(Athlete), School=factor(School), 
         Primary.Relationship=factor(Primary.Relationship), 
         Gender=factor(Gender), Married=factor(Married),
         City = factor(City), Country=factor(Country),
         State.Full=factor(State.Full), Region=factor(Region),
         SubRegion=factor(SubRegion), School.Category=factor(School.Category),
         Degree.Category=factor(Degree.Category), 
         GaveLastYear=factor(GaveLastYear, levels=c(0,1), labels=c("No", "Yes")))

Jones<-Jones%>%mutate(LastGave=ifelse(is.na(LastGave), 0, LastGave))
Jones<-Jones%>%
  mutate(Region=as.character(Region))%>%
  mutate(Region=ifelse(Region=="Canada" | Region == "Miltary" | Region == "Military" | Region == "Unknown", "Other", Region))%>%
  mutate(Region=factor(Region))

Jones<-Jones%>%
  mutate(Degree.Category=as.character(Degree.Category))%>%
  mutate(Degree.Category=ifelse(Degree.Category=="Associate" | Degree.Category == "Other", "Other", Degree.Category))%>%
  mutate(Degree.Category=factor(Degree.Category))

Jones<-Jones%>%
  mutate(Gender=as.character(Gender))%>%
  filter(Gender!="N")%>%
  mutate(Gender=factor(Gender))

Jones<-Jones%>%filter(GradToGive<64)



#### Get cummulative proportion of giving -- Jones ####
# For each entity ID, we want to calculate 
#    the number of times they've given (a) and
#    the number of years since graduation (b)
# and then divide a/b to get propGave
# We're counting first year as a year they could give
# so add 1 to gradtogive to get denometer
temp<-Jones%>%filter(Degree.Year.1>1986)%>%arrange(Entity, Year)
# starting entityID
curEnt<-temp[1, "Entity"]
prop<-0
times.given<-0
for(i in 1:(nrow(temp)-1)){
  ent<-temp[i,"Entity"] # current entity
  temp[i, "propGave"]<-prop # Set prop to prop given 
  
  # if current entity is diff from last current entity, 
  # update curEnt and set times.given to 0
  if(ent!=curEnt){
    curEnt<-ent
    times.given<-0 # Replace with 0 if it's actually a new person
    temp[i, "propGave"]<-0
  }
  times.given<-times.given+temp[i, "Gave"]
  prop<-times.given/(temp[i, "GradToGive"]+1)
}
Jones<-temp
save(Jones, file="Jones.RData")
write.csv(Jones, file="Jones.csv")