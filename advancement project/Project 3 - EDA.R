## Project 3
## Client: Jones

#### libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(maps)
library(mapproj)
library(stringi)
library(vcd)
library(reshape2)
library(rpart)
library(gridExtra)

#### read in data ####
setwd("/Users/wangyuexi/github/leverage subsampling/leverage subsampling/advancement project")
load("Jones.RData") # complete dataset

# temp<-Jones%>%filter(Gave==1)
# nrow(distinct(temp, Entity))==nrow(distinct(Jones, Entity)) # FALSE: 83331 != 93831
# # We have entities in our data set that never gave... 
# # Is this because we filtered out years before 1967 but not graduation date?
# # Should we remove these people...?
# temp<-temp%>%group_by(Entity)%>%summarize(firstYearGave=min(Year))
# 
# Jones<-merge(Jones, temp, by="Entity", all.x=TRUE)
# nrow(Jones) #2357603 -- so we didn't lose anyone in the process, 
# #           but there will be some people with NAs
# #           for firstYearGave


# puts degrees years into decades
Jones$Decade <- ifelse(Jones$Degree.Year.1 >= 1950 & Jones$Degree.Year.1 < 1960, "'50s",
                               ifelse(Jones$Degree.Year.1 >= 1960 & Jones$Degree.Year.1 < 1970, "'60s",
                                      ifelse(Jones$Degree.Year.1 >= 1970 & Jones$Degree.Year.1 < 1980, "'70s",
                                             ifelse(Jones$Degree.Year.1 >= 1980 & Jones$Degree.Year.1 < 1990, "'80s",
                                                    ifelse(Jones$Degree.Year.1 >= 1990 & Jones$Degree.Year.1 < 2000, "'90s",
                                                           ifelse(Jones$Degree.Year.1 >= 2000 & Jones$Degree.Year.1 < 2010, "'00s",
                                                                  ifelse(Jones$Degree.Year.1 >= 2010 & Jones$Degree.Year.1 < 2020, "'10s",
                                                                         "'40s"))))))) 

# Bin years
Jones$binPeriod <- ifelse(Jones$GradToGive <= 5, "1 - 5 Years",
                          ifelse(Jones$GradToGive  > 5 & Jones$GradToGive  <= 10, "6 - 10 Years",
                                 ifelse(Jones$GradToGive  > 10 & Jones$GradToGive  <= 15, "11 - 15 Years",
                                        ifelse(Jones$GradToGive  > 15 & Jones$GradToGive  <= 20, "16 - 20 Years",
                                               ifelse(Jones$GradToGive  > 20 & Jones$GradToGive  <= 25, "21 - 25 Years",
                                                      ifelse(Jones$GradToGive  > 25 & Jones$GradToGive  <= 30, "26 - 30 Years",
                                                             "30 + Years)"))))))


# filters out observations with 0 credit given.
nozero <- filter(Jones, Credit != 0)

giving.sum <- nozero %>%
              group_by(Entity) %>%
              summarize(Transactions=n(), SumCredit = sum(Credit, na.rm = TRUE),
                        MeanCredit = mean(Credit, na.rm = TRUE), MaxCredit = max(Credit,na.rm =TRUE),
                        MinCredit = min(Credit, na.rm = TRUE), MedianCredit = median(Credit, na.rm = TRUE),
                        Gender = Gender[1], School = School.Category[1] , Degree.Count = Degree.Count[1], Married = Married[1],
                        Degree.Year.1 = Degree.Year.1[1],  Athlete = Athlete[1], DegreeCat = Degree.Category[1],
                        Relationship.Status = Relationship.Status[1], State = State[1], Region = Region[1],
                        Decade = Decade[1],  GradToGive = GradToGive[1], Service = SRVCE[1], AlumEvent = ALUEV[1],
                        ChapEvent = CHAPT[1], Reunion = REUN[1], Greek = Greek[1], PartLevel = Part.Level[1])

giving.sum$Bins.sum <- ifelse(giving.sum$SumCredit < 100, "< $100", 
                              ifelse(giving.sum$SumCredit >= 100 & giving.sum$SumCredit < 1000, "$100 < Giv < $1K",
                                     ifelse(giving.sum$SumCredit >= 1000 & giving.sum$SumCredit < 10000, "$1K < Giv < $10K",
                                            ifelse(giving.sum$SumCredit >= 10000 & giving.sum$SumCredit < 100000, "$10K < Giv < $100K",
                                                   ifelse(giving.sum$SumCredit >= 100000 & giving.sum$SumCredit < 1000000, "$100K < Giv < $1MIL",
                                                          "> $1MIL")))))

giving.sum$Bins.med <- ifelse(giving.sum$MedianCredit < 50, "< $50", 
                              ifelse(giving.sum$MedianCredit >= 50 & giving.sum$MedianCredit < 100, "$50 < Giv < $100",
                                     ifelse(giving.sum$MedianCredit >= 100 & giving.sum$MedianCredit < 500, "$100 < Giv < $500",
                                            ifelse(giving.sum$MedianCredit >= 500 & giving.sum$MedianCredit < 1000, "$500 < Giv < $1K",
                                                   ifelse(giving.sum$MedianCredit >= 1000 & giving.sum$MedianCredit < 10000, "$1K < Giv < $10K", 
                                                          "$10K +")))))

giving.sum$Bins.med <- factor(giving.sum$Bins.med, levels=c("< $50", "$50 < Giv < $100", "$100 < Giv < $500",
                                                              "$500 < Giv < $1K", "$1K < Giv < $10K", "$10K +"))


# Create table for regions and binned median amounts
reg.sum.tab <- table(giving.sum$Region, giving.sum$Bins.med)

reg.sum.tab.df <- as.data.frame(table(giving.sum$Region, giving.sum$Bins.med))
prop.regSum <- reg.sum.tab.df %>%
  group_by(Var1) %>%
  mutate(classCount = sum(Freq),
         prop = 100*Freq/classCount)

# orders the factor levels correctly
prop.regSum$Var2 <- factor(prop.regSum$Var2, levels=c("< $50", "$50 < Giv < $100", "$100 < Giv < $500",
                                                              "$500 < Giv < $1K", "$1K < Giv < $10K", "$10K +"))

ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill= Freq), data = reg.sum.tab.df) +
  scale_fill_gradient(low = "white", high = "forestgreen")

ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill= prop), data = prop.regSum) +
  scale_fill_gradient(low = "floralwhite", high = "forestgreen", "%") +
  xlab("Region") +
  ylab("Median of Total Donations")

# boxplot for region
ggplot() +
  geom_boxplot(aes(x = Region, y = log(MedianCredit), group = Region), data = giving.sum)

#### Plots for Sumcredit ####
# histogram
plot1 <- ggplot() +
  geom_histogram(aes(x=log(SumCredit)), data=giving.sum) +
  xlab("log(total amount given)") +
  ylab("")

plot2 <- ggplot() +
  geom_histogram(aes(x=SumCredit), data=giving.sum) +
  xlab("total amount given") +
  ylab("Frequency")

# density
plot3 <- ggplot() +
  geom_density(aes(x=log(SumCredit)), data=giving.sum) +
  xlab("log(total amount given)") +
  ylab("")

plot4 <- ggplot() +
  geom_density(aes(x=SumCredit), data=giving.sum) +
  xlab("total amount given") +
  ylab("Frequency")

#boxplot
plot5 <- qplot(y=SumCredit, x=1, geom = "boxplot", data=giving.sum) + 
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) 
  
plot6 <- qplot(y=log(SumCredit), x=1, geom = "boxplot", data=giving.sum) +
  coord_flip()  +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
  

# creates a 1x2 arrangement of plots
grid.arrange(plot5, plot6, ncol=2)

# bar chart for sum binned amounts
giv.bins <- as.data.frame(table(giving.sum$Bins.sum))

giv.bins$Var1 <- factor(giv.bins$Var1, levels=c("< $100", "$100 < Giv < $1K", "$1K < Giv < $10K",
                                                      "$10K < Giv < $100K", "$100K < Giv < $1MIL", "> $1MIL"))

ggplot() +
  geom_bar(aes(x=Var1,y=Freq), stat="identity", data=giv.bins) +
  ylab("Number of Donators") +
  xlab("Total Donations") +
  theme_bw()

####################################################################
##################    1st Year of Donation   #######################
####################################################################

year.1st <- Jones %>%
  filter(Gave == 1) %>%
  arrange(Entity, GradToGive) 

year.1st <- year.1st %>%
  group_by(Entity) %>%
  summarize(Year.1st.Give = min(GradToGive), SumCredit = sum(Credit), N = n(),
            AvgCredit = mean(Credit), MedCredit = median(Credit),
            School = School.Category[1], Degree.Year.1 = Degree.Year.1[1], binPeriod = binPeriod[1],
            Athlete = Athlete[1], Region = Region[1], Decade = Decade[1])

year.1st$Athlete <- as.factor(year.1st$Athlete)
year.1st$Decade <- as.factor(year.1st$Decade)
year.1st$binPeriod <- as.factor(year.1st$binPeriod)


# summarize on athlete and first year of giving.
year.1st.ath <- year.1st %>%
  group_by(Year.1st.Give, Athlete) %>%
  summarize(SumCreditSum = sum(SumCredit), N = n(),
            AvgCreditSum = mean(SumCredit), MedCreditSum = median(SumCredit),
            SumCreditAvg = sum(AvgCredit), 
            AvgCreditAvg = mean(AvgCredit), MedCreditAvg = median(AvgCredit),
            SumCreditMed = sum(MedCredit), 
            AvgCreditMed = mean(MedCredit), MedCreditMed = median(MedCredit))

# summarize on decade and first year of giving.
year.1st.dec <- year.1st %>%
  group_by(Year.1st.Give, Decade) %>%
  summarize(SumCreditSum = sum(SumCredit), N = n(),
            AvgCreditSum = mean(SumCredit), MedCreditSum = median(SumCredit),
            SumCreditAvg = sum(AvgCredit), 
            AvgCreditAvg = mean(AvgCredit), MedCreditAvg = median(AvgCredit),
            SumCreditMed = sum(MedCredit), 
            AvgCreditMed = mean(MedCredit), MedCreditMed = median(MedCredit))                      

# average total donations for 1st years of giving for entities total donations
ggplot() +
  geom_point(aes(x = Year.1st.Give, y = AvgCreditSum, color = Athlete), data = year.1st.ath) +
  xlab("Years after Graduation that they First Donated") +
  scale_x_continuous(breaks = seq(0,30,5)) +
  scale_color_discrete(limits = c("Athlete","Non-athlete"),
                       labels = c("Athlete","Non-Athlete")) +
  ylab("Average of Total Donations by Entity") +
  theme_bw() +
  theme(legend.title=element_blank())
  

# median total donations for 1st years of giving for entities total donations
med_dot <- ggplot() +
  geom_point(aes(x = Year.1st.Give, y = MedCreditSum, color = Athlete), data = year.1st.ath) +
  xlab("Years after Graduation that they First Donated") +
  scale_x_continuous(breaks = seq(0,40,5)) +
  scale_color_discrete(limits = c("Athlete","Non-athlete"),
                       labels = c("Athlete","Non-Athlete")) +
  ylab("Median of Total Donations by Entity ($)") +
  theme_bw() +
  theme(legend.title=element_blank())

# median total donations for 1st years of giving for entities total donations - decade
med_dot <- ggplot() +
  geom_point(aes(x = Year.1st.Give, y = MedCreditSum, color = Decade), data = year.1st.dec) +
  xlab("Years after Graduation that they First Donated") +
  scale_x_continuous(breaks = seq(0,40,5)) +
  scale_color_discrete(limits = c("'80s","'90s","'00s","'10s")) +
  ylab("Median of Total Donations by Entity ($)") +
  theme_bw()

# looks at year 1st given by year 1st given.
year.1st.sum <- year.1st %>%
                group_by(Year.1st.Give) %>%
                summarize(N = n(), SumCreditSum = sum(SumCredit),
                          AvgCreditSum = mean(SumCredit), MedCreditSum = median(SumCredit),
                          SumCreditAvg = sum(AvgCredit), 
                          AvgCreditAvg = mean(AvgCredit), MedCreditAvg = median(AvgCredit),
                          SumCreditMed = sum(MedCredit), 
                          AvgCreditMed = mean(MedCredit), MedCreditMed = median(MedCredit))    


####################################################################
######################    ATHLETICS   ##############################
####################################################################

# table of GradToGives split by athlete status
ath_time <- nozero %>%
  group_by(GradToGive, Athlete) %>%
  summarize(n = n(), Sum = sum(Credit, na.rm = TRUE), 
            Mean = mean(Credit, na.rm = TRUE),  
            Median = median(Credit, na.rm = TRUE))

# time series plot for the mean
ggplot() +
  geom_line(aes(x = GradToGive, y = Mean/1000, color = Athlete), data = ath_time) +
  scale_color_discrete(limits = c("Athlete","Non-athlete"),
                       labels = c("Athlete","Non-Athlete")) +
  ylab("Average Donations (1,000's)") +
  xlab("Years Since Graduating") +
  theme_bw() +
  theme(legend.title=element_blank())

# time series plot for the median
yg_med<-ggplot() +
  geom_line(aes(x = GradToGive, y = Median, color = Athlete), data = ath_time) +
  scale_color_discrete(limits = c("Athlete","Non-athlete"),
                       labels = c("Athlete","Non-Athlete")) +
  ylab("Median Donations") +
  xlab("Years Since Graduating") +
  theme_bw() +
  theme(legend.title=element_blank())


# create table that has statistics for athlete status

giv.ath.table2 <- as.data.frame(table(giving.sum$Athlete, giving.sum$Bins.sum))
ath.tot <- sum(filter(giv.ath.table2, Var1 == "Athlete")$Freq)
nonath.tot <- sum(filter(giv.ath.table2, Var1 == "Non-athlete")$Freq)
giv.ath.table2$con.Freq <- ifelse(giv.ath.table2$Var1 =="Athlete", round(giv.ath.table2$Freq/ath.tot,4), 
                                 round(giv.ath.table2$Freq/nonath.tot,4))

giv.ath.table2$Var2 <- factor(giv.ath.table2$Var2, levels=c("< $100", "$100 < Giv < $1K", "$1K < Giv < $10K",
                                                      "$10K < Giv < $100K", "$100K < Giv < $1MIL", "> $1MIL"))

# histogram of athletes vs. non athletes
# the data in this histogram is first summed up by entity and then conditional frequencies are looked at for 
cf <- ggplot(aes(x=Var2, y=con.Freq, fill=Var1), data=giv.ath.table2)+
  geom_bar(aes(fill=Var1), stat="identity", position="dodge") +
  scale_fill_discrete(limits = c("Athlete","Non-athlete"),
                       labels = c("Athlete","Non-Athlete")) +
  xlab("$$$") +
  ylab("Conditional Frequency") +
  theme_bw() +
  geom_text(aes(label= paste(format(100*con.Freq, digits=2, drop0trailing=TRUE),"%",sep =""), y= con.Freq ), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(labels=percent) +
  theme(axis.text.y=element_blank(),
        legend.title=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position=c(.9, .85)) 

####################################################################
######################     School     ##############################
####################################################################
deg_time <- nozero %>%
  group_by(GradToGive, School.Category) %>%
  summarize(n = n(), Sum = sum(Credit, na.rm = TRUE), 
            Mean = mean(Credit, na.rm = TRUE),
            Median = median(Credit, na.rm = TRUE))

colnames(deg_time)[2] <- "School"

# time series plot sum
ggplot() +
  geom_line(aes(x = GradToGive, y = Sum/1000000, color = School), data = deg_time) +
  ylab("Total Donations (1,000,000's)") +
  xlab("Years Since Graduating") +
  theme_bw()

# time series plot Mean
ggplot() +
  geom_line(aes(x = GradToGive, y = Mean, color = School), data = deg_time) +
  ylab("Average Donations") +
  xlab("Years Since Graduating") +
  theme_bw()

# time series plot Median
med_school <- ggplot() +
  geom_line(aes(x = GradToGive, y = Median, color = School), data = deg_time) +
  ylab("Median Donations ($)") +
  xlab("Years Since Graduating") +
  theme_bw()


# school of graduation bar chart
giv.school <- giving.sum %>% 
  group_by(School) %>%
  summarize(n = n(), sum = sum(SumCredit), mean = mean(SumCredit), 
            max = max(SumCredit), min = min(SumCredit)) 

colnames(giv.school) <- c("School","N", "Sum", "Mean", "Max", "Min")

# summarize by School of Graduation
ggplot() +
  geom_bar(aes(x=School, y = Sum/1000000),stat="identity", data = giv.school) +
  ylab("Total Donations (1,000,000's)") +
  xlab("") +
  theme_bw()


####################################################################
#################     RELATIONSHIP TYPE     ########################
####################################################################
rel_time <- nozero %>%
  group_by(GradToGive, Degree.Category) %>%
  summarize(n = n(), Sum = sum(Credit, na.rm = TRUE), 
            Mean = mean(Credit, na.rm = TRUE), Median = median(Credit, na.rm = TRUE))

# time series plot mean
ggplot() +
  geom_line(aes(x = GradToGive, y = Mean/1000, color = Degree.Category), data = rel_time) +
  ylab("Average Donations (1,000's)") +
  xlab("Years Since Graduating") +
  theme_bw() +
  scale_color_discrete("Degree Type")

# time series plot median
ggplot() +
  geom_line(aes(x = GradToGive, y = Median, color = Degree.Category), data = rel_time) +
  ylab("Median Donations (1,000's)") +
  xlab("Years Since Graduating") +
  theme_bw()
  


####################################################################
#####################     DONATION MAPS     ########################
####################################################################
# summed by region
giv.region <- giving.sum %>% 
  group_by(Region) %>%
  summarize(n = n(), sum(SumCredit), mean(SumCredit), 
            max(SumCredit), min(SumCredit), median(SumCredit))

# might need to remove duplicates in conneticut and potentially other states. Two
# largest donors for this states seem to have been duplicated.

# summarize by state
giv.state <- giving.sum %>% 
  group_by(State) %>%
  summarize(n = n(), sum(SumCredit), mean(SumCredit), 
            max(SumCredit), min(SumCredit), median(SumCredit))

colnames(giv.state) <- c("State","N", "Sum", "Mean", "Max", "Min", "Median")
states.outlines <- map_data("state")
head(states.outlines)
states.outlines <- states.outlines %>% 
  mutate(state = state.abb[match(stri_trans_totitle(region),state.name)])

# now we need to merge this states data with the outline data so that it can be used to build a plot layer
states.all <- merge(states.outlines, giv.state, by.x="state", by.y="State")
head(states.all)

# plot by connecting the dots into shapes
ggplot() + 	# plot without a default data set
  geom_path(data=states.outlines, aes(x=long, y=lat, group=group, order=order)) 

#average donations
avg.map <- ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=Mean, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("$", low="white", high="forestgreen") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Average Donations by State")


# sum donations
ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=Sum/1000000, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("$$$", low="white", high="forestgreen") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Total Donations ($Millions) by State")

# number of donators
ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=N, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("Donators",low = "white", high="red4") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Donators by State")

# median amount
med.map <- ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=Median, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("$",low = "white", high="forestgreen") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Median Donations ($'s) by State")


# age map
age.state <- giving.sum %>% 
  group_by(State) %>%
  summarize(n = n(), sumYear = sum(Degree.Year.1), meanYear = mean(Degree.Year.1), 
            max(Degree.Year.1), min(Degree.Year.1), medYear = median(Degree.Year.1))

colnames(age.state) <- c("State","N", "sumYear", "meanYear", "Max", "Min", "medYear")

# now we need to merge this states data with the outline data so that it can be used to build a plot layer
states.age <- merge(states.outlines, age.state, by.x="state", by.y="State")
head(states.age)

#average age
ggplot() + 
  geom_polygon(data=states.age, aes(x=long, y=lat,
                                    fill=meanYear, group=group, order=order)) + 
  geom_path(data=states.age, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("$", low="white", high="steelblue") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Age of Donators by State")

#average age
ggplot() + 
  geom_polygon(data=states.age, aes(x=long, y=lat,
                                    fill=medYear, group=group, order=order)) + 
  geom_path(data=states.age, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("$", low="white", high="steelblue") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Age of Donators by State")



####################################################################
######################     Greek     ###############################
####################################################################

nozero$GreekLife <- as.factor(ifelse(nozero$Greek > 0, "Greek", "Non-Greek"))

greek_time <- nozero %>%
  group_by(GradToGive, GreekLife) %>%
  summarize(n = n(), Sum = sum(Credit, na.rm = TRUE), 
            Mean = mean(Credit, na.rm = TRUE),
            Median = median(Credit, na.rm = TRUE))

colnames(greek_time)[2] <- "Status"

# time series plot sum
ggplot() +
  geom_line(aes(x = GradToGive, y = Sum/1000000, color = Status), data = greek_time) +
  ylab("Total Donations (1,000,000's)") +
  xlab("Years Since Graduating") +
  theme_bw()

# time series plot Mean
ggplot() +
  geom_line(aes(x = GradToGive, y = Mean, color = Status), data = greek_time) +
  ylab("Average Donations") +
  xlab("Years Since Graduating") +
  scale_colour_manual(values = c("Greek" = "darkolivegreen","Non-Greek" = "gold")) +
  theme_bw()

# time series plot Median
med_greek <- ggplot() +
  geom_line(aes(x = GradToGive, y = Median, color = Status), data = greek_time) +
  ylab("Median Donations ($)") +
  xlab("Years Since Graduating") +
  scale_colour_manual(values = c("Greek" = "forestgreen","Non-Greek" = "gold")) +
  theme_bw()

####################################################################
######################     Married     ###############################
####################################################################

nozero$Mar <- as.factor(ifelse(nozero$Married == "0", "Not Married", "Married"))

mar_time <- nozero %>%
  group_by(GradToGive, Mar) %>%
  summarize(n = n(), Sum = sum(Credit, na.rm = TRUE), 
            Mean = mean(Credit, na.rm = TRUE),
            Median = median(Credit, na.rm = TRUE))

colnames(mar_time)[2] <- "Status"

# time series plot sum
ggplot() +
  geom_line(aes(x = GradToGive, y = Sum/1000000, color = Status), data = mar_time) +
  ylab("Total Donations (1,000,000's)") +
  xlab("Years Since Graduating") +
  theme_bw()


# time series plot Mean
ggplot() +
  geom_line(aes(x = GradToGive, y = Mean, color = Status), data = mar_time) +
  ylab("Average Donations") +
  xlab("Years Since Graduating") +
  # scale_colour_manual(values = c("Greek" = "darkolivegreen","Non-Greek" = "gold")) +
  theme_bw()


# time series plot Median
med_marriage <- ggplot() +
  geom_line(aes(x = GradToGive, y = Median, color = Status), data = mar_time) +
  ylab("Median Donations ($)") +
  xlab("Years Since Graduating") +
  scale_colour_manual(values = c("Not Married" = "gray", "Married" = "hotpink4")) +
  theme_bw()





