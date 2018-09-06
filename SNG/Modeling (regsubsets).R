# Jones Project

#### libraries ####
library(dplyr)
library(tidyr)
library(leaps)

#### read in data ####
setwd("/Users/wangyuexi/Desktop/Yuexi/data")
load("Jones.RData") # complete dataset

#sum data by entity ID (0 assign to NA)
JonesNA <- Jones
JonesNA$Credit[JonesNA$Credit == 0] <- NA

# create year.1st variable
year.1st <- Jones %>%
  filter(Gave == 1) %>%
  arrange(Entity, GradToGive) 

year.1st <- year.1st %>%
  group_by(Entity) %>%
  summarize(Year.1st.Give = min(GradToGive))


# summarise dataset 
# Due to violation we now try a transformation on Y and have to add 1 to total credit so that log works
giving.sum <- JonesNA %>%
  filter(Credit != 0) %>%
  group_by(Entity) %>%
  summarize(SumCredit = sum(Credit, na.rm = TRUE), Married = Married[1],
            Gender = Gender[1], School = School.Category[1] , Degree.Count = Degree.Count[1], 
            Degree.Year.1 = Degree.Year.1[1],  Athlete = Athlete[1], DegreeCat = Degree.Category[1],
            Relationship.Status = Relationship.Status[1], Region = Region[1],
            Service = SRVCE[1], AlumEvent = ALUEV[1],
            ChapEvent = CHAPT[1], Reunion = REUN[1], Greek = Greek[1], PartLevel = Part.Level[1]) %>%
  mutate(logSum = log(SumCredit))

giving.sum <- merge(giving.sum, year.1st, by="Entity", all.x = TRUE) %>%
              select(-Entity, -SumCredit)

# set random seed and create training and test datasets.
set.seed(769732)
train=sample(c(TRUE,FALSE), nrow(giving.sum),rep=TRUE)
test=(!train)

head(giving.sum[train,])
#variable selection
#nvmax: maximun size of subets to examine
regfit.best <- regsubsets(logSum~.^2, data=top10.year_clean[train,], nvmax =150, method = "forward")

#model matrix creates a design (or model) matrix, by expanding factors to a set of summy variables 
#and expanding interactions similarly.
test.mat=model.matrix(logSum ~ .^2,data=top10.year_clean[test,])

val.errors=rep(NA,150)
exp.val.errors=rep(NA,150)

ptm<-proc.time()
for(i in 1:150){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((top10.year_clean$logSum[test]-pred)^2) 
  exp.val.errors[i]=mean((exp(top10.year_clean$logSum[test])-exp(pred))^2) 
}
proc.time()-ptm
## 9 seconds. 

# plots validation errors to correct model can be chosen
val.errors
plot(val.errors[1:150])
plot(val.errors ,type='b', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     ylab ="RMSE", xlab = "Model Size")

var(giving.sum$logSum)
coef(regfit.best,40)

# create test ds and merge with predicted values to use for big donor inference
test.ds <- giving.sum[test,]


regfit.best <- regsubsets(logSum~.^2, data=giving.sum[train,], nvmax =150, method = "forward")
coef(regfit.best, 40)
test.mat=model.matrix(logSum ~ .^2,data=giving.sum[test,])

val.errors=rep(NA,40)
exp.val.errors=rep(NA,40)


for(i in 1:40){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((giving.sum$logSum[test]-pred)^2) 
  exp.val.errors[i]=mean((exp(giving.sum$logSum[test])-exp(pred))^2) 
}

colnames(pred) <- "yhat"
test.pred <- cbind(test.ds, pred)
test.pred$SumCredit <- exp(test.pred$logSum)
test.pred$SumCreditHat <- exp(test.pred$yhat)
test.pred$Diff <- test.pred$SumCredit - test.pred$SumCreditHat
test.pred$MultDiff <- test.pred$SumCredit/test.pred$SumCreditHat
# mean of multiplicitve difference
mean(test.pred$MultDiff)

library(psych)
# geometric mean of multiplicitve difference
exp(mean(log(abs(test.pred$MultDiff))))
geometric.mean(test.pred$MultDiff)

mean(abs(test.pred$Diff))
median(abs(test.pred$Diff))

# apply to full dataset - Note: not needed anymore as we are predicting using test DS above.

# ###########################################################
 regfit.fwd = regsubsets(logSum ~ .^2, data = giving.sum, nvmax = 40, method = "forward")
  coef(regfit.fwd, 40)
#############################################################
  
 # val.errors2 = rep(NA, 40)
 # x.test2 = model.matrix(logSum ~ .^2, data = giving.sum)  # notice the -index!
 # for (i in 1:40) {
   #  coefi2 = coef(regfit.fwd, id = i)
   #  pred2 = x.test2[, names(coefi2)] %*% coefi2
  #   val.errors2[i] = mean((giving.sum$logSum - pred2)^2)
   # }
# 
 # colnames(pred2) <- "yhat"
# new <- cbind(giving.sum, pred2)

# looking at big donors using test dataset 
big.donors <- test.pred %>% filter(pred > log(2000)) # donors greater than $2,000
table(big.donors$School) #farmer make up majority 65.85%
table(big.donors$Region) # midwest make up majority 70.12
table(big.donors$Athlete) # 27.4% athletes
table(big.donors$Greek) # roughly 52.43% Greek
table(big.donors$Gender) # 75.86% male
table(big.donors$Married) # 71.34% married
table(big.donors$Degree.Year.1)


hist(big.donors$Service) # uniform
hist(big.donors$AlumEvent) # right skewed
hist(big.donors$PartLevel) # uniform

# looking at small donors
small.donors <- test.pred %>% filter(pred < 3.912023) # donors less than $50
table(small.donors$School)  # arts & science and education make up majority 62.7%
table(small.donors$Region) # midwest make up majority 71.69
table(small.donors$Athlete) # 2.57% athletes
table(small.donors$Greek) # roughly 29.47% Greek
table(small.donors$Gender) # 64.24% female
table(small.donors$Married) # 0.3% married
table(small.donors$Degree.Year.1)

hist(small.donors$Service) # no service
hist(small.donors$AlumEvent) # no alum events
hist(small.donors$PartLevel) # no participation if little





# 1st attempt to get regsubsets to get predicted values 

# # run regsubsets on best selective variables.
# # best 50 variable model on whole dataset

#########################################################
 finalMod <- regsubsets(logSum~.^2, data=giving.sum, nvmax = 40, nbest =1, method = "forward")
 finMod <- summary(finalMod)
 coef(finalMod ,40)
##########################################################
# 
 outMat <- finMod$outmat
dim(outMat)
  outMat[41,]
outMat <- cbind(rep(1, dim(outMat)[1]), outMat)
# ##########################################################
 giv.sum <- model.matrix(logSum~.^2, data=giving.sum)

 opt.giv.sum <- giv.sum[outMat[41,]==1]
# # ##########################################################
# # predict regsubsets output
 predict.regsubsets = function(object, newdata, id, ...){
   temp_X <- cbind(rep(1, length(newdata[,1])), newdata)
   colnames(temp_X) <- c("(Intercept)", colnames(newdata)) 
   coefi = coef(object, id=i)
   my_pred = as.matrix(temp_X[ ,names(coefi)])%*%coefi
 
   return(my_pred)
 }
 
########################################

 predict.regsubsets(object = finalMod, newdata = giving.sum, id=2)
 ## my code ################### might work right ~~~~
 temp_X <- cbind(rep(1, length(giving.sum[,1])), giv.sum)
 colnames(temp_X) <- c("(Intercept)", colnames(giv.sum)) 
 coefi = coef(finalMod, id=2)
 my_pred = as.matrix(temp_X[ ,c("(Intercept)","Degree.Year.1","Married1:Year.1st.Give")])%*%coefi
 
 ########################################
 
 ds <- cbind(giving.sum[,"logSum"],giv.sum)
 ds<-data.table(ds)
 ds <- as.data.frame(ds) %>%
       select(V1, GenderM, Degree.Year.1, `AthleteNon-athlete`,Service, PartLevel,
              Married1:GenderM, Married1:Degree.Year.1, Married1:Year.1st.Give,
              GenderM:`SchoolEducation/Health`, GenderM:`AthleteNon-athlete`, GenderM:AlumEvent,
              GenderM:Year.1st.Give, `SchoolEducation/Health`:Degree.Count, SchoolOther:Degree.Count,
             SchoolFarmer:`AthleteNon-athlete`, SchoolFarmer:DegreeCatUndergraduate, `SchoolEducation/Health`:Relationship.Statusliving,
              SchoolOther:Relationship.Statusliving, `SchoolEducation/Health`:AlumEvent,SchoolFarmer:PartLevel,
              SchoolOther:Year.1st.Give,Degree.Count:Degree.Year.1,Degree.Year.1:DegreeCatUndergraduate,
              Degree.Year.1:Reunion, `AthleteNon-athlete`:DegreeCatOther, `AthleteNon-athlete`:RegionNortheast,
              `AthleteNon-athlete`:RegionWest, `AthleteNon-athlete`:Greek, DegreeCatUndergraduate:Relationship.Statusliving,
              DegreeCatUndergraduate:RegionNortheast, DegreeCatOther:RegionOther,Relationship.Statusliving:Service,
              Relationship.Statusliving:Year.1st.Give, RegionWest:Service, Reunion:Year.1st.Give, Greek:PartLevel,
              Married1:Relationship.Statusliving, `AthleteNon-athlete`:ChapEvent)

fit1 <- lm(V1 ~ .,
             data=ds)
summary(fit1)
coef(fit1)
# 
# giving.sum$yhat <- predict(fit1, giving.sum, type = "response")


############################ model 3 ###########################
modFinal<-glm(formula = Gave ~ GradToGive + GaveLastYear + Gender + Athlete +
      Relationship.Status + Married + Region + School.Category +
      LastGave + Degree.Category + SRVCE + ALUEV + CHAPT + REUN +
      Greek + Part.Level + GradToGive.2 + propGave * (GradToGive +
                                                        GaveLastYear + Gender + Athlete + Relationship.Status + Married +
                                                        Region + Degree.Category + SRVCE + ALUEV + CHAPT + Greek +
                                                        Part.Level + GradToGive.2) + GradToGive * (GaveLastYear +
                                                                                                     Gender + Athlete + Relationship.Status + Region + School.Category +
                                                                                                     LastGave + Degree.Category + SRVCE + REUN + Greek + Part.Level +
                                                                                                     GradToGive.2) + GaveLastYear * (Gender + Athlete + +Region +
                                                                                                                                       School.Category + ALUEV + CHAPT + Greek + Part.Level + GradToGive.2),
    family = "binomial", data = Jones)

summary(modFinal)



