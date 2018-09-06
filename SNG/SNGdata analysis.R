setwd("/Users/wangyuexi/Desktop/Yuexi/data")
SNGdata<-read.csv("SNGdata.csv")
SNGdata1987<-SNGdata%>%filter(year >= 1987)
SNGdata$logSum<-log(SNGdata$Amount)
df<-SNGdata[, -c(1, 2, 4, 5, 8, 12, 13,14, 15:18)]
df<-df[which(df$logSum!="-Inf"),]
## delete state with NA
df<-df[which(df$Gender!="N"), ]

## seperate data as train and test, which train has 75%
smp_size <- floor(0.75 * nrow(df))
set.seed(12345)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

##################### simple linear regression and select variable by AIC
prev<-Sys.time()
mlr <- lm(logSum ~ .,data = train)
Sys.time()-prev

prev<-Sys.time()
stepBackward <- step(mlr)
Sys.time()-prev

slr<-lm(logSum ~ (Transaction.year + School + year + Gender + Married.Alumni.Indicator + 
                   athlete + FRTTY + SOROR + Greek + Part.Level+GradToGive+lastGave+GaveLastYear), data=train)
test$pred<-predict(slr, test)
testMSE<-mean((test$logSum - test$pred)^2)
## testMSE=1.49474

########################## model with 3 interaction terms
mlr1<-lm(logSum ~ (Transaction.year + School + year + Gender + Married.Alumni.Indicator + 
                   athlete + FRTTY + SOROR + Greek + Part.Level + GaveLastYear*School + 
                    GradToGive*athlete + year*School), data=train)

test$pred<-predict(mlr1, test)
testMSE<-mean((test$logSum - test$pred)^2)
#### testMSE=1.49221

############################ model with 6 interaction terms
mlr2<-lm(logSum ~ (Transaction.year + School + year + Gender + Married.Alumni.Indicator + 
                    athlete +FRTTY + SOROR + Greek + Part.Level +GaveLastYear*School + 
                     GradToGive*athlete + year*School + Gender*Married.Alumni.Indicator + 
                    Married.Alumni.Indicator*School+GaveLastYear*athlete ), data=train)

test$pred<-predict(mlr2, test)
testMSE<-mean((test$logSum - test$pred)^2)
#### testMSE=1.489697

###### cross validation on the three models
# K-fold Cross validation

# use this function to add K grouping indeces
add_cv_cohorts <- function(dat,cv_K){
  if(nrow(dat) %% cv_K == 0){ # if perfectly divisible
    dat$cv_cohort <- sample(rep(1:cv_K, each=(nrow(dat)%/%cv_K)))
  } else { # if not perfectly divisible
    dat$cv_cohort <- sample(c(rep(1:(nrow(dat) %% cv_K), each=(nrow(dat)%/%cv_K + 1)),
                              rep((nrow(dat) %% cv_K + 1):cv_K,each=(nrow(dat)%/%cv_K)) ) )
  }
  return(dat)
}

df_cv <- add_cv_cohorts(df,10)
head(df)


cohorts_slr<-data.frame(cohort=1:10,
                        mse = rep(NA,10))
prev<-Sys.time()
for(cv_k in 1:10){
  print(cv_k)
  cohort_rows <- which(df_cv$cv_cohort == cv_k)
  train<-df[-cohort_rows ,]
  test<-df[cohort_rows, ]
  slr<-lm(logSum ~ (Transaction.year + School + year + Gender + Married.Alumni.Indicator + 
                      athlete + FRTTY + SOROR + Greek + Part.Level+GradToGive+lastGave+GaveLastYear), data=train)
  test$pred<-predict(slr, test)
  cohorts_slr$mse[cv_k]<-mean((test$logSum - test$pred)^2)
}
time_slr<-Sys.time()-prev
mean(cohorts_slr$mse)



cohorts_mlr1<-data.frame(cohort=1:10,
                        mse = rep(NA,10))
prev<-Sys.time()
for(cv_k in 1:10){
  print(cv_k)
  cohort_rows <- which(df_cv$cv_cohort == cv_k)
  train<-df[-cohort_rows ,]
  test<-df[cohort_rows, ]
  mlr1<-lm(logSum ~ (Transaction.year + School + year + Gender + Married.Alumni.Indicator + 
                       athlete + FRTTY + SOROR + Greek + Part.Level + GaveLastYear*School + 
                       GradToGive*athlete + year*School), data=train)
  
  test$pred<-predict(mlr1, test)
  cohorts_mlr1$mse[cv_k]<-mean((test$logSum - test$pred)^2)
}
time_mlr1<-Sys.time()-prev
mean(cohorts_mlr1$mse)

cohorts_mlr2 <- data.frame(cohort=1:10,
                      mse = rep(NA,10))
# loop over each validation cohort
prev<-Sys.time()
for(cv_k in 1:10){
  print(cv_k)
  cohort_rows <- which(df_cv$cv_cohort == cv_k)
  train<-df[-cohort_rows ,]
  test<-df[cohort_rows, ]
  mlr2<-lm(logSum ~ (Transaction.year + School + year + Gender + Married.Alumni.Indicator + 
                       athlete +FRTTY + SOROR + Greek + Part.Level +GaveLastYear*School + 
                       GradToGive*athlete + year*School + Gender*Married.Alumni.Indicator + 
                       Married.Alumni.Indicator*School+GaveLastYear*athlete ), data=train)
  
  test$pred<-predict(mlr2, test)
  cohorts_mlr2$mse[cv_k]<-mean((test$logSum - test$pred)^2)
}

time_mlr2<-Sys.time()-prev
mean(cohorts_mlr2$mse)














