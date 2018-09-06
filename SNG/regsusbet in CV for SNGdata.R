library(dplyr)
library(tidyr)
library(leaps)
library(ggplot2)

setwd("/Users/wangyuexi/Desktop/Yuexi/data")
SNGdata<-read.csv("SNGdata.csv")
SNGdata$logSum<-log(SNGdata$Amount)
SNGdata1987<-SNGdata%>%filter(year >= 1987)
df<-SNGdata1987[, -c(1, 2, 4, 5, 8, 12, 13,14, 15:18, 21, 22)]
df<-df[which(df$logSum!="-Inf"),]
## delete state with NA
df<-df[which(df$Gender!="N"), ]
df[, 5]<-as.factor(df[, 5])
df[, 6]<-as.factor(df[, 6])
df[, 7]<-as.factor(df[, 7])
df[, 8]<-as.factor(df[, 8])

## set school IS, RC FA as a group
df$School<-as.character(df$School)
df$School[df$School=="FA"]<-"others"
df$School[df$School=="IS"]<-"others"
df$School[df$School=="RC"]<-"others"
## fer and sor as 0 and 1
df$FRTTY[df$FRTTY ==2]<-1
df$FRTTY[df$SOROR ==2]<-1


smp_size <- floor(0.75 * nrow(df))
set.seed(12345)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

regfit.best <- regsubsets(logSum~.^3+I(year^3)+I(lastGave^3)+I(GradToGive^3), data=train, nvmax =1000, method = "forward")
test.mat=model.matrix(logSum~.^3+I(year^3)+I(lastGave^3)+I(GradToGive^3),data=test)
coefi=coef(regfit.best,id=155)
pred=test.mat[,names(coefi)]%*%coefi

###### second order #############
############################### 10-fold Cross validation

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
store_mse_2<-matrix(, nrow=10, ncol=47)
nvmax_mse_2<-data.frame(nvmax=1:47, 
                      testMSE=rep(NA, 47))

prev<-Sys.time()
for (cv_k in 1:10){
  print(cv_k)
  val.errors=rep(NA,47)
  exp.val.errors=rep(NA,47)
  cohort_rows <- which(df_cv$cv_cohort == cv_k)
  train<-df[-cohort_rows ,]
  test<-df[cohort_rows, ]
  regfit.best <- regsubsets(logSum~.^2, data=train, nvmax =47, method = "forward")
  test.mat=model.matrix(logSum ~ .^2,data=test)
  for(i in 1:47){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    store_mse_2[cv_k,i]=mean((test$logSum-pred)^2) 
  }
  
}
Sys.time()-prev
nvmax_mse_2$testMSE<-colSums(store_mse_2)/10
### pick the best model with min(nvmax)
regfit.best <- regsubsets(logSum~.^2, data=df, nvmax = which(nvmax_mse_2$testMSE==min(nvmax_mse_2$testMSE)), method = "forward")
coef(regfit.best,which(nvmax_mse$testMSE==min(nvmax_mse$testMSE)))
###### nvma=47 is the best model mse=1.2404


############ third order #######
df_cv <- add_cv_cohorts(df,10)
store_mse_3<-matrix(, nrow=10, ncol=329)
nvmax_mse_3<-data.frame(nvmax=1:329, 
                        testMSE=rep(NA, 329))

prev<-Sys.time()
for (cv_k in 1:10){
  print(cv_k)
  val.errors=rep(NA,329)
  exp.val.errors=rep(NA,329)
  cohort_rows <- which(df_cv$cv_cohort == cv_k)
  train<-df[-cohort_rows ,]
  test<-df[cohort_rows, ]
  regfit.best <- regsubsets(logSum~.^3+I(year^3)+I(lastGave^3)+I(GradToGive^3), data=train, nvmax =329, method = "forward")
  test.mat=model.matrix(logSum~.^3+I(year^3)+I(lastGave^3)+I(GradToGive^3),data=test)
  for(i in 1:329){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    store_mse_3[cv_k,i]=mean((test$logSum-pred)^2) 
  }
  
}
Sys.time()-prev
nvmax_mse_3$testMSE<-colSums(store_mse_3)/10
### pick the best model with min(nvmax)
regfit.best <- regsubsets(logSum~.^3+I(year^3)+I(lastGave^3)+I(GradToGive^3), data=df, nvmax = which(nvmax_mse_3$testMSE==min(nvmax_mse_3$testMSE)), method = "forward")
coef(regfit.best,which(nvmax_mse_3$testMSE==min(nvmax_mse_3$testMSE)))
###nvmax=29, mse=1.245041

###### plot ####
nvmax_mse_2$group<-"quadratic order"
nvmax_mse_3$group<-"cubic order"

plot_data<-rbind(nvmax_mse_2, nvmax_mse_3)
save(plot_data, file="cubic_quadratic.Rdata")

cubic<-ggplot(data=nvmax_mse_3, aes(nvmax, testMSE))+
  geom_smooth()+
  geom_point(size=0.5)

load("cubic_quadratic.Rdata")
plot_data1<-plot_data[-c(1:47), ]
ggplot(data=plot_data1, aes(nvmax, testMSE))+
  geom_smooth()+
  geom_point(size=0.5)+
  ggtitle("testMSE for different size of model")


names(SNGdata)








prev<-Sys.time()
for (cv_k in 1:2){
  print(cv_k)
  val.errors=rep(NA,329)
  exp.val.errors=rep(NA,329)
  cohort_rows <- which(df_cv$cv_cohort == cv_k)
  train<-df[-cohort_rows ,]
  test<-df[cohort_rows, ]
  regfit.best <- regsubsets(logSum~.^3, data=train, nvmax =329, method = "forward")
  test.mat=model.matrix(logSum~.^3,data=test)
  for(i in 1:329){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    store_mse_3[cv_k,i]=mean((test$logSum-pred)^2) 
  }
  
}
Sys.time()-prev
nvmax_mse_3$testMSE<-colSums(store_mse_3)/10
regfit.best <- regsubsets(logSum~.^3, data=df, nvmax = 329, method = "forward")
coef(regfit.best,29)



