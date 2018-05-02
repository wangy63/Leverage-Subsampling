Adv_data<-na.omit(Adv_data)
dat<-Adv_data[, -c(1, 2, 8, 12, 13, 14, 15:22)]
dat<-dat[, -c(5)]
dat<-dat[which(dat$Gender!="N"), ]
dat$Amount<-log(dat$Amount)
dat<-dat[which(dat$Amount!="-Inf"), ]
#dat<-dat[which(dat$GradToGive>=0), ]
dat$count.Degree<-as.factor(dat$count.Degree)
dat$School<-as.factor(dat$School)

levels(dat$count.Degree)[levels(dat$count.Degree) == 2] <- "more than one"
levels(dat$count.Degree)[levels(dat$count.Degree) == 3] <- "more than one"
levels(dat$count.Degree)[levels(dat$count.Degree) == 4] <- "more than one"
levels(dat$School)[levels(dat$School) == "99"] <- "other"
levels(dat$School)[levels(dat$School) == "IS"] <- "other"
levels(dat$School)[levels(dat$School) == "RC"] <- "other"
levels(dat$School)[levels(dat$School) == "FA"] <- "other"


BFast<-function(r1, r2, p, data){
  formula = as.formula("Amount ~ . -1")
  dt = model.matrix(formula, data)
  PI1.BF <- matrix(sample(c(-1, 1), r1 * nrow(data), replace = TRUE), nrow = r1, ncol = nrow(data))
  PI2.BF <- matrix(sample(c(-1, 1), p * r2, replace = TRUE), nrow = p, ncol = r2) ### dummy variable? 
  R.BF <- qr.R(qr((PI1.BF)%*%(dt)))
  Rinv.BF <- solve(R.BF)
  dtfrm.BF <- (dt)%*%(Rinv.BF)%*%(PI2.BF)
  Xpinv.BF <- solve(t(dtfrm.BF)%*%(dtfrm.BF))
  prob.BF <- rowSums((dtfrm.BF)%*%(Xpinv.BF) * (dtfrm.BF))/r2
  return(prob.BF)
}


#### MSE ###
fit <- lm(Amount ~ .-1, x=TRUE,data = dat)
leverage<-hatvalues(fit)
pi<-leverage/sum(leverage)
MSEaverage<-function(n, loop, r, r1, r2, p, method){
  #n is nrow(data), p # parameter, r size of subsample, loop # of loops for error term
  #dataType: GA, T3, T1, method: UNIF, LEV, BFSLEV, BFLEV, BFLEVUNW, BFDLW
  # r1, r2 are for Bfast leverage 
  storeMSE<-matrix(NA, nrow=loop, ncol=1)
  for (i in 1:loop){
    if (method=="UNIF"){
      SampInd <- sample(1:nrow(dat), size = r, replace = T)
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat[SampInd,])
    } else if (method=="LEV"){
      SampInd <- sample(1:nrow(dat), size = r, prob = pi, rep = T)
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat[SampInd,], weights = 1/pi[SampInd])
    } else if (method=="BFLEV"){
      temp<-cbind(dat, BFprob)
      SampInd <- sample(1:nrow(temp), size = r, prob = temp$BFprob, rep = T)
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)], weights = 1/dat$BFprob[SampInd])
    } else if (method=="BFSLEV"){
      dat<-cbind(dat, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = (0.9*dat$BFprob+0.1*(1/nrow(dat))), rep = T)
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)], weights=1/(0.9*dat$BFprob+0.1*(1/nrow(dat)))[SampInd])
    } else if (method=="BFLEVUNW"){
      dat<-cbind(dat, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)])
    } else if (method=="BFDLEV"){
      dat<-cbind(dat, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)], weights = dat$BFprob[SampInd])
    } else if (method=="true"){
      slr<-lm(Amount ~ .-1, x=TRUE,data = dat)
    }
    storeMSE[i, ]<-mean((predict(slr, dat)-dat$Amount)^2)
  }
  return(mean(storeMSE))
}

AdvancementMSE<-as.data.frame(matrix(NA, ncol=5))
names(AdvancementMSE)<-c("data", "method","data size", "subsample size", "MSE")
route<-0
p=13
r1=2*p
BFprob<-BFast(r1, 13, p, dat)
for (n in c(nrow(dat))){
  subsamplesize<-c(1000, 5000, 7000, 10000, 20000, 50000) ### smaller sample size
  r2=13
  for (r in subsamplesize){
    for (method in c("UNIF", "LEV", "BFLEV")){ #### test which warning is it ####
      print(method)
      route<-route+1
      AdvancementMSE[route,1]<-"advancement"
      AdvancementMSE[route, 2]<-method
      AdvancementMSE[route, 3]<-n
      AdvancementMSE[route, 4]<-r
      AdvancementMSE[route, 5]<-MSEaverage(n, 5, r, r1, r2, p, method=method)
    }
  }
}

AdvancementMSE1<-AdvancementMSE[which(AdvancementMSE$`subsample size`>= 5000), ]
ggplotly(ggplot(data=AdvancementMSE1, aes(x=`subsample size`, y=MSE, color=method))+
  geom_line()+
  ggtitle("advancement")+
  theme_bw())



