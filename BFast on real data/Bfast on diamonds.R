library(tidyverse)
head(diamonds)
nrow(diamonds)
diamond<-diamonds
levels(diamond$clarity)[levels(diamond$clarity) == "I1"] <- "worst"
levels(diamond$clarity)[levels(diamond$clarity) == "SI2"] <- "worst"
levels(diamond$clarity)[levels(diamond$clarity) == "SI1"] <- "okay"
levels(diamond$clarity)[levels(diamond$clarity) == "VS2"] <- "okay"
levels(diamond$clarity)[levels(diamond$clarity) == "VS1"] <- "good"
levels(diamond$clarity)[levels(diamond$clarity) == "VVS2"] <- "good"
levels(diamond$clarity)[levels(diamond$clarity) == "VVS1"] <- "top"
levels(diamond$clarity)[levels(diamond$clarity) == "IF"] <- "top"
levels(diamond$cut)[levels(diamond$cut) == "Good"] <- "Fair"
levels(diamond$color)[levels(diamond$color) == "D"] <- "best"
levels(diamond$color)[levels(diamond$color) == "E"] <- "best"
levels(diamond$color)[levels(diamond$color) == "F"] <- "well"
levels(diamond$color)[levels(diamond$color) == "G"] <- "well"
levels(diamond$color)[levels(diamond$color) == "H"] <- "well"
levels(diamond$color)[levels(diamond$color) == "I"] <- "worst"
levels(diamond$color)[levels(diamond$color) == "J"] <- "worst"


### put y variable to the last
diam.mut <- diamond %>%
  mutate(cut = as.character(cut), color = as.character(color), clarity = as.character(clarity), price = log(price), carat = log(carat))

BFast<-function(r1, r2, p, data){
  formula = as.formula("price ~ . -1")
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
MSEaverage<-function(n, loop, r, r1, r2, p, method){
  #n is nrow(data), p # parameter, r size of subsample, loop # of loops for error term
  #dataType: GA, T3, T1, method: UNIF, LEV, BFSLEV, BFLEV, BFLEVUNW, BFDLW
  # r1, r2 are for Bfast leverage 
  storeMSE<-matrix(NA, nrow=loop, ncol=1)
  for (i in 1:loop){
    fit <- lm(price ~ .-1, x=TRUE,data = diam.mut)
    leverage<-hatvalues(fit)
    pi<-leverage/sum(leverage)
    if (method=="UNIF"){
      SampInd <- sample(1:nrow(diam.mut), size = r, replace = T)
      slr<-lm(price ~ .-1, x=TRUE,data = diam.mut[SampInd,])
    } else if (method=="LEV"){
      SampInd <- sample(1:nrow(diam.mut), size = r, prob = pi, rep = T)
      slr<-lm(price ~ .-1, x=TRUE,data = diam.mut[SampInd,], weights = 1/pi[SampInd])
    } else if (method=="BFLEV"){
      BFprob<-BFast(r1, r2, p, diam.mut)
      dat<-cbind(diam.mut, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(price ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)], weights = 1/dat$BFprob[SampInd])
    } else if (method=="BFSLEV"){
      BFprob<-BFast(r1, r2, p, diam.mut)
      dat<-cbind(diam.mut, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = (0.9*dat$BFprob+0.1*(1/nrow(dat))), rep = T)
      slr<-lm(price ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)], weights=1/(0.9*dat$BFprob+0.1*(1/nrow(dat)))[SampInd])
    } else if (method=="BFLEVUNW"){
      BFprob<-BFast(r1, r2, p, diam.mut)
      dat<-cbind(diam.mut, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(price ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)])
    } else if (method=="BFDLEV"){
      BFprob<-BFast(r1, r2, p, diam.mut)
      dat<-cbind(diam.mut, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(price ~ .-1, x=TRUE,data = dat[SampInd, 1:(ncol(dat)-1)], weights = dat$BFprob[SampInd])
    } else if (method=="true"){
      slr<-lm(price ~ .-1, x=TRUE,data = diam.mut)
    }
    storeMSE[i, ]<-mean((predict(slr, diam.mut)-diam.mut$price)^2)
  }
  return(mean(storeMSE))
}

diamondMSE<-as.data.frame(matrix(NA, ncol=5))
names(diamondMSE)<-c("data", "method","data size", "subsample size", "MSE")
route<-0
p=15
r1=2*p
for (n in c(nrow(diam.mut))){
    subsamplesize<-c(150, 200, 250, 300, 400, 500, 1000, 5000, 10000, 50000)
    r2=4
  for (r in subsamplesize){
    for (method in c("UNIF", "LEV", "BFLEV", "BFSLEV", "BFLEVUNW", "BFDLEV")){ #### test which warning is it ####
      print(method)
      route<-route+1
      diamondMSE[route,1]<-"diamonds"
      diamondMSE[route, 2]<-method
      diamondMSE[route, 3]<-n
      diamondMSE[route, 4]<-r
      diamondMSE[route, 5]<-MSEaverage(n, 1000, r, r1, r2, p, method=method)
    }
  }
}

ggplot(data=diamondMSE, aes(x=`subsample size`, y=MSE, color=method))+
  geom_line()+
  ggtitle("diamonds")+
  ylim(0.025, 0.10)+
  xlim(0, 5000)

### with sim and diamonds, make nice plot, and put into latex, and email Dr. Smucker (black and white)
### change n=1000 from diamond dataset
### n=1000, p=10, 50, 100
### n=5000, p=50, 100, 500
### n=10,000, p=50, 100, 500
