library(MASS)
library(mvtnorm)
################################ data ##############################
##### generate X matrix ####
covariate<-function(p){
  covdat<-matrix(NA, nrow=p, ncol=p)
  for (i in 1:p){
    for (j in 1:p){
      covdat[i, j]<-2*(0.5^(abs(i-j)))
    }
  }
  return(covdat)
}
#### X matrix ####
Xmatrix<-function(n, p, dataType){
  if (dataType=="GA"){
    Xmat<-as.matrix(mvrnorm(n=n,rep(1, length=p), covariate(p)))
  } else if (dataType=="T3"){
    Xmat<-as.matrix(rmvt(n=n, sigma=covariate(p), df=3))
  } else {
    Xmat<-as.matrix(rmvt(n=n, sigma=covariate(p)), df=1)
  }
  return(Xmat)
}
#### beta ####
beta<-function(p){
  beta<-matrix(NA, nrow=p, ncol=1)
  for (i in 1:p){
    if (i < 11 | i > (p-10) ){
      beta[i, 1]<-1
    }else {
      beta[i, 1]<-0.1
    } 
  }
  return(as.matrix(beta))
}
datatype="GA"
#datatype="T3"
#datatype="T1"
p=500
n=5000
looping=10
X<-Xmatrix(n, p, dataType=datatype)
betaNum<-beta(p)
X%*%betaNum
####random noise error term ####
error<-function(n, loop){
  errorMat<-matrix(NA, nrow=n, ncol=loop)
  for (loop in 1:loop){
    print(loop)
    error<-rnorm(n, 0, 3)
    for (i in 1:n){
    errorMat[i, loop]<-error[i]
    }
    }
  return(errorMat)
}
errorMat<-error(n, looping)

Ymatrix<-function(n,loop){
  yMat<-matrix(NA, nrow=n, ncol=loop)
  for (seed in 1:loop){
    print(seed) 
    set.seed(seed)
    yMat[, seed]<-X%*%betaNum+errorMat[, seed]
  }
  return(yMat)
}
Ymat<-Ymatrix(n, loop=looping)
#### BFast approximation ####
BFast<-function(r1, r2, p, data){
  formula = as.formula("Y ~ . -1")
  dt = model.matrix(formula, data)
  PI1.BF <- matrix(sample(c(-1, 1), r1 * nrow(data), replace = TRUE), nrow = r1, ncol = nrow(data))
  PI2.BF <- matrix(sample(c(-1, 1), p * r2, replace = TRUE), nrow = p, ncol = r2)
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
    SYNdata<-as.data.frame(cbind(Ymat[, i],X))
    names(SYNdata)[2:ncol(SYNdata)]<-names(SYNdata)[1:p]
    names(SYNdata)[1]<-"Y"
    fit <- lm(Y ~ .-1,data = SYNdata)
    leverage<-hatvalues(fit)
    pi<-leverage/sum(leverage)
    if (method=="UNIF"){
      SampInd <- sample(1:nrow(SYNdata), size = r, replace = T)
      slr<-lm(Y ~ .-1, data = SYNdata[SampInd,])
    } else if (method=="LEV"){
      SampInd <- sample(1:nrow(SYNdata), size = r, prob = pi, rep = T)
      slr<-lm(Y ~ .-1, data = SYNdata[SampInd,], weights = 1/pi[SampInd])
    } else if (method=="BFLEV"){
      BFprob<-BFast(r1, r2, p, SYNdata)
      dat<-cbind(SYNdata, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(Y ~ .-1, data = dat[SampInd, 1:(ncol(dat)-1)], weights = 1/dat$BFprob[SampInd])
    } else if (method=="BFSLEV"){
      BFprob<-BFast(r1, r2, p, SYNdata)
      dat<-cbind(SYNdata, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = (0.9*dat$BFprob+0.1*(1/nrow(dat))), rep = T)
      slr<-lm(Y ~ .-1, data = dat[SampInd, 1:(ncol(dat)-1)], weights=1/(0.9*dat$BFprob+0.1*(1/nrow(dat)))[SampInd])
    } else if (method=="BFLEVUNW"){
      BFprob<-BFast(r1, r2, p, SYNdata)
      dat<-cbind(SYNdata, BFprob)
      SampInd <- sample(1:nrow(SYNdata), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(Y ~ .-1, data = dat[SampInd, 1:(ncol(dat)-1)])
    } else if (method=="BFDLEV"){
      BFprob<-BFast(r1, r2, p, SYNdata)
      dat<-cbind(SYNdata, BFprob)
      SampInd <- sample(1:nrow(dat), size = r, prob = dat$BFprob, rep = T)
      slr<-lm(Y ~ .-1, data = dat[SampInd, 1:(ncol(dat)-1)], weights = dat$BFprob[SampInd])
    } else if (method=="true"){
      slr<-lm(Y~., data=SYNdata)
    }
    storeMSE[i, ]<-(sum((predict(slr, SYNdata)-SYNdata$Y)^2))/nrow(SYNdata)
  }
  return(mean(storeMSE))
}

#### final result ####
prev<-Sys.time()
GA5000_P500<-as.data.frame(matrix(NA, ncol=5))
names(GA5000_P500)<-c("data", "method","data size", "subsample size", "MSE")
route<-0
for (n in n){
  if (n==1000){
    subsamplesize<-c(150, 200, 300, 400, 500, 700, 1000)
    r2=1
  } else if (n==5000){
    subsamplesize<-c(150, 200, 300, 400, 500, 1000, 1500, 2000, 3000, 5000)
    r2=8
  } else if(n==10000){
    subsamplesize<-c(200, 300, 500, 750, 1000, 2000, 3000, 5000, 10000)
    r2=9
  }
  for (r in subsamplesize){
    for (method in c("UNIF", "LEV", "BFLEV", "BFSLEV", "BFLEVUNW", "BFDLEV")){ #### test which warning is it ####
      print(method)
      route<-route+1
      GA5000_P500[route,1]<-datatype
      GA5000_P500[route, 2]<-method
      GA5000_P500[route, 3]<-n
      GA5000_P500[route, 4]<-r
      GA5000_P500[route, 5]<-MSEaverage(n,looping, r, 2*p, r2, p, method=method)
    }
  }
}
GA5000_P500_time<-Sys.time()-prev

setwd("/Users/wangyuexi/Desktop/research/leverage sampling/my work")
save(GA5000_P500, file="GA5000_P500.Rdata")


