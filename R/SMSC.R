SMSC <-
function (C, CT, t, p1 = 0.85, p2=0.10, method=c("Locfit","KNN1","KNN2","MSC"), eps = 1e-06,K=70,h=1000,maxIt=100){
 
  method <- match.arg(method)
  ny=length(C)
  u <- 0.85
  if (method=="KNN1") KNi=1
  if (method=="KNN2") KNi=2
  if (method!="MSC")  u = rep(u,ny)
  bpi1 <- matrix(0, nrow = 2, ncol = ny)
  bpi1[1,] <- u*dbinom(C,size=CT,prob=p1)/(u*dbinom(C,size=CT,prob=p1) + (1-u)*dbinom(C,size=CT,prob=p2))
  bpi1[2,] <- 1-bpi1[1,]
  newlog <- sum(log(u*dbinom(C,size=CT,prob=p1)+(1-u)*dbinom(C,size=CT,prob=p2)))
  error <- 0.1
  l=0
  while ((error > eps)&(l<maxIt)) {
    l=l+1
    p1old <- p1
    p2old <- p2
    uold <- u
    oldlog <- newlog
    u <- switch(method,MSC=Cheng_U(bpi1),KNN1=KNNi(bpi1,t,KNi,K=K,h=h),KNN2=KNNi(bpi1,t,KNi,K=K,h=h),
                Locfit=locfit_u(bpi1,t,K=K,h=h))
    p1 <- sum(bpi1[1,]*C)/sum(bpi1[1,]*CT)
    p2 <- sum(bpi1[2,]*C)/sum(bpi1[2,]*CT)
    newlog <- sum(log(u*dbinom(C,size=CT,prob=p1)+(1-u)*dbinom(C,size=CT,prob=p2)))
    error <- newlog - oldlog
    bpi1[1,] <- u*dbinom(C,size=CT,prob=p1)/(u*dbinom(C,size=CT,prob=p1) + (1-u)*dbinom(C,size=CT,prob=p2))
    bpi1[2,] <- 1-bpi1[1,]
  }
  object <- list(pi=u,THETAi0=bpi1[1,],p1=p1,p2=p2,method=method)
  class(object) <- "SMSC"
  object
}
