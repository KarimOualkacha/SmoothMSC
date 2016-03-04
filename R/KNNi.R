KNNi <-
function (X,tt,KNi,K,h){
  bandVar <- AssignBandwidths(tt,KNi,K)
  bandVar[bandVar<h]<-h
  u <- ksmooth2(tt,X[1,],kernel = "normal",x.points=tt,band=bandVar)$y  
  u
}
