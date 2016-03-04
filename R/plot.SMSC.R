plot.SMSC <-
function(ratio,org,t,fit){
  org[org==1] = 1.1
  org[org==0] = -0.1
  if (fit$method == "MSC") pi=rep(fit$pi,length(t))
  else pi = fit$pi
  title = fit$method
  matplot(t,ratio,col=3, type = "p",pch=19,cex=0.2,main=title,ylab="level methy",xlim(min(t),max(t)),xlab="Location",ylim=c(-0.05,1.05))
  matlines(t,org,col=4, type = "p",pch=3,cex=0.5,lwd=2)
  matlines(t,fit$THETAi0,col=2, type = "p",pch=3,cex=0.5)
  matlines(t,pi,col=1, type = "l",pch=4,cex=2, lwd=2)
}
