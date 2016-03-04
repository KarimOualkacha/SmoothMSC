locfit_u <-
function (X,tt,K,h){
  nn=K/dim(X)[2]
  fit <- locfit(X[1,] ~ lp(tt, nn = nn, h = h), family = "binomial", maxk = 10000)
  u=fitted.values(fit)
  u
}
