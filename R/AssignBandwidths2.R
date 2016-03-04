AssignBandwidths2 <-
function(Data, h) {
  
  # How many data points?
  N <- length(Data)
  BW <- rep(0,N)
  for (i in 1:N) BW[i] <- sum(abs(Data[i]-Data)<h)
  return(BW)
}
