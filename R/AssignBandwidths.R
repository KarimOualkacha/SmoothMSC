AssignBandwidths <-
function(Data, Rule, Param) {
  
  # How many data points?
  N <- length(Data)
  
  # Rule 0 means fixed bandwidth of Param for every data point
  if (Rule == 0) {
    BW <- rep(Param,N)
  }
  
  # Rule 1 is classic k-nearest neighbor (Param is k)
  if (Rule == 1) {
    BW <- rep(0,N)
    for (i in 1:N) {
      Lo <- max(1,i-Param)
      Hi <- min(N,i+Param)
      Temp <- sort(abs(Data[Lo:Hi]-Data[i]))
      BW[i] <- Temp[min(Param+1,length(Temp))]
    }
  }
  
  # Rule 2 is the max of Rule 1 and the distance to left and right
  if (Rule == 2) {
    BW <- rep(0,N)
    for (i in 1:N) {
      # Rule 1
      Lo <- max(1,i-Param)
      Hi <- min(N,i+Param)
      Temp <- sort(abs(Data[Lo:Hi]-Data[i]))
      Rule1 <- Temp[min(Param+1,length(Temp))]
      
      # Left neighbor
      Lefti <- max(1,i-1)
      Left <- Data[i]-Data[Lefti]
      
      # Right neighbor
      Righti <- min(i+1,N)
      Right <- Data[Righti]-Data[i]
      
      # And finally...
      BW[i] <- max(Rule1,Left,Right)
    }
  }
  
  # Rule 3 is the max of the kth neighbor to left and kth neighbor to right
  if (Rule == 3) {
    BW <- rep(0,N)
    for (i in 1:N) {
      Lo = max(1,i-Param)
      Hi = min(N,i+Param)
      BW[i] <- max(Data[i]-Data[Lo],Data[Hi]-Data[i])
    }
  }
  
  return(BW)
}
