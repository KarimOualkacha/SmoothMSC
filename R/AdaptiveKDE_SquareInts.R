AdaptiveKDE_SquareInts <-
function(Data,BW,Lo,Hi) {
  # Compute the radius of the square kernel at each data point
  Radii <- floor(sqrt(3)*BW)
  
  # Find start and end positions
  Starts <- pmax(Data-Radii,Lo)
  Ends <- pmin(Data+Radii,Hi)
  
  # We only do ones that overlap the Lo:Hi interval
  # We add an NA to the end of each list, for convenience in the
  # looping logic that follows
  ToDo <- Starts<Ends
  Starts <- Starts[ToDo]
  Ends <- Ends[ToDo]
  Heights <- 1/(2*Radii[ToDo]+1)
  
  # First, make a unified list of starts and ends, and the changes in height
  StartsAndEnds <- c(Starts,Ends)
  HeightChanges <- c(Heights,-Heights)
  
  # Next, sort increasing and add a dummy at the end
  O = order(StartsAndEnds)
  StartsAndEnds <- c(StartsAndEnds[O],Inf)
  HeightChanges <- HeightChanges[O]
  
  # Intervals will be filled in here - start position, end position, height
  Ints <- matrix(NA,nrow=2*length(ToDo),ncol=3)
  NInts <- 0
  CurrIdx <- 1
  CurrHeight <- 0
  while (CurrIdx<length(StartsAndEnds)) {
    # Create the next interval
    IntStart <- StartsAndEnds[CurrIdx]
    
    # Find where interval will end
    NextIdx <- CurrIdx
    while (StartsAndEnds[NextIdx]==StartsAndEnds[CurrIdx]) {
      NextIdx <- NextIdx+1
    }
    
    # As long as we have a real end...
    if ( StartsAndEnds[NextIdx]<Inf ) {
      # Accumlate the new height
      for (i in CurrIdx:(NextIdx-1)) {
        CurrHeight <- CurrHeight + HeightChanges[i]/length(Data)
      }
      
      # Install the next interval, if non-zero
      if (CurrHeight > 0) {
        NInts <- NInts+1
        Ints[NInts,1] <- StartsAndEnds[CurrIdx]
        Ints[NInts,2] <- StartsAndEnds[NextIdx]
        Ints[NInts,3] <- CurrHeight
      }
    }	
    # Update to start of next interval
    CurrIdx <- NextIdx
  }
  
  Ints <- Ints[1:NInts,]
  return(Ints)
}
