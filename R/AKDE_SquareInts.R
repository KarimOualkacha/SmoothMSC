AKDE_SquareInts <-
function(Data,BWRule,BWParam,Lo,Hi) {
  BW <- AssignBandwidths(Data,BWRule,BWParam)
  Smooth <- AdaptiveKDE_SquareInts(Data,BW,Lo,Hi)
  return(Smooth)
}
