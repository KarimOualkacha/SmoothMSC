AKDE <-
function(Data,BWRule,BWParam,KernelRule,Lo,Hi) {
  BW <- AssignBandwidths(Data,BWRule,BWParam)
  Smooth <- AdaptiveKDE(Data,BW,KernelRule,Lo,Hi)
  return(Smooth)
}
