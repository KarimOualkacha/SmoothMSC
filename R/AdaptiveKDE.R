AdaptiveKDE <-
function(Data,BW,Kernel,Lo,Hi) {
  
  # How many data points?
  N <- length(Data)
  
  # Initialize smooth
  Smooth <- rep(0,Hi-Lo+1)
  
  # Bandwidth multiplier for support
  if (Kernel==1) BWMult <- 5 # Gaussian
  if (Kernel==2) BWMult <- 2.44949 # Triangle (sqrt(6))
  if (Kernel==3) BWMult <- 1.732051 # Square (sqrt(3))
  
  # Kernel function
  if (Kernel==1) KernelFn <- function(X,Center,Bandwidth) { exp(-(X-Center)^2/(2*Bandwidth^2))/(2.506628*Bandwidth) }
  if (Kernel==2) KernelFn <- function(X,Center,Bandwidth) { (1-abs(X-Center)/(floor(2.44949*Bandwidth)))/floor(2.44949*Bandwidth) }
  if (Kernel==3) KernelFn <- function(X,Center,Bandwidth) { 1/(2*floor(1.732051*Bandwidth)+1) }
  
  # Loop over data points
  for (i in 1:N) {
    
    # Center and bandwidth of kernel
    D <- Data[i]
    B <- BW[i]
    
    # Support of kernel
    SupportLo <- max(Lo,ceiling(D-BWMult*B))
    SupportHi <- min(Hi,floor(D+BWMult*B))
    
    if ( SupportHi >=  SupportLo ) {
      # Explicit support
      Support <- SupportLo:SupportHi 
      SupportI <- Support-Lo+1		
      # Evaluating kernel
      KernelVal <- KernelFn(Support,D,B)
      
      # Adding to smooth
      Smooth[SupportI] <- Smooth[SupportI] + KernelVal
    }
  }
  
  # Normalize
  Smooth <- Smooth/N
  
  return(Smooth)
}
