\name{SMSC}
\alias{SMSC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SMSC estimates the parameters of the mixture of binomial using the non parametric approach.
}
\description{
we propose statistical methods for single cell MethylC-Seq and RRBS data that
simultaneously take into account the spatial correlation between DNA methylation profiles at
neighbouring CpG sites and correct for the experimental and sequencing errors. Following Cheng
& Zhu (2014), we model the observed count of C reads in the presence of sequencing errors with
a mixture of binomial distributions. In our case, we consider a methylation probability that
varies with the genomic location and derive a smoothed kernel-based EM-algorithm to estimate
the parameters of our model.
}
\usage{
SMSC(C, CT, t, p1 = 0.85, p2 = 0.1, method = c("Locfit", "KNN1", "KNN2", "MSC"), eps = 1e-06, K = 70, h = 1000, maxIt = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C}{
   the observed count of C reads at the ith CpG site
}
  \item{CT}{
  the observed count of C reads and T reads for each site i
}
  \item{t}{
  The location of the site i on a chromosome
}
  \item{p1}{
  The error rate for obtaining C reads at unmethylated sites 
}
  \item{p2}{
 the compelement of p2 (1-p2) denote the error rate for obtaining T reads at methylated sites
}
  \item{method}{
   a character string specifying the ckeck the non parametric method to use for smoothing the probability that site i is methylated, valid options are:
		\itemize{
		\item \code{"Locfit"}: we use the .
                \item \code{"KNN1"}: we use the .
                \item \code{"KNN2"}: we use the .
		\item \code{"MSC"}: the probability in the site i is the average of all sites (procedure MSC: Cheng et al 2014),
		} Default is \code{"Locfit"}
}
  \item{eps}{
  convergence threshold for EM algorithm using smoothing technic
}
  \item{K}{
  the kth nearest neighbor
}
  \item{h}{
   the bandwidth
}
  \item{maxIt}{
the maximum number of iterations allowed.
}
}

\value{
An object with S3 class \code{\link{SMSC}}.
}
\references{
		\itemize{
		\item Hansen, K.D., Langmead, B. & Irizarry, R.A. BSmooth: from whole genome bisulfite sequencing reads to differentially methylated regions. Genome Biol. 13, R83 (2012). 
                \item Cheng L, Zhu Y. A classification approach for DNA methylation profiling with bisulfite next-generation sequencing data. Bioinformatics. 2014; 30(2):172–9. 
                \item Rackham OJ, Dellaportas P, Petretto E1, Bottolo L: WGBSSuite: simulating whole-genome bisulphite sequencing data and benchmarking differential DNA methylation analysis tools. 10.1093/bioinformatics/btv114. Epub Mar 15
		} 

Parameswaran Ramachandran,and Theodore J Perkins: Adaptive bandwidth kernel density estimation for next-generation sequencing data. BMC Proc. 2013; 7(Suppl 7): S7. 

}






\examples{
#### MSC scenario
data(Data_MSC)
#### HHM scenario
data(Data_HHM)

y <- Data_MSC #### or y <- Data_HHM

fit0<-SMSC(y$Ccount, y$CT,y$position, method="MSC")

fit1<-SMSC(y$Ccount, y$CT,y$position, method="KNN1")

fit2<-SMSC(y$Ccount, y$CT,y$position, method="KNN2")

fit3<-SMSC(y$Ccount, y$CT,y$position, method="Locfit")

}



\keyword{ Methylation }
\keyword{ Mixture binomial}
\keyword{nonparametric regression}
