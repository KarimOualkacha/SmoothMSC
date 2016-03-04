\name{Data_MSC}
\alias{Data_MSC}
\docType{data}
\title{
  data set generated using the same parameters as in Cheng et al 2014
}

\usage{data("Data_MSC")}
\format{
  A data frame with 1000 observations on the following 4 variables.
  \describe{
    \item{\code{position}}{The location of the site i on a chromosome}
    \item{\code{CT}}{the observed count of C reads and T reads for each site i}
    \item{\code{origin}}{The statue of the ith Cpg site, 0 : the site is unmethylated, 1 : the site is methylated}
    \item{\code{Ccount}}{the observed count of C reads at the ith CpG site}
  }
}


\references{
Cheng L, Zhu Y. A classification approach for DNA methylation profiling with bisulfite next-generation sequencing data. Bioinformatics. 2014; 30(2):172–9. 
}
\examples{
data(Data_MSC)
## maybe str(Data_MSC) ; plot(Data_MSC) ...
}

