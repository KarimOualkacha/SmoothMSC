\name{Data_HHM}
\alias{Data_HHM}
\docType{data}
\title{
data set generated using the same parameters as in Cheng et al 2014
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("Data_HHM")}
\format{
  A data frame with 1000 observations on the following 4 variables.
  \describe{
    \item{\code{position}}{The location of the site i on a chromosome}
    \item{\code{CT}}{the observed count of C reads and T reads for each site i}
    \item{\code{origin}}{The statue of the ith Cpg site, 0 : the site is unmethylated, 1 : the site is methylated}
    \item{\code{Ccount}}{the observed count of C reads at the ith CpG site}
  }
}

\source{
http://www.wgbssuite.org.uk/
}
\references{
Rackham OJ, Dellaportas P, Petretto E1, Bottolo L: WGBSSuite: simulating whole-genome bisulphite sequencing data and benchmarking differential DNA methylation analysis tools. 10.1093/bioinformatics/btv114. Epub Mar 15
}
\examples{
data(Data_HHM)
## maybe str(Data_HHM) ; plot(Data_HHM) ...
}

