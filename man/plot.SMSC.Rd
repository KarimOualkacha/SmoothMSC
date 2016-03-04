\name{plot.SMSC}
\alias{plot.SMSC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the "SMSC" object
}
\description{
An Illustrated Plot Summary (see: Article)
}
\usage{
plot.SMSC(ratio, org, t, fit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ratio}{
   the observed count of C reads at the ith CpG site divided by  the observed count of C reads and T reads for each site i
}
  \item{org}{
     The statue of the ith Cpg site, 0 : the site is unmethylated, 1 : the site is methylated
}
  \item{t}{
The location of the site i on a chromosome
}
  \item{fit}{
SMSC object
}
}

\examples{
#### MSC scenario
data(Data_MSC)
#### HHM scenario
data(Data_HHM)

y <- Data_MSC #### or y <- Data_HHM

origin = y$origin
ratio=y$Ccount/y$CT
position = y$position

fit0<-SMSC(y$Ccount, y$CT,y$position, method="MSC")
plot(ratio,origin,position,fit0)

fit1<-SMSC(y$Ccount, y$CT,y$position, method="KNN1")
plot(ratio,origin,position,fit1)

fit2<-SMSC(y$Ccount, y$CT,y$position, method="KNN2")
plot(ratio,origin,position,fit2)

fit3<-SMSC(y$Ccount, y$CT,y$position, method="Locfit")
plot(ratio,origin,position,fit3)
}

