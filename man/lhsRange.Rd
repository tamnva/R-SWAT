% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lhsRange.R
\name{lhsRange}
\alias{lhsRange}
\title{Random sample of n parameters within a given range using Latin Hypercube Sampling}
\usage{
lhsRange(nIter, paramRange)
}
\arguments{
\item{nIter}{The number of samples}

\item{paramRange}{Dataframe with two columns, the first row and second row
are the minimum and maximum range. The number of rows is the number of
parameters, each parameter in one row}
}
\value{
Data frame with the first columns is the is the sample ID, followed
by n columns correspond to n parameters. The number of rows is the number
of sample (nIter)
}
\description{
Random sample of n parameters within a given range using Latin Hypercube Sampling
}
\examples{

lhsRange(nIter=5, paramRange=data.frame(min=c(0,0,0), max=c(2,2,2)))

}
