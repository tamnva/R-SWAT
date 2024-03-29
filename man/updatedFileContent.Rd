% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updatedFileContent.R
\name{updatedFileContent}
\alias{updatedFileContent}
\title{Load content of the files in the TxtInOut that are need to be updated every
model run}
\usage{
updatedFileContent(HRUinfo, paraSelection, SWATParam, TxtInOutFolder)
}
\arguments{
\item{HRUinfo}{Dataframe which contains HRU information}

\item{paraSelection}{Dataframe which contains the selected parameters to
change and their respective information (e.g., range, spatial level,...).}

\item{SWATParam}{Dataframe contains parameters of SWAT or SWAT+}

\item{TxtInOutFolder}{Link to the TxtInOut folder}
}
\value{
A list of all files and their content that will be updated/rewritten
every model run.
}
\description{
Load content of the files in the TxtInOut that are need to be updated every
model run
}
\examples{

\donttest{
updatedFileContent(HRUinfo, paraSelection, SWATParam, TxtInOutFolder)
}

}
\seealso{
[getHruInfo()] to read HRU information from a file and return
dataframe which contains HRU information
}
