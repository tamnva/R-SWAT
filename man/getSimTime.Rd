% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSimTime.R
\name{getSimTime}
\alias{getSimTime}
\title{Get simulation time setting in the file.cio of SWAT or time.sim of SWAT+}
\usage{
getSimTime(TxtInOut)
}
\arguments{
\item{TxtInOut}{path to the TxtInOut directory}
}
\value{
a list stores information about the simulation time (warmup and
simulation time)
}
\description{
Search for file.cio or time.sim file in the TxtInOut and get the simulation
time setting
}
\examples{

\donttest{
# Create a directory and populate with TxtInOut of SWAT
extracExampleData(exampleData,"swatTxtInOut", tempdir())
TxtInOut <- file.path(tempdir(), "swatTxtInOut")

# Now get hru information
getSimTime(TxtInOut)
}

}
