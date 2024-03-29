% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readAllOutVar.R
\name{readAllOutVar}
\alias{readAllOutVar}
\title{This function is used to read all output files in the working folder}
\usage{
readAllOutVar(workingFolder, numTimesteps)
}
\arguments{
\item{workingFolder}{path to the working folder}

\item{numTimesteps}{a vector of integer numbers, number of output timestep for
each output variables, for example, the first output variables has 3000 output
time step, the second output variables has 4000 time step, then
numTimesteps <- c(3000,4000)}
}
\value{
a list of data frame for each output variable. Each data frame has n
rows (number of rows are number of time steps) and m columns (m is the number
of iterations: column 1 = result from iteration 1 and column i = result from
iteration i)
}
\description{
When running RSWAT, output files will be saved in the working folder/Output/Core
depending number of cores/threads and number of output variables, there will
be many out_var_xx.txt generated in these folders. This function is used to read
these files.
}
\examples{

\donttest{
workingFolder <- "C:/data/workingFolder"
# Assume that I have only 1 observed data with number of output time step = 3000
numTimesteps <- c(3000)

#  if you have two output variables with different number of time steps
#  numTimesteps <- c(3000, 4000)

output <- readAllOutVar("C:/data/workingFolder", numTimesteps)
}

}
