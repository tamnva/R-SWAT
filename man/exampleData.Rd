% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{exampleData}
\alias{exampleData}
\title{Example SWAT and SWAT+ data files}
\description{
Examples of TxtInOut, observed data files, and parameter files
of SWAT and SWAT+ that are needed for running some exampled in the code.
Information/data about the example watersheds for the SWAT project can be
found from \doi{10.1016/j.envsoft.2022.105497}, and for the
SWAT+ project was taken from
\url{https://chrisschuerz.github.io/SWATrunR/reference/load_demo.html}. The
example data set consist of following data
"swatFiles" - example list of files in the TxtInOut folder of SWAT \cr
"swatTxtInOut" - example TxtInOut folder (and its files) of SWAT \cr
"swatParam" - example parameter file (swatParam.txt) of SWAT \cr
"swatObservedData" - example observed streamflow data (obs_var_1.txt) of SWAT \cr

"swatPlusFiles" - example list of files in the TxtInOut folder of SWAT+ \cr
"swatPlusTxtInOut" - example TxtInOut folder (and its files) of SWAT+ \cr
"swatPlusParam" - example parameter file (cal_parms.cal) of SWAT+ \cr
"swatPlusObservedData" - example observed streamflow data (obs_var_1.txt) of SWAT+ \cr
}
\examples{

#To see example data name
names(exampleData)

# To see example data content
exampleData$swatParam

}
\seealso{
\code{\link{extracExampleData}} for extracting (save to files) these example data
}
\author{
Tam V. Nguyen \email{tam.nguyen@ufz.de}
}
\keyword{exampleData}
