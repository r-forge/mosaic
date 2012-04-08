\name{xgridresults}
\alias{xgridresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Job Results
}
\description{
Retrieve results from a completed job on an Apple Xgrid.
}
\usage{
xgridresults(grid, auth, jobnum, outdir, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
domain name of the grid controller
}
  \item{auth}{
authorization to use to access the grid: options are "None" (default), "Kerberos" or "Password"
}
  \item{jobnum}{
Apple Xgrid job identifier
}
  \item{outdir}{
directory to return output created by the remote job

}
  \item{verbose}{
verbose output (default = FALSE)
}
}
\value{
None.
}
\author{
Nicholas Horton (\email{nhorton@smith.edu})
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{xgrid}}
}
