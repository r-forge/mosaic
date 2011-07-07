\name{xgridresults}
\alias{xgridresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
retrieve job results
}
\description{

retrieve results from a completed job on an Apple Xgrid
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
None.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicholas Horton (\email{nhorton@smith.edu})
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{xgrid}}
}
