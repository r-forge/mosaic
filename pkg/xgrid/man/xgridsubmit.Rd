\name{xgridsubmit}
\alias{xgridsubmit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Submit a Job
}
\description{
Submit a job to an Apple Xgrid.
}
\usage{
xgridsubmit(grid, auth, indir, Rcmd, ntask, param, resfile, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
domain name of the grid controller
}
  \item{auth}{
authorization to use to access the grid: options are "None" (default), "Kerberos" or "Password"
}
  \item{indir}{
directory to be copied to the remote agent
}
  \item{Rcmd}{
R script file within the \code{indir} directory to be run by the remote agent
}
  \item{ntask}{
number of tasks to be run by each job
}
  \item{param}{
an optional parameter to be provided to the remote job

}
  \item{resfile}{
filename to use to save results from the remote job
%%     ~~Describe \code{resfile} here~~
}
  \item{verbose}{
verbose output (default = FALSE)
}
}
\value{
Returns an integer representing the Apple Xgrid job identifier.
}
\author{
Nicholas Horton (\email{nhorton@smith.edu})
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{xgrid}}
}
