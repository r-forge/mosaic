\name{xgrid}
\alias{xgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
submit a set of jobs to an Apple Xgrid
}
\description{
submit a set of jobs to an Apple Xgrid, letting the grid deal with scheduling and load balancing
}
\usage{
xgrid(grid = "localhost", numsim = 20, ntask = 1, 
  indir = "input", outdir = "output", param = 1, 
  Rcmd = "runjob.R", auth = "None", outfile = "RESULTS.rda", 
  suffix = "RESULT", throttle = 20, sleeptime = 5, verbose = FALSE)
}

\arguments{
  \item{grid}{
domain name of the grid controller
}
  \item{numsim}{
number of simulations (individual tasks) to run 
}
  \item{ntask}{
number of tasks to be run by each job
}
  \item{indir}{
input directory (contents copied from the client to the agent by the controller)
}
  \item{outdir}{
output directory to be used to store results returned from the agent by the controller
}
  \item{param}{
an optional parameter to be provided to the remote job
}
  \item{Rcmd}{
R script file within the \code{input} directory to be run by the remote agent
}
  \item{auth}{
authorization to use to access the grid: options are "None" (default), "Kerberos" or "Password"
}
  \item{outfile}{
directory to return output created by the remote job
}
  \item{suffix}{
file suffix to use to name individual job results files
}
  \item{throttle}{
maximum number of jobs to run at one time (default = 20)
}
  \item{sleeptime}{
number of seconds to sleep after checking for completion of a remote job (default = 1 second)
}
  \item{verbose}{
verbose output (default = FALSE)
}
}
\details{
The function is used to generate a series of commands to execute using the Apple command line program \code{xgrid} which
stuff jobs on a grid, monitor their progress, and return results when completed.  The function assumes that
the function \code{Rcmd} exists in the \code{input} directory, and that
the return value for this function is stored in the object \code{res0} 
that will be returned separately for each job and collated into 
a single dataframe.

The number of jobs run simultaneously can be controlled using the \code{throttle} option (otherwise, all jobs are queued and the 
Xgrid controller determines which are run).
}

\value{
Returns
a dataframe with number of rows equal to the number of
simulations (tasks) calculated as given by the \code{numsim} option.  
The specific contents of
the dataframe are specified by the code run by the user.
}

\references{
Mac OS X Server: Xgrid Administration and
High Performance Computing Version 10.6 Snow
Leopard (2009). Apple Inc.
}

\author{
Nicholas Horton (\email{nhorton@smith.edu})
}

\note{
It is possible to utilize add-on packages on the remote agent, even if no direct access to the machine
is provided.  This requires that packages are installed into the \code{input} directory, then accessed with the
\code{lib.loc} option to \code{library} by the remote job.

Larger values of \code{sleeptime} may lead to longer job latency upon completion, but will 
provide less of a load on the network.
}

\seealso{
\code{\link{xgridsubmit}},
\code{\link{xgridattr}},
\code{\link{xgridresults}} and
\code{\link{xgriddelete}}
}



