\name{xgrid}
\alias{xgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xgrid(grid = "mathgrid0.smith.edu", numsim = 20, ntask = 1, outdir = "output", param = 1, Rcmd = "runjob.R", auth = "Kerberos", outfile = "RESULTS.rda", suffix = "RESULT", throttle = 20, sleeptime = 15, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{numsim}{
%%     ~~Describe \code{numsim} here~~
}
  \item{ntask}{
%%     ~~Describe \code{ntask} here~~
}
  \item{outdir}{
%%     ~~Describe \code{outdir} here~~
}
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{Rcmd}{
%%     ~~Describe \code{Rcmd} here~~
}
  \item{auth}{
%%     ~~Describe \code{auth} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{suffix}{
%%     ~~Describe \code{suffix} here~~
}
  \item{throttle}{
%%     ~~Describe \code{throttle} here~~
}
  \item{sleeptime}{
%%     ~~Describe \code{sleeptime} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(grid="mathgrid0.smith.edu", numsim=20, ntask=1, 
   outdir="output", param=1, Rcmd="runjob.R", auth="Kerberos", outfile="RESULTS.rda", 
   suffix="RESULT", throttle=20, sleeptime=15, verbose=FALSE) {
   # submit a group of jobs to the Xgrid, letting the grid deal with 
   # scheduling and load balancing
   # numsim is the total number of simulations to run
   # ntask is the maximum number of repetitions (tasks) per job
   offset = 101  # needs to be 3 digits to keep formatting clean
   alljobs = floor(numsim/ntask)
   if (verbose==TRUE) {
      cat("numsim=",numsim," ntask=",ntask,"alljobs=",alljobs, "\n")
   }
   if (alljobs != ceiling(numsim/ntask)) {
      stop("numsim divided by ntask should be an integer!")
   }
   if (ceiling(numsim/ntask)<2) {
      stop("must have at least 2 jobs!")
   }

   if (file.access(outdir, 0) == -1) {
      if (system(paste("mkdir ", outdir, sep="")) != 0) {
         stop(paste("The directory '", outdir, "' can't be created!\n", sep=""))
      }
   } else if (file_test("-d", outdir) != TRUE) {
      stop(paste("The directory '", outdir, "' is not a directory!\n", sep=""))
   }

   # process the jobs
   jobs = numeric(alljobs)
   jobidentifier = 1:alljobs+9999
   for (i in 1:length(jobs)) {
      jobs[i] = xgridsubmit(grid, auth, Rcmd, ntask, param,
         paste(suffix, "-", jobidentifier[i], sep=""), verbose)
   }
   if (verbose==TRUE) {
      cat("jobs=",jobs,"\n")
   }
   whichjob = 1
   while (length(jobs)>0) {
      if (verbose==TRUE) {
         cat("length of jobs=", length(jobs)," whichjob=", whichjob, "\n", sep="")
      }
      returnlist = checkonjob(whichjob, jobs, grid, outdir, auth, sleeptime, verbose)
      jobs = returnlist$jobs
      whichjob = returnlist$whichjob
   }
   return(collateresults(jobidentifier, ntask, outdir, outfile, suffix, verbose))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
