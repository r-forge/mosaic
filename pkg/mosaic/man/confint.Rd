\name{confint}
\alias{confint.data.frame}
\alias{confint.numeric}

\title{
Confidence intervals on distributions.
}
\description{
These facilitate the simple calculation of confidence intervals on bootstrap-like simulations.
}
\usage{
\method{confint}{data.frame}(object, parm, level=0.95, method=c("stderr", "quantile"), margin=FALSE)
\method{confint}{numeric}(object, parm, level=0.95, method=c("stderr", "quantile"), margin=FALSE)
}

\arguments{
  \item{object}{ an R object, especially a data frame or a numerical variable from a data frame }
  \item{parm}{Not used.  For compatibility with \code{stats::confint}.}
  \item{level}{The confidence level.}
  \item{method}{Whether to use a calculation direct from quantiles, or one based on the standard error and a t statistic.}
  \item{margin}{If \code{TRUE}, instead of reporting the lower and upper bounds, report a point estimate and a margin of error.}
  \item{\dots}{additional arguments}
}
\details{
Intended to pull confidence intervals out of resampling distributions.
}
\value{
For a single variable, returns a vector with the upper and lower bounds of the CI, or a vector with the center and margin of error.

For a data frame, returns a new data frame with the above information for each quantitative variable.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu})
}
\seealso{
\code{resample}, \code{shuffle}, \code{do}
}
\examples{
data("KidsFeet")
s = do(100) * mean( width ~ sex, data=resample(KidsFeet) )
confint(s)
confint(s, margin=TRUE)
confint(s, method="quantile")
}
\keyword{ bootstrap }
