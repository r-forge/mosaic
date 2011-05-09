\name{aggregating}
\alias{mean}
\alias{mean.formula}
\alias{mean.default}
\alias{sd}
\alias{sd.formula}
\alias{sd.default}
\alias{var}
\alias{var.formula}
\alias{var.default}

\title{
Aggregating summary statistics
}
\description{
These drop-in replacements for several summary statistics functions are formula aware.
When given formulas, they call \code{\link{aggregate}} using the formula.
}
\usage{
mean(x, ...)
\method{mean}{formula}(x, data, na.rm=TRUE, ...)
\method{mean}{default}(x, na.rm=TRUE, ...)

sd(x, ...)
\method{sd}{formula}(x, data, na.rm=TRUE, ...)
\method{sd}{default}(x, na.rm=TRUE, ...)

var(x, ...)
\method{var}{formula}(x, data, na.rm=TRUE, ...)
\method{var}{default}(x, na.rm=TRUE, ...)

}
\arguments{
  \item{x}{ an R object, possibly a formula }

  \item{data}{ a data frame for the formula methods}

  \item{na.rm}{ a logical indicating whether missing data should be removed 
  before calculation.  
  }
  \item{\dots}{additional arguments}
}
\details{
The default value for \code{na.rm} is reversed from the functions in \code{stats}.
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
\code{link{aggregate}}
}
\examples{
mean(age ~ sex, data=HELP)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
