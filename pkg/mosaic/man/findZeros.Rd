\name{findZeros}
\alias{findZeros}
\title{
Find the zeros of a function
}
\description{
Compute numerically the zeros of a function.
}
\usage{
findZeros(expr, ..., xlim=NULL, npts=1000 )
}
\arguments{
  \item{expr}{A formula.  The right side names the variable with respect to which the zeros should be found.  The left side is an expression, e.g. \code{sin(x) ~ x}.  All variables named in the expression must be assigned values via \code{\ldots}
}
  \item{\dots}{Specific numerical values for the unbound variables in the expression.}

  \item{xlim}{The range of the dependent variable to search for zeros. \code{Inf} is a legitimate value, but is interpretted in the numerical sense.}
  \item{npts}{How many sub-intervals to divide the \code{xlim} into when looking for candidates for zeros.  If \code{Inf} is involved, the intervals are numerically spaced up to the largest finite floating point number.}
}
\details{
Searches numerically using \code{uniroot}.
}
\value{
A set of zero or more numerical values.  Plugging these into the expression on the left side of the formula should result in values near zero.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
findZeros( sin(x) ~ x, xlim=c(-10,10) )
findZeros( A*sin(2*pi*t/P) ~ t, xlim=c(0,100), P=50, A=2)
% Interval of a normal at half its maximum height.
findZeros( dnorm(x,mean=0,sd=10) - 0.5*dnorm(0,mean=0,sd=10) ~ x, xlim=c(-Inf,Inf))
}
\keyword{ calculus }
