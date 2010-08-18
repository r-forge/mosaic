\name{D}
\alias{D}
\title{Numerical differentiation }
\description{
Numerical differentiation based on computing the slope of a secant line.
}
\usage{
D(f, n = 1, h = 5e-05)
}
\arguments{
  \item{f}{ function to be differentiated with respect to its first argument
}
  \item{n}{
  	order of derivative
}
  \item{h}{ (horizontal) distance between two points used to approximate
  the derivative
}
}
\value{
A function of one variable approximating the derivative of \code{f}
}
\author{
Danny Kaplan and Randall Pruim
}

\section{Warning}{This function masks the symbolic differentiation function
by the same name, but it is still available as \code{\link{deriv}}.}

\seealso{
See also \code{\link{deriv}}
}
\examples{
curve(D(sin)(x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
