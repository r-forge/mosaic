\name{manipDerivs}
\alias{manipDerivs}
\title{
Interactive App for Displaying Functions
}
\description{
This applet displays a plot of a function, its derivative, and its antiderviative. 
}
\usage{
manipDerivs(expr, xlim=c(0,10), ...) 
}
\arguments{
  \item{expr}{a mathematical expression. See mosaic \code{D}.
}
  \item{xlim}{ The range of the dependent variable.
  }
  \item{\dots}{ additional arguments to \code{expr}
  }
}
\details{
Displays a function (defined by \code{expr}, along with the derivative and integral of that function.  Allows the user to see how those three functions are related, visually. The user may
}
\value{
A function.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
Andrew Rich (\email{andrew.joseph.rich@gmail.com})
}

\examples{
manipDerivs((cos(x))~x, xlim = range(0, 10))
manipDerivs(sin(exp(x))~x, xlim = range(-2, 3))
}
\keyword{ calculus }
