\name{manipTaylor}
\alias{manipTaylor}
\title{
Interactive App to Examine Taylor Approximations and Least Squares Approximations
}
\description{
This applet displays a plot of a function, its Taylor series approximation, and a least squares approximation. 
}
\usage{
manipTaylor(expr, xlim=c(-5, 5), ...) 
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
Displays a function (defined by \code{expr}, along with its corresponding Taylor series calculated at point \code{a}, and the least squares approximation. The user may choose the point at which the Taylor series is evaluated, \code{a}, the degree of the Taylor approximation, and whether to display the least squares approximation or not.
}
\value{
A function.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
Andrew Rich (\email{andrew.joseph.rich@gmail.com})
}

\examples{
manipTaylor(exp(x)~x, xlim = range(-2, 2))
manipTaylor(sin(x)~x, xlim = range(-4, 4))
manipTaylor(sin(x^2)~x, xlim = range(-5,5))
}
\keyword{ calculus }
