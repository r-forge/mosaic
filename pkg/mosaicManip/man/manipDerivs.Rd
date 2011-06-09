\name{manipDerivs}
\alias{manipDerivs}
\title{
Interactive App for Displaying Functions
}
\description{
This applet displays a function, its derivative, and its antiderviative. 
}
\usage{
manipDerivs(expr, xlim=c(0,10), ...) 
}
\arguments{
  \item{expr}{a mathematical expression. See mosaic \code{D}.
}
  \item{xlim}{ The range of the dependent variable.
  }
  \item{\dots}{ additional arguments to \code{f}
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A function.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
Andrew Rich (\email{arich@macalester.edu})
}

\examples{
manipDerivs(abs(cos(x))~x, xlim = range(0, 10))
}
\keyword{ calculus }
