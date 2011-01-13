\name{D}
\alias{D}
\alias{antiD}
\title{
Derivative and Anti-derivative operators 
}
\description{
Operators for computing numerical derivatives and anti-derivatives as 
functions.
}
\usage{
D(f, h = 1e-04)
antiD(f, input0 = 0, val0 = 0, init = c(input0, val0), ...) 
}
\arguments{
  \item{f}{a function that returns a numerical vector 
  of the same length as its first (numerical) argument.
  All values are required to be finite.
}
  \item{h}{ horizontal distance between points used for secant slope
  calculation in D() 
  }
  \item{input0}{initial input to \code{f} unless \code{init} is used instead }
  \item{val0}{initial value of \code{f} unless \code{init} is used instead }
  \item{init}{ a numerical vector  of length 2 providing an initial
  input to and value of the function \code{f}}
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
Danny Kaplan
}
%\note{
%%%  ~~further notes~~
%}

\examples{
plot(D(sin),xlim=c(-2*pi,2*pi))
plot(antiD(sin),xlim=c(-2*pi,2*pi))
f <- antiD(cos)
f(1)
sin(1)
}
\keyword{ calculus }
