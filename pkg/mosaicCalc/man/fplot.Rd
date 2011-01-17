\name{fplot}
\alias{fplot}
\title{
Plot a mathematical function
}
\description{
Plot a mathematical function
}
\usage{
fplot(f, xlim, ylim, n = 100, args = list(), type = "l", xlab, ylab, ...)
}
\arguments{
  \item{f}{a function or list of functions to be plotted.  
  First argument must be numeric and 
  output must be a numeric vector of the same length.
  Functions may be described using character strings which name them.
  Constant functions may be specified by giving the numerical value
  of their output.  See examples.
}
  \item{xlim}{Limits on plotting window
}
  \item{ylim}{Limits on plotting window
}
  \item{n}{ approximate number of points to use when generating plot
}
  \item{args}{
  additional arguments to \code{f}
}
  \item{type}{type of plot (see \code{link{xyplot}}).
}
  \item{xlab}{x-axis label}
  \item{ylab}{y-axis label}
  \item{\dots}{
  additional arguments passed to \code{\link{xyplot}}
}
}
\details{
\code{fplot} uses a simple adaptive algorithm to sample more points
in regions where the function is changing rapidly.
}
\value{
a trellis plot produced by \code{\link{xyplot}}
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}

\seealso{
\code{\link{curve}}, \code{\link{xyplot}}
}
\examples{
fplot(sin)
fplot(sin,xlim=c(0,4*pi))
f <- function(t) { sin(1/t)  }
fplot(f)
fplot(f,n=500)
fplot(list("sin",cos,1,-1,function(x) { x^2/25 }), xlim=c(-7,7))
fplot(list("sin",cos,1,-1,function(x) { x^2/25 }), xlim=c(-7,7),
	auto.key=list(
		text=c('cos','sin',1,-1,'quadratic'),
		lines=TRUE,
		points=FALSE)
	)
}
\keyword{ graphics }
