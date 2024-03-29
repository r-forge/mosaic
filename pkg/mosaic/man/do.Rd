\name{do}
\alias{do}
\title{
Do Things Repeatedly
}
\description{
\code{do()} provides a natural syntax for repetition tuned to assist 
with replication and resampling methods.
}
\usage{
do(n = 1, cull=NULL, mode=NULL) 

}
\arguments{
  \item{n}{ number of times to repeat }
  \item{cull}{ function for culling output of objects being repeated.  If NULL,
  a default culling function is used.  The default culling function is 
  currently aware of objects of types
  \code{lme},
  \code{lm},
  \code{htest},
  \code{table},
  \code{cointoss}, and 
  \code{matrix}.
  }
  \item{mode}{ target mode for value returned}
}

\value{
\code{do} returns an object of class \code{repeater} which is only useful in
the context of the operator \code{*}.  See the examples.
}
\author{
Daniel Kaplan (\email{kaplan@macalaster.edu})
and Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
\code{\link{replicate}}
}
\examples{
do(3) * rnorm(1)
do(3) * "hello"
do(3) * lm(shuffle(height) ~ sex + mother, Galton)
do(3) * summary(lm(shuffle(height) ~ sex + mother, Galton))
do(3) * 1:4
do(3) * mean(rnorm(25))
do(3) * c(mean = mean(rnorm(25)))
}
\keyword{ iteration }



