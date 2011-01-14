\name{do}
\alias{do}
\title{
Do Things Repeatedly
}
\description{
\code{do()} provides a natural syntax for repetition tuned to assist 
with replication methods.
}
\usage{
do(n = 1, mode=NULL) * x
}
\arguments{
  \item{n}{ number of times to repeat }
  \item{mode}{ target mode for value returned}
  \item{x}{ an object or function to be repeated }
}
\details{
Need to add details here.
}
\value{
\code{do} returns an object of class \code{repeater} which is only useful in
the context of the operator \code{*}.
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
do(3) * lm(shuffle(height) ~ sex + mother, galton)
do(3) * summary(lm(shuffle(height) ~ sex + mother, galton))
do(3) * 1:4
do(3) * mean(rnorm(25))
do(3) * c(mean = mean(rnorm(25)))
}
\keyword{ iteration }
