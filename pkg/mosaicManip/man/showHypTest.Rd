\name{showHypTest}
\alias{showHypTest}
\title{
Interactive display for hypothesis testing concepts.
}
\description{
Displays a sampling distribution and calculates power.
}
\usage{
showHypTest(doF=FALSE)
}
\arguments{
  \item{doF}{Show the F distribution.  Default: a t-distribution for the sample statistic.}
}
}
\details{
Allows the user to set various parameters that govern the sampling distribution, and the 
significance, then calculates the power.  Displays all this graphically.
}
\value{
A function.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
}

\examples{
showHypTest()
}
\keyword{statistics}
