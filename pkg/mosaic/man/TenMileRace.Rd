\name{TenMileRace}
\alias{TenMileRace}
\docType{data}
\title{
Cherry Blossom Race
}
\description{
The Cherry Blossom 10 Mile Run is a road race held in Washington,
D.C. in April each year.  (The name comes from the famous cherry trees
that are in bloom in April in Washington.)  The results of this race
are published.  This data frame  contains the results from the 2005 race. 
}
\usage{data(TenMileRace)}
\format{
  A data frame with 8636 observations on the following variables.
  \describe{
    \item{\code{state}}{State of residence of runner.}
    \item{\code{time}}{Official time from starting gun to finish line.}
    \item{\code{net}}{The recorded time from when the runner crossed the starting
line to when the runner crossed the finish line.  This is generally
less than the official time because of the large number of runners in
the race: it takes time to reach the starting line after the gun has
gone off.}
    \item{\code{age}}{Age of runner in years.}
    \item{\code{sex}}{A factor with levels \code{F} \code{M}.}
  }
}
\examples{
data(TenMileRace)
xyplot(net ~ age, data=TenMileRace, groups=sex)
lm(net ~ age + sex, data=TenMileRace)
}
\keyword{datasets}
