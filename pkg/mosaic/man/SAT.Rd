\name{SAT}
\alias{SAT}
\docType{data}
\title{
State by State SAT data
}
\description{
SAT data
}
\usage{data(SAT)}
\format{
  A data frame with 50 observations on the following 8 variables.
  \describe{
    \item{\code{state}}{a factor with levels for each state}
    \item{\code{expend}}{a numeric vector}
    \item{\code{ratio}}{a numeric vector}
    \item{\code{salary}}{a numeric vector}
    \item{\code{frac}}{a numeric vector}
    \item{\code{verbal}}{SAT verbal score}
    \item{\code{math}}{SAT mathematics score}
    \item{\code{sat}}{total SAT score}
  }
}
\source{
TBA
}
\references{
TBA
}
\examples{
data(SAT)
xyplot(sat ~ expend, SAT)
xyplot(sat ~ expend, SAT, 
	panel=function(x,y){grid.text(abbreviate(SAT$state,3),x,y,default.units='native')})
}
\keyword{datasets}
