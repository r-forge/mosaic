\name{rgeo}
\alias{rgeo}
\title{Sample longitude and latitude on earth
}
\description{
Randomly sample longitude and latitude on earth so that equal areas are (approximately) equally likely to 
be sampled.  (Approximation assumes earth as a perfect sphere.)
}
\usage{
rgeo(n, long = c(-180, 180), lat = c(-90, 90), verbose=FALSE)
}
\arguments{
  \item{n}{
  number of random locations
}
  \item{long}{ 
  range of longitudes to sample within, not yet implemented 
}
  \item{lat}{
  range of latitudes to sample within, not yet implemented 
}
  \item{verbose}{
  return verbose output that includes Euclidean coordinates on unit sphere as well as longitude and lattitude.
}
}
\value{
A data frame with variables \code{long} and \code{lat}
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}

\examples{
rgeo(4)
}
\keyword{ random }
