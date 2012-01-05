\name{rgeo}
\alias{rgeo}
\alias{rgeo2}
\alias{xyz2latlon}
\alias{latlon2xyz}
\alias{deg2rad}
\alias{rad2deg}

\title{Sample longitude and latitude on a sphere}

\description{
Randomly samples longitude and latitude on earth so that equal areas are
(approximately) equally likely to be sampled.  
(Approximation assumes earth as a perfect sphere.)

Some additional utility functions are documented here as well.
}

\usage{
rgeo(n=1, latlim=c(-90,90), lonlim=c(-180,180), verbose=FALSE ) 

rgeo2(n=1, latlim=c(-90,90), lonlim=c(-180,180), verbose=FALSE ) 

latlon2xyz(latitude,longitude) 

xyz2latlon(x,y,z)

rad2deg(x)

deg2rad(x)
}

\arguments{
  \item{n}{
  number of random locations
}
  \item{latlim, lonlim}{ 
  range of latitudes and longitudes to sample within, only implemented for \code{rgeo}.
}
  \item{verbose}{
  return verbose output that includes Euclidean coordinates on unit sphere as well as longitude and lattitude.
}
\item{x,y,z,latitude,longitude}{other arguments}
}
\value{
\code{rgeo()} returns a data frame with variables 
\code{long} and \code{lat}.  If \code{verbose} is
TRUE, then x, y, and z coordinates are included in the data frame.
}
\details{
\code{rgeo} and \code{rgeo2} differ in the algorithms used to generate random positions.  
Each assumes a spherical globe.  \code{rgeo} uses that fact that each of the x, y and z
coordinates is uniformly distributed (but not independent of each other).  Furthermore, the 
angle about the z-axis is uniformly distributed and independent of z.  This provides 
a straightforward way to generate Euclidean coordinates using \code{runif}.  These are then
translated into latitude and longitude.

\code{rgeo2} samples points in a cube by independently sampling each coordinate.  It then
discards any point outside the sphere contained in the cube and projects the non-discarded points
to the sphere.  This method must oversample to allow for the discarded points.
}

\author{
Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
\code{\link{googleMap}}
}
\examples{
rgeo(4)
# sample from a region that contains the continental US
rgeo( 4, latlim=c(25,50), lonlim=c(-65,-125) )
rad2deg(2*pi)
deg2rad(180)
latlon2xyz(45,45)
xyz2latlon(1,1,1)     # point may be on sphere of any radius
xyz2latlon(0,0,0)     # this produces a NaN for latitude
}
\keyword{ random }
\keyword{ geometry }
