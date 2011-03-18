
rgeo <- function( n, long=c(-180,180), lat=c(-90,90), verbose=FALSE ) {

	# oversample pts in a cube

	m <- 10 + 3*n
	x <- runif(m,-1,1)
	y <- runif(m,-1,1)
	z <- runif(m,-1,1)

	# select pts inside unit sphere and project them onto surface

	r <- sqrt( x^2 + y^2 + z^2 )
	ids <- which( r < 1 ) [1:n]
	x <- x[ids]
	y <- y[ids]
	z <- z[ids]
	r <- r[ids]
	x <- x/r
	y <- y/r
	z <- z/r

	# now convert this to longitude and latitude

	lat <- asin(z) * 180 / pi
	long1 <- asin(y) * 180 / pi
	long <- (x < 0) * 180 + 1 - 2*(y>0) * long1
	if (verbose) {
		return(data.frame(long=long, lat=lat, x=x, y=y, z=z))
	}
	return(data.frame(long=long, lat=lat))
}
