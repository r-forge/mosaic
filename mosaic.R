#
# this builds elements of the mosaic R package and uses package.skeleton()
# to put files in place.
# hand edited versions of these files should be maintained in the parallel
# hand-edited directory to avoid collisions.
# 
setwd('pkg')

D <- function(f,n=1,h=0.00005) {
	n = as.integer(n)
	if (! (is.integer(n) && n > 0) ) {
		warning('n must be a non-negative integer.')
	}
	if (n==0) { return (f) }
	if (n==1) { return ( function(x) { ( f(x+h) - f(x) ) / h } ) }
	return( D(f,n=n-1,h=h) )
}

package.skeleton('mosaic')

system('mosaic_shift.sh')

