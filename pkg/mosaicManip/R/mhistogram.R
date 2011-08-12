
mhistogram <- function(..., n=12, nint=n) {
	if ( require(manipulate) ) {
		manipulate(
			histogram( ..., nint = N ),
			N = slider( 3, 50, initial=nint )
		)
	}
	else {
		histogram(..., nint=nint)
	}
}
