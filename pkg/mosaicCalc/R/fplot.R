
fplot <- function(f, xlim, ylim, n=100, args=list(), type='l', xlab, ylab, 
	... ) 
{
	if (! is.function(f) ) { 
		stop('f must be a function') 
	}
	if (missing(xlab)) {
		xlab=names(formals(f))[1]
	}
	if (missing(ylab)) {
		ylab=as.character(substitute(f))
	}

	if (missing(xlim)) {
		if ( is.finite( do.call(f,args=c(list(0),args)) ) ) {
			xlim <- c(-2,2)
		} else {
			xlim <- c(0,2)
		}
	}

	x <- adapt_seq(xlim[1], xlim[2], f=f, args=args, length.out=n)
	y <- do.call(f, args=c(list(x), args))
	xyplot(y ~ x, 
		type=type, 
		ylim=ylim, 
		xlab=xlab, ylab=ylab, 
		...)
}
