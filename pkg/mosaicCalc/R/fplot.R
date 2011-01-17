
fplot <- function(f, xlim, ylim, n=100, args=list(), type='l', xlab, ylab, 
	... ) 
{
	makeFunction <- function(f) {
		if ( is.function(f) ) {
			return(f)
		}
		if ( is.numeric(f) ) {
			return ( function(x) { f } ) 
		} 
		if (is.character(f)) {
			return ( function(x) { do.call( f, args=list(x) ) } )
		}
		stop( "Unable convert to a function." )
	}

	if (! is.list(f) ) { 
	 	fList = list(f)
	}

	if (missing(ylab)) { 
		if ( is.character(fList[[1]]) ) {
			ylab <- fList[[1]]
		} else {
			ylab='function value' 
		}
	}

	fList <- lapply(fList, makeFunction) 

	if ( !all( unlist( lapply(fList, is.function) ) ) ) {
		stop('f must be a function or list of functions') 
	}
	if (missing(xlab)) {
		xlab=names(formals(fList[[1]]))[1]
	}

	if (missing(xlim)) {
		if ( is.finite( do.call(fList[[1]],args=c(list(0),args)) ) ) {
			xlim <- c(-2,2)
		} else {
			xlim <- c(0,2)
		}
	}
	ddd <- data.frame(x=numeric(0), y=numeric(0), group=character(0))

	id <- 0
	for (f in fList) {
		id <- id+1
		x <- adapt_seq(xlim[1], xlim[2], f=f, args=args, length.out=n)
		x <- unique(x)
		y <- do.call(f, args=c(list(x), args))
		ddd <- rbind(ddd, 
				data.frame(x=x, y=y, group= rep(as.character(id), length(x)))
				)
	}

	xyplot(y ~ x , ddd,
		groups = group, 
		type=type, 
		ylim=ylim, 
		xlab=xlab, ylab=ylab, 
		...)
}
