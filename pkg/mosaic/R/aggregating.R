 
.mosaic_aggregate <- function(x, data, FUN, overall=TRUE, ...) {
	if (length(x) == 2 ) {
		return( data.frame( FUN (eval( x[[2]], data) ) ) )
	} else {
		return( as.data.frame( 
			Hmisc::summary.formula( x, data, fun=FUN, overall=overall, method='cross',...) ) )
	}
	result <- summary(x, data, fun=FUN, overall=overall, method=method, ...)
	result <- as.data.frame(oldUnclass(result))
	return(result)
}


mean <- function(x, ...) UseMethod('mean')

# do similar thing to other functions later
# isolate reusable elements to make maintenance easier...

mean.formula <- function( x, data=parent.frame(), na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=base::mean, na.rm=na.rm, ... )
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- 'mean'
	return(mean=result)
}

mean.default <- function( x, na.rm=TRUE, ... ) {
	c(mean = base::mean.default(x, na.rm=na.rm, ...))
}


sd <- function(x, ...) UseMethod('sd')

sd.default <- function(x, na.rm=TRUE, ... ) {
	stats::sd(x, na.rm=na.rm)
}

sd.formula <- function( x, data, na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=stats::sd, na.rm=na.rm)
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- 'sd'
	return(result)
}

var <- function(x, ...) UseMethod('var')

var.default <- function(x, na.rm=TRUE, ...) {
	stats::var(x, na.rm=na.rm)
}

var.formula <- function( x, data, na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm)
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- 'var'
	return(result)
}

median <- function(x, ...) UseMethod('median')

median.default <- function(x, na.rm=TRUE, ...) {
	stats::median.default(x, na.rm=na.rm)
}

median.formula <- function( x, data, na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=stats::median.default, na.rm=na.rm)
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- 'median'
	return(result)
}

count <- function(x, ...) { UseMethod('count') }

count.logical <- function(x, level=TRUE, na.rm=TRUE, ...) 
	count.factor( as.factor(x), level=level, na.rm=na.rm ) 

count.factor <- function(x, level=levels(x)[1], na.rm=TRUE, ...) {
	if (! level %in% levels(x) ) {
		level = levels(x) [as.numeric(level)]
	}
	result <- sum( x == level, na.rm=na.rm ) 
	names(result) <- paste('count', level, sep=".")
	return(result)
}

count.formula <- function( x, data, na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=count, na.rm=na.rm)
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- 'count'
	return(result)
}

count.default <- function(x, na.rm=TRUE, ...) {
	count.factor( as.factor(x), ...)
}

prop <- function(x, ...) { UseMethod('prop') }

prop.logical <- function(x, level=TRUE, na.rm=TRUE, ...) 
	prop.factor( as.factor(x), level=level, na.rm=na.rm )

prop.factor <- function(x, level=levels(x)[1], na.rm=TRUE, ...) {
	xx <- substitute(x)
	x.string <- tail(as.character(xx),1)
	if (! level %in% levels(x) ) {
		level = levels(x) [as.numeric(level)]
	}
	result <- mean( x == level, na.rm=na.rm ) 
	names(result) <- paste('prop', level, sep=".")
	return(result)
}

prop.formula <- function( x, data, na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=prop, na.rm=na.rm)
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- 'prop'
	return(result)
}

prop.default <- function(x, na.rm=TRUE, ...) {
	prop.factor( as.factor(x), ...)
}
