
mean <- function(x, ...) UseMethod('mean')

mean.formula <- function( x, data, na.rm=TRUE, ... ) {
	aggregate( x, data, FUN=base::mean, na.rm=na.rm, ... )
}

mean.default <- function( x, na.rm=TRUE, ... ) {
	base::mean.default(x, na.rm=na.rm, ...)
}


sd <- function(x, ...) UseMethod('sd')

sd.default <- function(x, na.rm=TRUE, ... ) {
	stats::sd(x, na.rm=na.rm)
}

sd.formula <- function( x, data, na.rm=TRUE, ... ) {
	aggregate( x, data, FUN=stats::sd, na.rm=na.rm)
}

var <- function(x, ...) UseMethod('var')

var.default <- function(x, na.rm=TRUE, ...) {
	stats::var(x, na.rm=na.rm)
}

var.formula <- function( x, data, na.rm=TRUE, ... ) {
	aggregate( x, data, FUN=stats::var, na.rm=TRUE)
}
