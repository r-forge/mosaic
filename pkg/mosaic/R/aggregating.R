 
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

# basic simple stat functions, e.g., mean, IQR, sd, var, median
.stat.fun.maker = function(fun,methodname){
  function(x, data=NULL, ...) {
    if( is.name( substitute(x) ) )
      fun(eval( substitute(x), data, enclos=parent.frame()), ...)
    else {
      if( "formula" == class(x)  && length(x)==2 ) {
         # It's a formula with no left-hand side
         fun( eval( x[[2]], data, enclos=parent.frame()), ...)
      }
      else UseMethod(methodname)
    }
  }
}

mean   <- .stat.fun.maker( mean, "mean" )
sd     <- .stat.fun.maker( stats::sd, "sd" )
var    <- .stat.fun.maker( var, "var" )
median <- .stat.fun.maker( median, "median" )
IQR    <- .stat.fun.maker( IQR, "IQR" )
prop   <- .stat.fun.maker( prop, "prop" )
count  <- .stat.fun.maker( count, "count" )
min    <- .stat.fun.maker( min, "min" )
max    <- .stat.fun.maker( max, "max" )

.stat.fun.formula.maker <- function(FUN,resname) {
  function( x, data=parent.frame(), na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=FUN, na.rm=na.rm, ... )
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- resname
	return(result)
  }
}

mean.formula    <- .stat.fun.formula.maker( base::mean,    "mean" )
sd.formula      <- .stat.fun.formula.maker( stats::sd,     "sd" )
var.formula     <- .stat.fun.formula.maker( stats::var,    "var" )
median.formula  <- .stat.fun.formula.maker( stats::median, "median" )
IQR.formula     <- .stat.fun.formula.maker( stats::IQR,    "IQR" )
count.formula   <- .stat.fun.formula.maker( count.default, "count" )
prop.formula    <- .stat.fun.formula.maker( prop.default,  "prop" )
min.formula     <- .stat.fun.formula.maker( base::min,     "min" )
max.formula     <- .stat.fun.formula.maker( base::max,     "max" )

mean.default   <- function( x, na.rm=TRUE, ... ) c(mean = base::mean.default(x, na.rm=na.rm, ...))
sd.default     <- function( x, na.rm=TRUE, ... ) stats::sd(x, na.rm=na.rm)
var.default    <- function( x, na.rm=TRUE, ... ) stats::var(x, na.rm=na.rm)
median.default <- function( x, na.rm=TRUE, ... ) stats::median.default(x, na.rm=na.rm)
IQR.default    <- function( x, na.rm=TRUE, ... ) stats::IQR( x, na.rm=na.rm, ...)
count.default  <- function( x, na.rm=TRUE, ... ) count.factor( as.factor(x), na.rm=na.rm, ...)
prop.default   <- function( x, na.rm=TRUE, ... ) prop.factor( as.factor(x), ...)
min.default    <- function( x, na.rm=TRUE, ... ) base::min( x, na.rm=na.rm, ...)
max.default    <- function( x, na.rm=TRUE, ... ) base::max( x, na.rm=na.rm, ...)

.stat.fun.factor.bogus.maker = function(statname) {
  function( x, na.rm=TRUE, ...) {
    stop( paste("Can't take",statname,"of a factor (categorical).  Use, e.g., count( ) or prop( ).") )
  }
}

mean.factor   <- .stat.fun.factor.bogus.maker("mean")
median.factor <- .stat.fun.factor.bogus.maker("median")
sd.factor     <- .stat.fun.factor.bogus.maker("sd")
var.factor    <- .stat.fun.factor.bogus.maker("var")
IQR.factor    <- .stat.fun.factor.bogus.maker("IQR")
min.factor    <- .stat.fun.factor.bogus.maker("min")
max.factor    <- .stat.fun.factor.bogus.maker("max")

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

