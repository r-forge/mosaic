 
	#overall=mosaic.par.get("aggregate.overall"), ...) {
.mosaic_aggregate <- function(x, data, FUN, overall=TRUE, ...) {
	if (length(x) == 2 ) {
		return( data.frame( FUN (eval( x[[2]], data, enclose=parent.frame()) ) ) )
	} else {
		return( as.data.frame( 
			Hmisc::summary.formula( x, data, fun=FUN, overall=overall, method='cross',...) ) )
	}
	result <- summary(x, data, fun=FUN, overall=overall, method=method, ...)
	result <- as.data.frame(oldUnclass(result))
	return(result)
}

# check for formula with no left-hand side or a simple right-hand side, e.g. NULL, ., 1, or 0
.is.simple.formula <-  function(x){
     "formula" == class(x)  &&
         (length(x)==2 || is.null(x[[3]]) ||
          (length(x[[3]])==1 &&
          ((is.numeric(x[[3]]) && (x[[3]]==0 || x[[3]]==1)) ||  (all.names(x[[3]]) %in% c(".")))))
}

.simple.part <- function(x) {
	if (! .is.simple.formula(x) ) {
		return(NULL) 
	} else {
		return(x[[2]])
	}
}


.flatten <- function(x) {
    .x <- c()
  for (item in x) .x <- c(.x,item)
  return(.x)
}


##########################################################################################

setGeneric( 
	"mean", 
	function(x, ..., na.rm=TRUE, trim=0)  {
		dots <- list(...)
		if ( is.name(substitute(x)) ) {
			if ( length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
				data <- dots[[1]]
				return(base::mean(eval( substitute(x), data),  ..., na.rm=na.rm, trim=trim))
			}
		}
		standardGeneric('mean')
	}
)

setMethod(
	'mean',
	'numeric',
	function(x, ..., na.rm=TRUE, trim=0) 
		c( mean=base::mean( c(x,.flatten(list(...))), na.rm=na.rm, trim=trim ) )
	
)

setMethod( 
	"mean", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE, trim=0) 
		base::mean(x=x, ..., na.rm=na.rm, trim=trim)
	
)

setMethod( 
	"mean", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE, trim=0) {
		if( .is.simple.formula(x) ) {
			return( mean( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., na.rm=na.rm, trim=trim ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::mean, ..., na.rm=na.rm, trim=trim ) )
		} 
	}
)

##########################################################################################


setGeneric( 
	"median", 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( is.name(substitute(x)) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(stats::median(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('median')
	}
)

setMethod(
	'median',
	'numeric',
	function(x, ..., na.rm=TRUE) 
		c( median=stats::median( c(x,.flatten(list(...))), na.rm=na.rm) )
	
)

setMethod( 
	"median", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) sapply( x, stats::median, na.rm=na.rm)
)

setMethod( 
	"median", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( median( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::median, na.rm=na.rm) )
		} 
	}
)

##########################################################################################

setGeneric( 
	"sd", 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( is.name(substitute(x)) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(stats::sd(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('sd')
	}
)

setMethod(
	'sd',
	'numeric',
	function(x, ..., na.rm=TRUE) 
		c( sd=stats::sd( c(x,.flatten(list(...))), na.rm=na.rm) )
	
)

setMethod( 
	"sd", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) sapply( x, stats::sd, na.rm=na.rm)
)

setMethod( 
	"sd", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( sd( eval( .simple.part(x), data, enclos=parent.frame()), ..., na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::sd, na.rm=na.rm) )
		} 
	}
)
##########################################################################################


.make.Maxlike.generic <- function( NAME="Max", FUN = stats::max ) {

	setGeneric( 
		NAME, 
		function(x, ..., na.rm=TRUE)  {
			dots <- list(...)
			if ( is.name(substitute(x)) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
				data <- dots[[1]]
				return(FUN(eval( substitute(x), data),  na.rm=na.rm))
			}
			standardGeneric(NAME)
		}
	)

	setMethod(
		NAME,
		'numeric',
		function(x, ..., na.rm=TRUE) {
			FUN(x, ..., na.rm=na.rm) 
		}
	)


	setMethod( 
		NAME, 
		signature=c("data.frame"),
		function(x, ..., na.rm=TRUE) {
			sapply( x, FUN, na.rm=na.rm)
		}
	)


	setMethod( 
		NAME, 
		signature=c("formula"),
		function(x, ..., na.rm=TRUE) {
			dots <- list(...)
			data  <- dots[[1]]

			if( .is.simple.formula(x) ) {
				return( FUN( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
			} else {
				return( .mosaic_aggregate( x, data, FUN=FUN, na.rm=na.rm) )
			} 
		}
	)
}

.make.Maxlike.generic( 'Max', base::max )
.make.Maxlike.generic( 'Min', base::min )

##########################################################################################



setGeneric( 
	"var", 
	function(x, y=NULL, na.rm=TRUE, use, data=NULL)  {
		if ( is.name(substitute(x)) && ! is.null(y) && is.name(substitute(y)) && is.data.frame( data ) ) {
			return( stats::var(eval( substitute(x), data), eval(substitute(y, data), na.rm=na.rm, use=use)) )
		}
		if ( is.name(substitute(x)) && is.data.frame( y ) ) {
			return(stats::var(eval( substitute(x), y),  na.rm=na.rm))
		}
		if ( is.name(substitute(x)) && is.null(y) && is.data.frame( data ) ) {
			return( stats::var( eval( substitute(x), data), na.rm=na.rm, use=use) )
		}
		standardGeneric('var')
	}
)

setMethod(
	'var',
	c('numeric','numeric'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		c( var=stats::var( x, y, na.rm=na.rm, use=use) )
)

setMethod(
	'var',
	c('numeric'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		c( var=stats::var( x, y, na.rm=na.rm, use=use) )
)

setMethod(
	'var',
	c('matrix'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use) 
)

setMethod( 
	"var", 
	signature=c("data.frame"),
	function(x, y, na.rm=TRUE, use=use) stats::var(x, y, na.rm=na.rm, use=use)
)

setMethod( 
	"var", 
	signature=c(x="formula", y="missing", na.rm='ANY', use='ANY', data="data.frame"),
	function(x, na.rm=TRUE, use, data=parent.frame()) {
		if( .is.simple.formula(x) ) {
			return( stats::var( eval( .simple.part(x), data ),  na.rm=na.rm, use=use ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm, use=use) )
		} 
	}
)

setMethod( 
	"var", 
	signature=c(x="formula", y="data.frame", na.rm='ANY', use='ANY', data="missing"),
	function(x, y=parent.frame(),  na.rm=TRUE, use) {
		data <- y
		if( .is.simple.formula(x) ) {
			return( stats::var( eval( .simple.part(x), data),  na.rm=na.rm, use=use ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm, use=use) )
		} 
	}
)
##########################################################################################
example(mean)
mean(1,2,3,4,15)
mean(1:4,15)
mean(USArrests)
mean( Sepal.Length ~ Species , data=iris )
mean(iris$Sepal.Length)
mean( ~ Sepal.Length, data=iris )
mean( Sepal.Length ~ 1, data=iris )
mean( Sepal.Length ~ 0, data=iris )
mean( Sepal.Length ~ ., data=iris )
mean( Sepal.Length, data=iris )
mean( ~ Sepal.Length + 3, data=iris)
# this doesn't work yet
# mean( Sepal.Length + 3, data=iris)

example(median)
median(1,2,3,4,15)
median(1:4,15)
median(USArrests)
median(iris$Sepal.Length)
median( Sepal.Length ~ Species , data=iris )
median( ~ Sepal.Length, data=iris )
median( Sepal.Length ~ 1, data=iris )
median( Sepal.Length ~ 0, data=iris )
median( Sepal.Length ~ ., data=iris )
median( Sepal.Length, data=iris )

example(sd)
sd(1,2,3,4,15)
sd(1:4,15)
sd(USArrests)
sd(iris$Sepal.Length)
sd( Sepal.Length ~ Species , data=iris )
sd( ~ Sepal.Length, data=iris )
sd( Sepal.Length ~ 1, data=iris )
sd( Sepal.Length ~ 0, data=iris )
sd( Sepal.Length ~ ., data=iris )
sd( Sepal.Length, data=iris )

example(var)
var(USArrests)
var(iris$Sepal.Length)
var( Sepal.Length ~ Species , data=iris )
var( Sepal.Length ~ Species , iris )
var( Sepal.Length, data=iris )
var( ~ Sepal.Length, iris )
var( ~ Sepal.Length, data=iris )
var( Sepal.Length ~ 1, data=iris )
var( Sepal.Length ~ 0, data=iris )
var( Sepal.Length ~ ., data=iris )
var( matrix(1:9, nr=3), matrix(1:9, nc=3) )

Max(1,2,3,4,15)
Max(1:4,15)
Max(USArrests)
Max(iris$Sepal.Length)
Max( Sepal.Length ~ Species , data=iris )
Max( ~ Sepal.Length, data=iris )
Max( Sepal.Length ~ 1, data=iris )
Max( Sepal.Length ~ 0, data=iris )
Max( Sepal.Length ~ ., data=iris )
Max( Sepal.Length, data=iris )

max <- Max

example(max)
max(1,2,3,4,15)
max(1:4,15)
base::max(USArrests)
max(USArrests)
max(iris$Sepal.Length)
max( ~ Sepal.Length, data=iris )
max( Sepal.Length ~ 1, data=iris )
max( Sepal.Length ~ 0, data=iris )
max( Sepal.Length ~ ., data=iris )
max( Sepal.Length, data=iris )
max( Sepal.Length ~ Species , data=iris )


a <- 0
b <- 5
min(a,b)
max(b,a)

