##############################################################
# The repeater
# - DTK, May 22, 2008, based on repeattrials.
##############################################################
# .as_repeater = function(n=1){ 
#   foo = list(n=n)
#   class(foo) = 'repeater'
#   return(foo)
# }


.clean_names <- function(x) {
	x <- gsub('\\(Intercept\\)','Intercept', x)
	x <- gsub('resample\\(','', x)
	x <- gsub('sample\\(','', x)
	x <- gsub('shuffle\\(','', x)
	x <- gsub('\\(','.', x)
	x <- gsub('\\)','', x)
	return(x)
}

setClass('repeater', 
	representation(n='numeric', cull='ANY', mode='ANY')
)

do = function(n=1L, cull=NULL, mode=NULL) {
	new( 'repeater', n=n, cull=cull, mode=mode )
}

# squash names of a data frame into a single string
.squash_names <- function(object,sep=":") {
	if ( ncol(object) < 1 ) {return(rep("",nrow(object)))}

	result <- object[,1]
	if ( ncol(object) < 2 ) {return(result)}

	for (c in 2:ncol(object)) {
		result <- paste(result, object[,c], sep=sep)
	}
	return(result)
		
}

# handle objects like models to do the right thing
.cull_for_do = function(object) {

	if (any(class(object)=='aggregated.stat')) {
		result <- object
		res <- result[, ncol(result)]
#		res <- as.vector( matrix( stat, nr=1) )
#		class(res) <- c("flattened.stat", class(res))
		names(res) <- paste(
						attr(object,'stat.name'), 
						.squash_names(object[,-ncol(object),drop=FALSE]), sep=".")
		return(res)
	}
	if (any(class(object)=='lme')){ # for mixed effects models
		result <- object
		names(result) <- .clean_names(names(result))
		return( object$coef$fixed )
	}
	if (any(class(object)=='lm') ) {
		sobject <- summary(object)
		result <-  c( coef(object), sigma=sobject$sigma, "r-squared" = sobject$r.squared ) 
		names(result) <- .clean_names(names(result))
		return(result)
	}
	if (any(class(object)=='htest') ) {
		result <-  data.frame( 
					  statistic = object$statistic, 
		              parameter = object$parameter,
					  p.value = object$p.value,
					  conf.level = attr(object$conf.int,"conf.level"),
					  lower = object$conf.int[1],
					  upper = object$conf.int[2],
					  method = object$method,
					  alternative = object$alternative,
					  data = object$data.name
					  )
		if ( ! is.null( names(object$statistic) ) ) names(result)[1] <- names(object$statistic)
		return(result)
	}
	if (any(class(object)=='table') ) {
		nm <- names(object)
		result <-  as.vector(object)
		names(result) <- nm
		return(result)
	}
	if (any(class(object)=='cointoss')) {
		return( c(n=attr(object,'n'), 
				heads=sum(attr(object,'sequence')=='H'),
				tails=sum(attr(object,'sequence')=='T')
				) )
	}
	if (is.matrix(object) && ncol(object) == 1) {
		nn <- rownames(object)
		object <- as.vector(object)
		if (is.null(nn)) {
			names(object) <- paste('v',1:length(object),sep="")
		} else {
			names(object) <- nn
		}
		return(object)
	}
	return(object) }

setMethod("print",
    signature(x = "repeater"),
    function (x, ...) 
    {
  		print(paste('This repeats a command',x@n,'times. Use with *.'))
  		invisible(x)
    }
)

setMethod("*",
    signature(e1 = "repeater", e2="ANY"),
    function (e1, e2) 
    {
		fthing = substitute(e2)
		if ( ! is.function(e2) ) {
			e2 = function(){eval.parent(fthing, n=2) }   
		}
		n = e1@n

		cull = e1@cull
		if (is.null(cull)) {
			cull <- .cull_for_do
		}

		res1 = cull(e2())  # was (...)

		nm = names(res1)

		if (!is.null(e1@mode)) { 
			out.mode <- e1@mode 
		} else {
			out.mode <- 'list'

			if ( is.vector( res1) || is.data.frame(res1) ) {
				if (is.null(nm)) { 
					out.mode <- 'matrix' 
				} else {
					out.mode <- 'data.frame'
				}
			}
		}


		if ( out.mode == 'list' ) {
			result <- list()
			result[[1]] <- res1
			if (n < 2) return (res1) 
			for (k in 2:n) {
				result[[k]] <- cull(e2()) # was (...)
			}
			return(result)
		}

		if (out.mode == 'data.frame') {
			if ( is.vector (res1) ) {
				result <- as.data.frame( matrix(res1, nr=1) )
			} else {
				result <- as.data.frame(res1)
			}
			if (n>1) {
			  for (k in 2:n) {
				result <- rbind( result, cull(e2()) ) 
			  }
			}
			rownames(result) <- 1:n
			names(result) <- nm
			return(result)
		}

		result <- matrix(nrow=n,ncol=length(res1))
		result[1,] <- res1

		if (n > 1) {
			for (k in 2:n) {
				result[k,] <- cull(e2()) # was (...)
			}
		}

		if (dim(result)[2] == 1 & is.null(nm) ) return(data.frame(result=result[,1])) else return(result)
	}
)
