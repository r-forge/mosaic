##############################################################
# The repeater
# - DTK, May 22, 2008, based on repeattrials.
##############################################################
# .as_repeater = function(n=1){ 
#   foo = list(n=n)
#   class(foo) = 'repeater'
#   return(foo)
# }

do = function(n=1, mode=NULL) {
  foo = list(n=n, mode=mode)
  class(foo) = 'repeater'
  return(foo)
}

#five = do(5)
#ten = do(10)
#dozen = do(12)
#hundred = do(100)
#thousand = do(1000)


# handle objects like models to do the right thing
.cull_for_do = function(object) {
	if (any(class(object)=='lme')){ # for mixed effects models
		return( object$coef$fixed )
	}
	if (any(class(object)=='lm') ) {
		return( coef(object) )
	}
	return(object)
}


.do_repeats= function(a,f){
	fthing = substitute(f)
	n = a$n
	if (class(f) != 'function') {
		f = function(){eval.parent(fthing, n=2) }
	}
	res1 = .cull_for_do(f());  # was (...)
	if (n < 2) { return (res1) }

	nm = names(res1);

	if (!is.null(a$mode)) { 
		out.mode <- a$mode 
	} else {
		out.mode <- 'list'

		if ( is.vector( res1) ) {
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
		for (k in 2:n) {
			result[[k]] <- .cull_for_do(f()); # was (...)
		}
		return(result)
	}

	result <- matrix(nrow=n,ncol=length(res1));
	if ( out.mode == 'data.frame' ){
		result = data.frame(result);
		names(result) = names(res1);
	}
	result[1,] = res1;

	for (k in 2:n) {
		result[k,] <- .cull_for_do(f()); # was (...)
	}

	if (dim(result)[2] == 1 & is.null(nm) ) return(c(result)) else return(result);
}

"*.repeater" = .do_repeats

print.repeater = function(x, ...) {
  print(paste('This repeats a command',x$n,'times. Use with *.'))
  invisible(x)
}
