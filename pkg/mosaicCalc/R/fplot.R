
formulaTerm <- function(t) {
	if (inherits(t,"name")) { return (as.name(t)) }
	if (inherits(t,"call")) { return (as.call(t)) }
	return(t)
}
mosaicParseFormula <- function ( model ) {

	if ( length(model) == 1 ) {
		return(model)
	}

	result <- list()

	if (length(model) == 2 ) {
		stop("Haven't done models of length 2 yet")
	}

	if ( length(model) == 3 ) {
		result$op <- model[[1]]
		result$left <- formulaTerm(model[[2]])
		result$right <- formulaTerm(model[[3]])
		class(result) <- 'mosaicParsedFormula'
		return(result)
	}

	stop("shouldn't get here.")
}

print.mosaicParsedFormula <- function(model) {
	print("I'm a parsed formula.")
	print(lapply(model, as.character))
}


, dimension = 2)
{
    expr2char <- function(x) paste(deparse(x), collapse = "")
    parseSide <- function(model) {
        model.vars <- list()
        while (length(model) == 3 && model[[1]] == as.name("+")) {
            model.vars <- c(model.vars, model[[3]])
            model <- model[[2]]
        }
        rev(c(model.vars, model))
    }
    parseCond <- function(model) {
        model <- substitute(~m, list(m = model))[[2]]
        model.vars <- list()
        while (length(model) == 3 && (model[[1]] == as.name("*") || 
            model[[1]] == as.name("+"))) {
            model.vars <- c(model.vars, model[[3]])
            model <- model[[2]]
        }
        rev(c(model.vars, model))
    }
    lrep <- function(x, n) {
        save.attr <- attributes(x)
        x <- rep(x, n)
        attributes(x) <- save.attr
        x
    }
    concat <- function(arglist) {
        if (length(arglist) == 1) 
            arglist[[1]]
        else if (any(sapply(arglist, is.factor))) {
            factor(unlist(lapply(arglist, as.character)))
        }
        else if (any(sapply(arglist, is.shingle))) {
            stop("shingles can not be concatenated")
        }
        else do.call("c", arglist)
    }
    if (!inherits(model, "formula")) 
        stop("model must be a formula object")
    if (multiple && !outer && !is.null(groups)) {
        multiple <- FALSE
        warning("'multiple=TRUE' ignored ('groups' non-null with 'outer=FALSE')")
    }
    ans <- if (dimension == 2) {
        list(left = NULL, right = NULL, condition = NULL, left.name = character(0), 
            right.name = character(0))
    }
    else if (dimension == 3) {
        list(left = NULL, right.x = NULL, right.y = NULL, condition = NULL, 
            left.name = character(0), right.x.name = character(0), 
            right.y.name = character(0))
    }
    else stop(gettextf("invalid dimension '%s'", as.character(dimension)))
    if (length(model) == 3) {
        if (multiple) {
            varsLHS <- parseSide(model[[2]])
            nLHS <- length(varsLHS)
        }
        else {
            varsLHS <- list(model[[2]])
            nLHS <- 1
        }
    }
    else {
        nLHS <- 1
    }
    modelRHS <- model[[length(model)]]
    if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|")) 
        modelRHS <- modelRHS[[2]]
    env <- environment(model)
    modelRHS <- model[[length(model)]]
    if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|")) {
        modelRHS.vars <- parseCond(modelRHS[[3]])
        modelRHS <- modelRHS[[2]]
        if (multiple && dimension == 2) {
            varsRHS <- parseSide(modelRHS)
            nRHS <- length(varsRHS)
        }
        else {
            varsRHS <- list(modelRHS)
            nRHS <- 1
        }
        ans$condition <- vector("list", length(modelRHS.vars))
        names(ans$condition) <- sapply(modelRHS.vars, expr2char)
##        for (i in seq_along(modelRHS.vars)) {
##            ans$condition[[i]] <- lrep(as.factorOrShingle(eval(modelRHS.vars[[i]], 
##                data, env), subset, drop = drop.unused.cond), 
##                nLHS * nRHS)
##        }
    }
    else if (multiple && dimension == 2) {
        varsRHS <- parseSide(modelRHS)
        nRHS <- length(varsRHS)
    }
    else {
        varsRHS <- list(modelRHS)
        nRHS <- 1
    }
    if (length(model) == 3) {
        ans$left.name <- expr2char(model[[2]])
##        ans$left <- lrep(concat(lapply(varsLHS, function(i) {
##            tmp <- eval(i, data, env)
##            if (!is.matrix(tmp)) 
##                tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
##                  tmp[subset, drop = drop.unused.data]
##                else tmp[subset]
##            if (inherits(tmp, "POSIXt")) 
##                tmp <- as.POSIXct(tmp)
##            tmp
##        })), nRHS)
    }
    if (dimension == 2) {
##        tmp <- eval(varsRHS[[1]], data, env)
##        if (is.matrix(tmp)) 
##            tmp <- as.data.frame(tmp)
##        nobs <- if (is.data.frame(tmp)) 
##            nrow(tmp)
##        else length(tmp)
##        if (nLHS == 1 && nRHS == 1) {
##            if (is.data.frame(tmp)) 
##                ans$right <- tmp[subset, ]
##            else ans$right <- if (is.factor(tmp) || is.shingle(tmp)) 
##                tmp[subset, drop = drop.unused.data]
##            else tmp[subset]
##        }
##        else {
##            ans$right <- concat(lapply(varsRHS, function(i) {
##                tmp <- eval(i, data, env)
##                tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
##                  tmp[subset, drop = drop.unused.data]
##                else tmp[subset]
##                tmp <- lrep(tmp, nLHS)
##                if (inherits(tmp, "POSIXt")) 
##                  tmp <- as.POSIXct(tmp)
##                tmp
##            }))
##        }
        ans$right.name <- expr2char(modelRHS)
        nRows <- length(ans$right)/(nLHS * nRHS)
    } 
	else if (dimension == 3 && length(modelRHS) == 3 && (modelRHS[[1]] == 
        "*" || modelRHS[[1]] == "+")) {
##        tmp <- eval(modelRHS[[2]], data, env)
##        nobs <- length(tmp)
##        if (!is.matrix(tmp)) 
##            tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
##                tmp[subset, drop = drop.unused.data]
##            else tmp[subset]
##        ans$right.x <- lrep(tmp, nLHS)
##        if (inherits(ans$right.x, "POSIXt")) 
##            ans$right.x <- as.POSIXct(ans$right.x)
##        tmp <- eval(modelRHS[[3]], data, env)
##        if (!is.matrix(tmp)) 
##            tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
##                tmp[subset, drop = drop.unused.data]
##            else tmp[subset]
##        ans$right.y <- lrep(tmp, nLHS)
##        if (inherits(ans$right.y, "POSIXt")) 
##            ans$right.y <- as.POSIXct(ans$right.y)
##        ans$right.x.name <- expr2char(modelRHS[[2]])
##        ans$right.y.name <- expr2char(modelRHS[[3]])
##        nRows <- length(ans$right.x)/nLHS
    }
    else stop("invalid model")
    if (nLHS > 1) 
        LHSgroups <- rep(gl(nLHS, nRows, labels = sapply(varsLHS, expr2char)), nRHS)
    if (nRHS > 1) 
        RHSgroups <- gl(nRHS, nRows * nLHS, labels = sapply(varsRHS, expr2char))
    newFactor <- if (nLHS > 1 && nRHS > 1) {
        interaction2(LHSgroups, RHSgroups, sep = lattice.getOption("interaction.sep"))
    }
    else if (nLHS > 1) 
        LHSgroups
    else if (nRHS > 1) 
        RHSgroups
    else NULL
    if (nLHS == 1 && nRHS == 1) {
        if (!is.null(groups)) 
            ans$groups <- groups
        if (subscripts) 
            ans$subscr <- seq_len(nobs)[subset]
    }
    else if (outer) {
        if (!is.null(groups)) 
            ans$groups <- rep(groups, nLHS * nRHS)
        if (!is.null(newFactor)) {
            if (is.null(ans$cond)) 
                ans$condition <- list(newFactor)
            else ans$condition[[length(ans$condition) + 1]] <- newFactor
        }
        else stop("newFactor cannot be NULL; you have found a bug!")
        if (subscripts) 
            ans$subscr <- as.vector(matrix(seq_len(nobs * nLHS * 
                nRHS), nrow = nobs)[subset, ])
    }
    else {
        if (is.null(groups) && !is.null(newFactor)) 
            ans$groups <- newFactor
        else stop("newFactor != NULL && groups == NULL does not hold; you have found a bug!")
        if (subscripts) 
            ans$subscr <- seq_len(length(newFactor))
        if (length(newFactor) != nRows * nLHS * nRHS) 
            stop("Length check mismatch; you have found a bug!")
    }
    ans
}

fplot <- function(x, ...) { UseMethod('fplot') }

fplot.formula <- function( 
	formula, xlim, ylim, 
	n=100, args=list(), 
	type='l', 
	xlab, ylab, 
	...) {

	parsed <- latticeParseFormula(formula)

	f <- function( x ) { x }

}

fplot.default <- function(f, xlim, ylim, n=100, args=list(), type='l', xlab, ylab, 
	... ) 
{
	if (is.character(f)) {
		temp_ylab = f
	} else {
		temp_ylab = deparse(substitute(f))
	}

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
	 	fList <- list(f)
	} else {
		fList <- f
	}
	f <- fList[[1]]

	if (missing(ylab)) { 
		if (length(fList) > 1) {
			ylab='function value' 
		} else {
			ylab = temp_ylab
#	    	ylab = paste(deparse(substitute(f)), "", sep="")
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
