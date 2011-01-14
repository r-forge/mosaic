##############################################
# aliases
#
deal    <- function(x, size, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE) {
	sample(x, size, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids )
}
resample <- function(x, size, replace=TRUE, prob=NULL, groups=NULL, orig.ids=FALSE) {
	sample(x, size, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids )
}
shuffle <- function(x, replace=TRUE, prob=NULL, groups=NULL, orig.ids=FALSE) 
{
	if (!is.null(groups)){
		return( .shuffle_within(x, groups=groups, replace=replace) )
	}
	return( sample(x, replace=replace, prob=prob) )
}


##############################################
# override base::sample with something fancier
#
sample <- function (x, size, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE) { 
	UseMethod('sample') 
}

.shuffle_within = function( x, groups=NULL, replace=FALSE, prob=NULL, orig.ids=FALSE ){
	if (is.null(groups)) {
		stop("Must specify groups to resample within.")
	}
	# force groups to have the right size, recycling as needed.
	if (is.null(dim(x))) {
		groups <- rep(groups, length.out=length(x))
	} else {
		groups <- rep(groups, length.out=nrow(x))
	}
	groups = as.factor(groups)
	flag = FALSE
	levs = levels(groups);
	for (lev in levs) { # k in 1:length(levs) ) {
		ids = which( groups==lev )
		if (length(ids)==1 ) { flag = TRUE }
		rids = sample(ids, replace=replace, orig.ids=orig.ids) 
		if( is.null(dim(x))) {
			x[ ids] = x[rids]}
		else {
			if( is.data.frame(x) | is.matrix(x) ) {
				x[ids,] = x[rids,]
			} else {
				x[ids] = x[rids]
			}
		}
	}
	if (flag) {
		warning("One or more groups had only 1 member. Can't shuffle that group.")
	}
	return(x)
}



sample.default <- function(x, size, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE) { 
	if (! is.null(groups) ) {
		return(.shuffle_within(x, replace=replace, prob=prob, groups=groups, 
			orig.ids=orig.ids))
	}

	result <- base::sample(x, size, replace=replace, prob=prob) 
	return(result)
}


sample.data.frame <- function(x, size, replace = FALSE, prob = NULL, groups=NULL, orig.ids=TRUE) {
	if (! is.null(groups) ) {
		return(
			.shuffle_within(x, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids)
		)
	}
	n <- nrow(x)
		ids <- base::sample(n, size, replace=replace, prob=prob)
		data <-  x [ ids, , drop=FALSE] 
		names(data) <- names(x)
		if (orig.ids) {
			data$orig.ids <- ids
		}
		if (length(ids) < 50) { return(data) } else {invisible(data)}
}

sample.matrix <- function(x, size, replace = FALSE, prob = NULL, groups=NULL, orig.ids=FALSE) {
	if (! is.null(groups) ) {
		return(
			.shuffle_within(x, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids)
		)
	}
	n <- nrow(x)
		ids <- base::sample(n, size, replace=replace, prob=prob)
		data <-  x [ ids, , drop=FALSE] 
		names(data) <- names(x)
		if (orig.ids) {
			attr(data,'orig.row') <- ids
		}
		if (length(ids) < 50) { return(data) } else {invisible(data)}
}


