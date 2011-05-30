
# these globals are yucky.  any way to avoid them?  (probably don't need them in lattice)
..currentAxisNames=c("", "")
..currentAxisLimits=list(c(0,1),c(0,1))

#==========
findZeros = function(expr, ..., xlim=NULL, npts=1000) {
    vals = list(...)
    ..f.. = createMathFun( sexpr=substitute(expr), ...)
    vars = formals(..f..$fun)
    # If there is a plot showing, and no independent 
    # variable was specified, get the axis names from the plot
    if (is.null(..f..$names) | length(..f..$vals[[..f..$names[1]]]) == 0 ) {
      if( ..currentAxisNames[1] == "" )
        stop("No plotting variable defined")
      else ..f..$names = ..currentAxisNames[1]
      if (is.null(xlim) & length(..f..$vals)==0 ) xlim = ..currentAxisLimits[[1]]
    }
    ndims = length(..f..$names)
    if( ndims != 1 ) stop("Only works for one unknown.")
    pfun = function(.x){
      vals[[..f..$names]] = .x
      eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
    }
    xlim2 = xlim
    if( is.null(xlim) & ..f..$names %in% names(vals) ) xlim2 = vals[[..f..$names]]
    
    mx = max(xlim2)
    mn = min(xlim2)
    if( length(xlim2) < 2 | mx==mn ) 
       stop("Must provide a finite range to search over.")
    # Deal with very large numbers for the interval, e.g. Inf
    verybig = .Machine$double.xmax
    plainbig = 10000 # linear spacing below this.
    mx = max(min(verybig,mx),-verybig)
    mn = min(max(-verybig,mn),verybig)
    rightseq = NULL
    leftseq = NULL
    middleseq = NULL
    if( mx > plainbig ) { 
      rightseq = exp( seq( log(max(plainbig,mn)),log(mx),length=npts) )
    }
    middleseq = seq( max(-plainbig,mn), min(plainbig,mx), length=npts)
    if( mn < -plainbig ){
      leftseq = -exp( seq( log(-mn), log(-min(-plainbig,mx)), length=npts))
    }
    searchx = unique(c(leftseq,middleseq, rightseq))
    # searchx = seq(min(xlim2), max(xlim2), length=npts)
    y = pfun(searchx)
    ys = sign(y)
    testinds = which(abs(diff(ys)) != 0)
    if (length(testinds) == 0 ) return(NULL)
    zeros = rep(NA, length(testinds) )
    for (k in 1:length(testinds) ) {
      where = testinds[k]
      zeros[k] = uniroot(function(qqzz){pfun(qqzz)}, 
        lower=searchx[where], upper=searchx[where+1])$root
    }
    return(zeros)
}
  
#==========
panel.plotFun = function(expr, ..., 
  xlim=NULL, ylim=NULL, 
	npts=200,
  filled=TRUE, nlevels=10,
	col, col.line = reference.line$col, 
	lwd = reference.line$lwd, 
  lty = reference.line$lty, 
  colorscheme=topo.colors, 
	type="l", 
	transparency=NULL ) 
{
    vals = list(...)

    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) {
        col.line <- col
		}
    
    ..f.. = createMathFun( sexpr=substitute(expr), ...)
    vars = formals(..f..$fun)
    if (length(..f..$names) == 0 ) {
      if( ..currentAxisNames[1] == "" )
        stop("No plotting variable defined")
      else ..f..$names = ..currentAxisNames[ ..currentAxisNames != ""]
    }
    ndims = length(..f..$names)
    if (ndims != 1 ) {
      stop("Must be exactly 1 plotting variable.")
		}

	# following line seems silly when we can put 200 as the default value
	# npts = ifelse( is.null(npts), 200, npts)

	# create a function of that one variable
	#   the "bogus" arg is to make things work with adapt_seq
	pfun = function(.x){
		vals[[..f..$names]] = .x
		
		# PUT AN assign HERE
		#assign(..f..$names, .x)
		eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
	}

	# figure out the limits.  
	# Is a limit specified, either through xlim or the variable name
	xlim2 = xlim
	if( ..f..$names %in% names(vals) ) {
			xlim2 = vals[[..f..$names]]
	}
	if( length(xlim2)<2 ) { 
		xlim2 = current.panel.limits()$xlim
	} 

	if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
			stop(paste("Must provide x-axis limit via ", 
				..f..$names, "= or xlim=", sep=""))
	}

	# Evaluate the function.
	if( require(mosaic) ) {
		.xset = adapt_seq(min(xlim2), max(xlim2), 
				 f=function(xxqq, bogus){pfun(xxqq)}, length=npts)
	} else {
		.xset = seq(min(xlim2), max(xlim2), length=npts)
	}
	
	.yset = pfun(.xset)
	if( length(.yset) != length(.xset) ){
		.yset == rep(0, length(.xset)) 
		for (k in 1:length(.xset) ) {
			.yset[k] = pfun(.xset[k]) # NULL for compatibility with adapt_seq
		}
	}
	return(panel.lines(.xset, .yset, lwd=lwd, col=col.line))

}

#==========
panel.levelFun = function(expr, 
	subscripts, 
	at,
	shrink, 
	labels = FALSE, 
  label.style = c("mixed", "flat", "align"), 
	contour = FALSE, 
  region = TRUE, 
	col = add.line$col, 
	lty = add.line$lty, 
	lwd = add.line$lwd, 
  border = "transparent", 
	..., 
	col.regions = regions$col, 
	alpha.regions = regions$alpha,
  xlim=NULL, 
	ylim=NULL ,
	npts=40
#	filled=TRUE, 
# nlevels=10,
#	type="l", 
) {

    vals = list(...)
		regions <- trellis.par.get('regions')
		add.line <- trellis.par.get('add.line')
    if (!missing(col) && missing(col.regions)) {
        col.regions <- col
		}
    
    ..f.. = createMathFun( sexpr=substitute(expr), ...)
    vars = formals(..f..$fun)
    if (length(..f..$names) == 0 ) {
      if( ..currentAxisNames[1] == "" )
        stop("No plotting variable defined")
      else ..f..$names = ..currentAxisNames[ ..currentAxisNames != ""]
    }
    ndims = length(..f..$names)
    if( ndims != 2 ){
			stop("Must specify exactly 2 plotting variables.")
		}

      # create a function of those two variables
      pfun = function(.x, .y){
        vals[[..f..$names[1]]] = .x
        vals[[..f..$names[2]]] = .y
        eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
      }
      xlim2 = xlim
      ylim2 = ylim
      if( ..f..$names[1] %in% names(vals) ) {
          xlim2 = vals[[..f..$names[1]]]
      }
      if( ..f..$names[2] %in% names(vals) ) {
        ylim2 = vals[[..f..$names[2]]]
      }
      if (length(xlim2)==0 | length(ylim2) == 0) {
        xlim2 = current.panel.limits()$xlim
        ylim2 = current.panel.limits()$ylim
      }
			
        
      if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
          stop(paste("Must provide x-axis limit via ", 
            ..f..$names, "= or xlim=", sep=""))
      }
        
      .xset = seq(min(xlim2), max(xlim2), length=npts)
      .yset = seq(min(ylim2), max(ylim2), length=npts)
     
      xvals = outer(.xset, .yset, function(x, y){x} )
      yvals = outer(.xset, .yset, function(x, y){y} )
      zvals = outer(.xset, .yset, function(x, y){pfun(x, y)} )
			if (missing(at)) { at = pretty(zvals) }
			zcol <- lattice::level.colors(zvals, at, col.regions, colors = TRUE)

			if(missing(subscripts)) { subscripts <- 1L:length(zvals) }

			return( panel.levelplot( xvals, yvals, zvals, 
				subscripts = subscripts, 
				at = at,
				shrink = shrink, 
				labels = labels, 
    		label.style = label.style,
				contour = contour,
    		region = region, 
				col = col,
			  lty = lty,
				lwd = lwd,
    		border = border,
				..., 
				col.regions = col.regions, 
				alpha.regions = alpha.regions) 
				)
}
# =============================
    


plotFun = function(expr, ..., add=FALSE,
  ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, 
  xlim=NULL, ylim=NULL, npts=NULL,
  lwd=1, col="black", filled=TRUE, nlevels=10,
  surface=FALSE,
  colorscheme=topo.colors, type="l", transparency=NULL ) { 
    vals = list(...)
    
    
    ..f.. = createMathFun( sexpr=substitute(expr), ...)
    vars = formals(..f..$fun)
    if (length(..f..$names) == 0 ) {
      if( ..currentAxisNames[1] == "" )
        stop("No plotting variable defined")
      else ..f..$names = ..currentAxisNames[ ..currentAxisNames != ""]
    }
    ndims = length(..f..$names)
    if( ndims == 1 ){
      npts = ifelse( is.null(npts), 200, npts)
      # create a function of that one variable
      #   the "bogus" arg is to make things work with adapt_seq
      pfun = function(.x){
        vals[[..f..$names]] = .x
        
        # PUT AN assign HERE
        #assign(..f..$names, .x)
        eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
      }
      # since plot will handle expressions nicely, let it do so.
      # but note that zlab in 3-d plots doesn't handle expressions
      # so it's necessary to deparse things there.
      if( length(ylab) == 0 ) ylab = ..f..$sexpr #deparse(..f..$sexpr)
      if( length(xlab) == 0 ) xlab = ..f..$names
      # figure out the limits.  
      # Is a limit specified, either through xlim or the variable name
      xlim2 = xlim
      if( ..f..$names %in% names(vals) ) {
          xlim2 = vals[[..f..$names]]
      }
      if( length(xlim2)<2 ) { # no limits were specified
        if( ..f..$names != ..currentAxisNames[1] )
          stop(paste("Dependent variable in add-on plot, ",
           ..f..$names, ", does not match existing plot variable ", 
           ..currentAxisNames[1], sep=""))
        else xlim2 = ..currentAxisLimits[[1]]
           
      } 
      if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
          stop(paste("Must provide x-axis limit via ", 
            ..f..$names, "= or xlim=", sep=""))
      }
      # Evaluate the function.
      if( require(mosaic) )
        .xset = adapt_seq(min(xlim2), max(xlim2), 
             f=function(xxqq, bogus){pfun(xxqq)}, length=npts)
      else
        .xset = seq(min(xlim2), max(xlim2), length=npts)
      
      .yset = pfun(.xset)
      if( length(.yset) != length(.xset) ){
        .yset == rep(0, length(.xset)) 
        for (k in 1:length(.xset) ) {
          .yset[k] = pfun(.xset[k]) # NULL for compatibility with adapt_seq
        }
      }
      if (add) {
        # Check to make sure the new plot will show up
        if( all(.yset > max(..currentAxisLimits[[2]])) |
            all(.yset < min(..currentAxisLimits[[2]])) )
            warning("New values are outside of the y-axis range.")
         
        lines(.xset, .yset, lwd=lwd, col=col)
      }
      else { # draw a new plot
        ..currentAxisLimits[[1]] <<- xlim2
        ..currentAxisNames <<- c(..f..$names, "")
        plot( .xset, .yset, type=type, 
         lwd=lwd, col=col, xlim=xlim, ylim=ylim,
         xlab=xlab, ylab=ylab, main=main)
        goo = par("usr") # get the limits of the plot
        ..currentAxisLimits[[2]] <<- goo[c(3,4)]
      }  
    }
    if (ndims == 2 ) {
      npts = ifelse( is.null(npts), 40, npts)
      # create a function of those two variables
      pfun = function(.x, .y){
        vals[[..f..$names[1]]] = .x
        vals[[..f..$names[2]]] = .y
        eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
      }
      if( length(ylab) == 0 ) ylab = ..f..$names[2]
      if( length(xlab) == 0 ) xlab = ..f..$names[1]
      if( length(zlab) == 0 ) zlab = deparse(..f..$sexpr)
      xlim2 = xlim
      ylim2 = ylim
      if( ..f..$names[1] %in% names(vals) ) {
          xlim2 = vals[[..f..$names[1]]]
      }
      if( ..f..$names[2] %in% names(vals) ) {
        ylim2 = vals[[..f..$names[2]]]
      }
      if (add  | length(xlim2)==0 | length(ylim2) == 0) {
        xlim2 = ..currentAxisLimits[[1]]
        ylim2 = ..currentAxisLimits[[2]]
        add = TRUE
        if( !all(..f..$names == ..currentAxisNames) ){
          stop(paste("Dependent variables in add-on plot, ",
           ..f..$names[1], " and ", 
           ..f..$names[2], ", do not match existing plotting variable ", 
           ..currentAxisNames[1], " and ", ..currentAxisNames[2], sep=""))
        }
      }
     
        
      if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
          stop(paste("Must provide x-axis limit via ", 
            ..f..$names, "= or xlim=", sep=""))
      }
        
      .xset = seq(min(xlim2), max(xlim2), length=npts)
      .yset = seq(min(ylim2), max(ylim2), length=npts)
  #   zvals = matrix( rep(0, length(.xset)*length(.yset)), nrow=length(.xset))
  #   for (k in 1:length(.xset)) {
  #    for (j in 1:length(.yset)) {
  #     zvals[k, j] = pfun( .xset[k], .yset[j] )
  #    }
  #   }
     
     if( !add ){
       ..currentAxisLimits <<- list( c(min(xlim2), max(xlim2)), c(min(ylim2), max(ylim2)))
     }
     
     zvals = outer(.xset, .yset, function(x, y){pfun(x, y)} )
     if( surface ) {
       grid = expand.grid( .xset, .yset )
       grid$height = c(zvals)
       if( require(manipulate) ) {
         manipulate(print(wireframe(height ~ Var1 + Var2, xlab=xlab, ylab=ylab, zlab=zlab, data=grid, drape=filled,
           shade=TRUE, screen=c(x=-90, y=rot, z=0), col=rgb(1, 1, 1, 0))), 
          rot = slider(-180, 180, step=5, initial=45, label="Rotation"))
           
       } 
       else {
         print(wireframe(height ~ Var1 + Var2, xlab=xlab, ylab=ylab, zlab=zlab, data=grid, drape=filled, shade=TRUE,
           col=rgb(1,1,1,0)) )
       }
       ..currentAxisNames <<- ..f..$names
       # No ADD method yet for surface plots
     }
     else {
      if( add & is.null(transparency) ) transparency=.4
      if( is.null(transparency) ) transparency=1
      fillcolors = colorscheme(20, alpha=transparency)
      if( is.logical(zvals[1,1]) ){ # it's a constraint function
        if( add ) fillcolors= c(rgb(0,0,0,transparency), rgb(0,0,0,0))
        else fillcolors = colorscheme(2)
        nlevels=2
      }
      if( filled) {
       image( .xset, .yset, zvals, col=fillcolors, add=add,
         xlab=xlab, ylab=ylab, main=main )
       contour(.xset, .yset, zvals, col=col, lwd=lwd, add=TRUE, nlevels=nlevels)
      }
      else {
       contour(.xset, .yset, zvals, nlevels=nlevels, add=add, lwd=lwd, col=col,
         xlab=xlab, ylab=ylab, main=main)
      } 
     }
     ..currentAxisNames <<- ..f..$names
    }
    else if( ndims > 2 ) 
      stop("More than 2 plotting variables.")
   invisible(..f..$fun)
  }
# =============================
    


createMathFun = function(sexpr=NULL, ...) {
  # sexpr = substitute(expr)
    # reconstruct the function finput from expr
    if(is.character(sexpr)) { # passing a function, e.g. sin
      fcall = paste(sexpr, "(x)~x",sep="")
      expr = parse(text = fcall)
      sexpr = substitute(expr)
    }
    if (is.numeric(sexpr)) {
      fcall = paste(sexpr,"+0*x ~ x",sep="")
      expr = parse(text=fcall)
      sexpr = substitute(expr)
    }
    # is.name(sexpr) is handled below
    
  #sexpr comes from a substitute(expr) in the top-level interface function.
  vals = list(...)
  # if( !is.null(s)) vals[["s"]] = s  
  # The above line, and the use of s in the argument list are
  # a kluge so that "s" can be used in the expression.
  specialNames=NULL
  specialVals=NULL
  if (is.name(sexpr)) {
        fargs = formals(eval(sexpr))
        if( length(fargs) == 0 ) fcall = paste(sexpr, "(x)~x",sep="")
        else {
          if( length(fargs) == 1 ) 
           fcall = paste(sexpr,"(",names(fargs),") ~ ", names(fargs))
          else {
            nms = c()
            for (k in 1:length(fargs)) {
              if( nchar(as.character(fargs[[k]]))>0 )
                nms = c(nms, names(fargs)[k])
            }
            fcall = paste( sexpr,"(", paste(nms,collapse=","),")~",
              paste(nms,collapse="+"))
          }
        }
        expr <- parse(text = fcall)
        sexpr = substitute(expr)
        specialNames = "x"
        specialVals=NULL
  }
  exprClass = tryCatch(class(eval(sexpr)), error=function(e){class(sexpr)})
  if (exprClass == "formula") {
    # Get the special names from the right side of the formula
    expr = eval(sexpr)
    sexpr = expr[[2]] # left side of formula
    specialNames = all.vars(expr[[3]]) # right side of formula
    specialVals = NULL
  }
  else if (exprClass == "call") {
    nmvals = names(vals)
  # which ones are the limits?
    sz = mapply( length, vals )
    inds = which(sz==2)
    specialNames = nmvals[inds]
    specialVals = vals[inds]
    vals[specialNames] = NULL
  }
  
    # create the formal arguments of the function
    goo = c(list())
    goo[specialNames] = NA # put it first in the list
    goo[all.vars(sexpr)] = NA
    goo[names(vals)] = vals 
    # make the plotting variable undefined again if the previous statement
    # gave them numerical values
    goo[specialNames] = NA 
    # EXCEPTIONS for 
    # global variables which are not accessible
    # if they are contained in the formals list
    goo["pi"] = NULL # let pi stand for pi
    
    ff = function() {
      eval(sexpr, enclos=parent.frame())
    }
    formals(ff) = goo
    return(list(fun=ff,names=specialNames,vals=specialVals,others=names(vals),sexpr=sexpr))
}
#### Derivative
newD = function(expr, ..h..=1e-4, method=c("center","left","right"), ...){
  vals = list(...)
  ..h.. = ifelse( length(..h..) == 0, 1e-4, ..h.. )
  sexpr = substitute(expr)
  
  foo = createMathFun(sexpr=sexpr, ...)
  if( length(foo$names) != 1 ) {
    stop("This function works only with a single variable of differentiation at a time.")
  }
  df = function() {
    baseline = eval(foo$sexpr)
    vvv = get(foo$names[1])
    # to avoid round-off error in h 
    temp = vvv + ..h..
    ..h.. = temp-vvv
    assign(foo$names[1], vvv+..h..)
    # use the "method" to decide which type of deriv to do.
    # Not yet implemented !!!!!!!!!!!
    return( (eval(foo$sexpr) - baseline)/..h..)
  }
  goo = list()
  goo[foo$names] = NA # put it first in the list
  goo[all.vars(foo$sexpr)] = NA
  goo[names(vals)] = vals
  goo[foo$names] = NA 
    # EXCEPTIONS for 
    # global variables which are not accessible
    # if they are contained in the formals list
    goo["pi"] = NULL # let pi stand for pi
  
  formals(df) = goo #formals(foo$fun)
  return(df) 
}

# ========================
#### Integral
newAntiD = function(expr, from=NULL, to=NULL, input0=0, ...){
  if( length(from) != 0 ) input0=from
  vals = list(...)
  sexpr = substitute(expr)
  
  
  # reconstruct the function finput from expr

  #if( is.name(sexpr) ) {
  #  fcall = paste(sexpr, "(x)~x")
  #  expr = parse(text = fcall)
  #}
  
  # If it's an expression
  foo = createMathFun(sexpr=sexpr, ...) 
  if( length(foo$names) != 1 ) {
    stop("This function works only with a single variable of integration.")
  }
  finput = function(.x) {
    assign(foo$names[1], .x)
    return( eval(foo$sexpr))
  }
  # Test if the function will evaluate at the starting point
  #init.x = ifelse( length(from)!=0, from, input0 )
  #v = finput(init.x)
  #if (abs(v) == Inf | is.nan(v)) 
  #  stop(paste("Function being integrated is Inf or NaN at x0 =", init.x))
  
  foutput = function() { # really a function of the upper limit: "to"
    ..finput = function(.x) { # Create the function in this environment
      assign(foo$names[1], .x)    
      return( eval(foo$sexpr) )
    }  
    # handle the case where to is fixed and from is assigned to multiple values
    ..multiplier=1
    if( length(from) > 1 & length(to) == 1 ){
      ..temp = to
      to = from
      from = ..temp
      ..multiplier=-1
    }
    # handle situation where both from and to are a set of values.
    if( length(from)>1 & length(to)>1 ){
      if( length(from)!=length(to) ) stop("Either fix 'from' or set it to the same length as 'to'")
      ..res = rep(0,length(to))
      for (..k.. in 1:length(to)) {
        ..res[..k..] = integrate(..finput,from[k],to[k])$value
      }
      return(..res)
    }
    ..val0 = integrate(..finput, from, to[1])$value
    if (length(to) == 1) {
      return(..multiplier*..val0)
    }
    ..res = rep(..val0, length(to))
    for (..k.. in 2:length(..res)) {
      ..res[..k..] = integrate(..finput, to[..k.. - 1], to[..k..])$value
    }
    ..res = cumsum(..res)
    return(..multiplier*..res)
  }
  if( any( c("to","from") %in% c(foo$names,names(formals(foo$fun)))) )
    stop("Can't use a variable called 'to' or 'from' in a function being integrated.")
  starting.args = alist(to=,from=input0)
  original.args = formals(foo$fun)
  goo = c(starting.args, original.args)
  goo[all.vars(foo$sexpr)] = NA
  goo[names(vals)] = vals
  goo[foo$names] = NULL 
    # EXCEPTIONS for 
    # global variables which are not accessible
    # if they are contained in the formals list
    goo["pi"] = NULL # let pi stand for pi
  
  formals(foutput) = goo
  return(foutput) 
}
