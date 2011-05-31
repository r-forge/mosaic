fplot2 <- function(
  expr, ..., 
  add=FALSE,
  ylab=NULL, xlab=NULL, zlab=NULL, 
  main=NULL, 
  xlim=NULL, 
  ylim=NULL, 
  npts=NULL,
  lwd=1, 
  col="black", 
  filled=TRUE, 
  nlevels=10,
  surface=FALSE,
  colorscheme=topo.colors, 
  type="l", 
  transparency=NULL 
) { 

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
	  return(xyplot( .yset ~ .xset, 
	  	expr = expr,
	  	panel=panel.plotFun,
		xlim=xlim,
		ylim=ylim,
  		ylab=ylab, xlab=xlab, 
  		main=main, 
  		npts=npts,
  		lwd=lwd, 
  		col=col,
		filled=filled,
		nlevels=nlevels,
  		type=type,
	  ))
    }
     
  }
# =============================
