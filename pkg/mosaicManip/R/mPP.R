# Phase-Plane Software
# revise should be a push-button
mPP = function( DE=predator.prey, xlim=c(-10,2000),ylim=c(-10,2000)) {
  if (!require(manipulate) | !require(lattice)) stop("Must have manipulate package.")
  # Storage for the trajectories.  Starts out empty
  Tcolors = c("red","green","blue", "steelblue","springgreen","pink")
  TcolorsBack = c("deeppink","darkgreen", "darkblue","darkseagreen","darkslategray","fuschia")
  TS = list()
  for (k in 1:length(Tcolors)) 
    TS[[k]] = list(foward=NULL, back=NULL, system=NULL, init=NULL)
  # An initial condition
  initCond = c(mean(xlim),mean(ylim))
  stateNames = names(formals(DE))
  names(initCond) = stateNames # needed so that solveDE will work
  reviseWhatState="initializing"
  storeWhatState="Working"
  # ========
  plotTraj = function(soln, n=1001, ...) {
    t = seq(soln$tlim[1], soln$tlim[2], length=n )
    one = soln[[1]](t)
    two = soln[[2]](t)
    llines(one, two, ...)
  }
  #========
  plotPort = function(soln, var=x, n=1001, ...){
    xport=xyplot(x~t, ylab=stateNames[1], xlab=NULL, type = "l", lwd=3, scales=list(x=list(draw=FALSE)))
    yport=xyplot(y~t, ylab=stateNames[2], xlab="t", type = "l", lwd=3)
    return(list(xport,yport))
  }
  #=========
  flow.plot = function(fun,xlim=c(0,1), ylim=c(0,1), resol=10, col="black",
  add=FALSE,EW=NULL,NS=NULL,both=TRUE) {
  current.dyn.system <<- fun
  arg.names = names(formals(fun) )
  if (length( arg.names ) != 2 )
    stop("Must give dynamical function with two arguments.")
  if (add) {
    hoo = par("usr")
    xlim = hoo[1:2]
    ylim = hoo[3:4]
  }
  else{
    #panel.xyplot(x=0, y=0, xlim=xlim, ylim=ylim,
    #   xlab=arg.names[1], ylab=arg.names[2] )
  }
   
  x <- matrix(seq(xlim[1],xlim[2], length=resol), byrow=TRUE, resol,resol);
  y <- matrix(seq(ylim[1],ylim[2], length=resol),byrow=FALSE, resol, resol);
  npts <- resol*resol;
  xspace <- abs(diff(xlim))/(resol*5);
  yspace <- abs(diff(ylim))/(resol*5);
  x <- x + matrix(runif(npts, -xspace, xspace),resol,resol);
  y <- y + matrix(runif(npts, -yspace, yspace),resol,resol);
  z <- fun(x,y);
  z1 <- matrix(z[1:npts], resol, resol);
  z2 <- matrix(z[(npts+1):(2*npts)], resol, resol);
  maxx <- max(abs(z1));
  maxy <- max(abs(z2));
  dt <- min( abs(diff(xlim))/maxx, abs(diff(ylim))/maxy)/resol;
  lens <- sqrt(z1^2 + z2^2);
  lens2 <- lens/max(lens); 
  if( both ){
    larrows(c(x), c(y),
           c(x+dt*z1/((lens2)+.1)), c(y+dt*z2/((lens2)+.1)),
           length=.04, col=col);
  }
  if( !is.null(NS) ) {
    larrows(c(x), c(y),
           c(x), c(y+dt*z2/((lens2)+.1)),
           length=.04, col=NS);
  }
  if( !is.null(EW) ){
    larrows(c(x), c(y),
           c(x+dt*z1/((lens2)+.1)), c(y),
           length=.04, col=EW);
  }
    
    
}
  
  # ========
  doPlot = function(xstart,ystart,Ntraj,tdur,tback,
                    nullclines=FALSE,reviseWhat,flowWhat,param1,param2) {
    # set initial condition
    initCond[1] <<- xstart
    initCond[2] <<- ystart
    arg.names = names(formals(DE) )
    # Handle editing of the system, setting initial condition here
    # Need to set state manually to avoid lockup
    if( reviseWhatState != reviseWhat ) {
      # state changed, so do something
      reviseWhatState <<- reviseWhat
      if(!is.null(TS[[Ntraj]]$system)){
        DE <<- TS[[Ntraj]]$system
      }
      if( reviseWhat >= 0) {
        DE <<- edit(DE) 
      }
      
    }
    # ... system editing code here
          
# Store the results in the currently selected trajectory in "scratch" index 1
    TS[[1]]$init <<- initCond
    TS[[1]]$system <<- DE
    # Find the forward trajectory
    if( tdur > 0 )
      TS[[1]]$forward <<- solve.DE( DE, init=initCond, tlim=c(0,tdur) )
    else TS[[1]]$forward <<- NULL
    # Solve the trajectory backward here.  (Does solve.DE do this?  Add a backward flag!)
    if (tback < 0 )
      TS[[1]]$back <<- solve.DE( DE, init=initCond, tlim=c(0,tback) )
    else TS[[1]]$back <<- NULL
      
    if( storeWhatState != Ntraj ){
      storeWhatState <<- Ntraj
    
      # Store the results in the currently selected trajectory
      TS[[Ntraj]]$init <<- TS[[1]]$init
      TS[[Ntraj]]$system <<- TS[[1]]$system
      TS[[Ntraj]]$forward <<- TS[[1]]$forward
      TS[[Ntraj]]$back <<- TS[[1]]$back
    }
    
    
    

      TSfull=data.frame()
      for(k in 1:Ntraj){
        if(!is.null(TS[[k]]$forward)){
          
          TSfull[[paste("x",k, sep = "")]] = TS[[k]]$forward[[1]](.t)
          TSfull[[paste("y",k, sep = "")]] = TS[[k]]$forward[[2]](.t)
          
        }
      }
      
      port=plotPort(TSfull$forward)
      

    #=============
    myPanel=function(x,y, ...){
      # Plot out the flow field
      flow.plot( TS[[flowWhat]]$system, xlim=xlim, ylim=ylim)
      # Plot out the nullclines
      if( nullclines ) show.nullclines()
      # plot out the trajectories
      # NEED TO DO BOTH FORWARD AND BACKWARD, maybe alpha different for backward, or darken a bit
      # here is the forward one
      for( k in 1:length(TS)) {
        if( !is.null(TS[[k]]$system)) {
          if( !is.null(TS[[k]]$forward) )
            plotTraj( TS[[k]]$forward, col=Tcolors[k+1])
          if( !is.null(TS[[k]]$back) ) 
            plotTraj( TS[[k]]$back, col=TcolorsBack[k+1])
          goo = TS[[k]]$init
          lpoints( goo[1], goo[2], col=Tcolors[k],pch=20)
        }
      }
    }
    PP=xyplot(ylim~xlim, panel=myPanel, xlab=NULL, ylab=stateNames[2], scales=list())
    print(PP, position=c(0.1,.43,.9,1), more=TRUE)
    print(port[[1]], position=c(0, .25, 1, .5), more=TRUE)
    print(port[[2]], position=c(0, 0, 1, .33), more=FALSE)
  }
  # =======
  manipulate( doPlot(xstart=xstart, ystart=ystart, 
                     Ntraj=Ntraj,tdur=tdur,tback=tback,
                     nullclines=nullclines,reviseWhat=reviseWhat,
                     flowWhat=flowWhat, param1=param1,param2=param2),
              xstart = slider(xlim[1],xlim[2],init=mean(xlim),label=paste(stateNames[1], "Start")),
              ystart = slider(ylim[1],ylim[2],init=mean(ylim),label=paste(stateNames[2], "Start")),
              Ntraj = picker( One=2,Two=3,Three=4,Four=5,Five=6, initial="One",label="Trajectory"),
              tdur = slider(0,100,init=10,label="Trajectory Duration"),
              tback = slider(-100,0,init=0, label="Go back in time"),
              nullclines = checkbox(initial=FALSE, label="Show Nullclines"),
              reviseWhat = picker( "None"=-1, "All" = 0, "One"=1, "Two"=2, "Three" = 3,
                                   "Four"=4, "Five"=5, label="Revise DE for", initial = "None"),
              flowWhat= picker("One"=1, "Two"=2, "Three" = 3, "Four"=4, "Five" = 5, 
                               initial = "One", label="What flow to plot?"),
              param1 = slider(.1,10,init=1,label="Parameter 1"),
              param2 = slider(.1,10,init=1,label="Parameter 2")
              )
}