# Phase-Plane Software
# revise should be a push-button
mPP = function( DE=predator.prey, xlim=c(-10,2000),ylim=c(-10,2000)) {
  if (!require(manipulate)) stop("Must have manipulate package.")
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
  doWhatState="initializing"
  storeWhatState="Working"
  # ========
  plotTraj = function(soln, n=1001, ...) {
    t = seq(soln$tlim[1], soln$tlim[2], length=n )
    one = soln[[1]](t)
    two = soln[[2]](t)
    lines(one, two, ...) # replace with lattice equivalent
  }
  # ========
  doPlot = function(xstart,ystart,Ntraj,tdur,tback,
                    nullclines=FALSE,doWhat,param1,param2) {
    # set initial condition
    initCond[1] <<- xstart
    initCond[2] <<- ystart
    # Handle editing of the system, setting initial condition here
    # Need to set state manually to avoid lockup
    if( doWhatState != doWhat ) {
      # state changed, so do something
      doWhatState <<- doWhat
      if( doWhat == "revise") DE <<- edit(DE)
      if( doWhat == "initial") {
        # Get the initial condition via locator () ## CHANGE FOR LATTICE
        # foo = locator(1) # DOESNT WORK WITH MANIPULATE
       
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
    # Plot out the flow field
    flow.plot( DE, xlim=xlim, ylim=ylim)
    # Plot out the nullclines
    if( nullclines ) show.nullclines()
    # plot out the trajectories
    # NEED TO DO BOTH FORWARD AND BACKWARD, maybe alpha different for backward, or darken a bit
    # here is the forward one
    for( k in 1:length(TS)) {
      if( !is.null(TS[[k]]$system)) {
        if( !is.null(TS[[k]]$forward) )
          plotTraj( TS[[k]]$forward, col=Tcolors[k])
        if( !is.null(TS[[k]]$back) ) 
          plotTraj( TS[[k]]$back, col=TcolorsBack[k])
        goo = TS[[k]]$init
        points( goo[1], goo[2], col=Tcolors[k],pch=20)
      }
    }
  }
  # =======
  manipulate( doPlot(xstart=xstart, ystart=ystart, 
                     Ntraj=Ntraj,tdur=tdur,tback=tback,
                     nullclines=nullclines,doWhat=doWhat,param1=param1,param2=param2),
              xstart = slider(xlim[1],xlim[2],init=mean(xlim),label=paste(stateNames[1], "Start")),
              ystart = slider(ylim[1],ylim[2],init=mean(ylim),label=paste(stateNames[2], "Start")),
              Ntraj = picker( One=2,Two=3,Three=4,Four=5,Five=6, initial="One",label="Trajectory"),
              tdur = slider(0,100,init=10,label="Trajectory Duration"),
              tback = slider(-100,0,init=0, label="Go back in time"),
              nullclines = checkbox(initial=FALSE, label="Show Nullclines"),
              doWhat = picker( ShowTraj="show", SetInitialCond="initial", ReviseSystem="revise",initial="ShowTraj",label="Do ..."),
              param1 = slider(.1,10,init=1,label="Parameter 1"),
              param2 = slider(.1,10,init=1,label="Parameter 2")
              )
}