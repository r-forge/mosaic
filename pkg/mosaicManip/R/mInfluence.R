mInfluence=function(expr, data){
  if (!require(manipulate) | !require(lattice) | !require(grid)) stop("Must have manipulate package.")
  xvar = list();
  yvar = all.vars(expr)[1]
   
  terms=terms(expr)
  nterms=length(terms)
  for(a in all.vars(expr)[-1]){  #Name the levels of xvar, filled with char string
    xvar[[a]]=a
  }
  #====================
  myFun=function(xpick, multX, multY, newPoint){
    xdat=data[[xvar[[xpick]]]]
    ydat=data[[yvar]]
    newRow=data[1,]
    for(b in 1:length(newRow)){
      if(is.numeric(newRow[1,b]))     #At the moment just leaving the categorical variables as is
        newRow[1,b]=median(data[,b])  #That is, what they were from data[1,], first row of data
    }

    if(is.numeric(newRow[[xpick]])){
      newRow[[1,xpick]]=multX*sd(xdat)+mean(xdat) #This needs to be adjusted to allow categorical 
      }                                                   #variables to work. mean(factor) doesn't work
    if(is.factor(newRow[[xpick]])){
      newRow[[1,xpick]]=levels(xdat)[floor(length(levels(xdat))*(multX+5)/10.001)+1]
    }
    if(is.numeric(newRow[[1,yvar]])){
      newRow[[1,yvar]]=multY*sd(ydat)+mean(ydat)
      }
     if(is.numeric(xdat))
       maxxlim=c(-5.2*sd(xdat)+mean(xdat), 5.2*sd(xdat)+mean(xdat))  #manipulate control params
     maxylim=c(-5.2*sd(ydat)+mean(ydat), 5.2*sd(ydat)+mean(ydat))
    modData=rbind(data, newRow)
    if(newPoint)
      data=modData
    if(is.factor(data[[xvar[[xpick]]]])){
      xlevels=levels(xdat)
    }
    mod=lm(expr, data)
    influ=influence.measures(mod)
    influpts=which(apply(influ$is.inf, 1, any))
#     influpts=data[influpts,]

    myPanel=function(x,y,...){
      if(is.factor(x)) {
        set.seed(73241)
        x=jitter(as.numeric(x))
      }
      panel.xyplot(x,y,  ...)  #Data
      lpoints(x[influpts], y[influpts], col="red") #Influential Data overplotted
      panel.xyplot(x,fitted(mod), pch=18, cex=1, col="black")          #Fitted data points
      if(newPoint){
        lpoints(x[length(x)], y[length(y)], col="orange", cex=2, lwd=3)  #last point is newpoint
      }
    }
    
    if(is.factor(xdat)){
      xyplot(data[[yvar]]~data[[xvar[[xpick]]]], ylab=yvar, 
             xlab=xvar[[xpick]],panel=myPanel, ylim=maxylim,
             scales=list(x=list(labels=xlevels,at=seq(1,length(xlevels),by=1))))
    }
    else
      xyplot(data[[yvar]]~data[[xvar[[xpick]]]], ylab=yvar, xlab=xvar[[xpick]],
             xlim=maxxlim, ylim=maxylim, panel=myPanel)
  }
  #==================
  controls=list(xpick=picker(xvar, label="Dep. Variable to plot"),
                newPoint=checkbox(FALSE, label="Add new point"),
                multX=slider(-5, 5, initial=1, label="New x (measured in SDs)"),
                multY=slider(-5, 5, initial=1, label="New y (measured in SDs)")
            ##    influPick=picker(cooks.distance="Cook's Distance", label="Type of Influence")
            ## Influence picker never implemented, Influence calculated by
            ## influence.measures is.inf    
                )
  manipulate(myFun(xpick=xpick, multY=multY, multX=multX, newPoint=newPoint), 
             controls)
}