mInfluence=function(expr, data){
  if (!require(manipulate) | !require(lattice) | !require(grid)) stop("Must have manipulate package.")
  xvar = list(); chars=list()
  yvar = all.vars(expr)[1]
   
  terms=terms(expr)
  nterms=length(terms)

  for(a in all.vars(expr)[-1]){
    #temp=as.character(terms[a])
    xvar[[a]]=a
  }
  maxsd=0
  for(b in 1:(nterms-1)){
    maxsd=max(maxsd, sd(data[[xvar[[b]]]]), na.rm=TRUE) #HERE categorical variables return In var(as.vector(x), na.rm = na.rm) : NAs introduced by coercion
  }

  #====================
  myFun=function(xpick, whereX, whereY, newPoint){    
    newRow=data.frame()
    colnames(newRow)=colnames(data)
browser()
    newRow[[1,xvar[[xpick]]]]=whereX
    newRow[[1,yvar]]=whereY
    modData=rbind(data, newRow)
    browser()
    if(newPoint)
      data=modData
    
    mod=lm(expr, data)
    coefs=coef(mod)
    influ=influence.measures(mod)
    influpts=which(apply(influ$is.inf, 1, any))
    influpts=data[influpts,]

    myPanel=function(x,y,...){
      panel.xyplot(x,y, ...)
      lpoints(influpts[[xvar[[xpick]]]], influpts[[yvar]], col="red")
      panel.abline(mod)
      if(newPoint){
        lpoints(modData[dim(modData)[1],], col="orange", cex=2)
        print(modData[dim(modData)[1],])
      }
    }
    
#     if(is.factor(data[[yvar]]))
#       data[[yvar]]=jitter(data[[yvar]])

    xyplot(data[[yvar]]~data[[xvar[[xpick]]]], ylab=yvar, xlab=xvar[[xpick]],
           panel=myPanel)
  }
  #==================
  controls=list(xpick=picker(xvar, label="Dep. Variable to plot"),
                newPoint=checkbox(FALSE, label="Add new point"),
                whereX=slider(-5*maxsd, 5*maxsd, 
                              initial=1*maxsd, label="New x"),
                whereY=slider(-5*maxsd, 5*maxsd, 
                              initial=1*maxsd, label="New y"),
                influPick=picker(cooks.distance="Cook's Distance", label="Type of Influence")
                )
  manipulate(myFun(xpick=xpick, whereX=whereX, whereY=whereY, newPoint=newPoint), 
             controls)
}