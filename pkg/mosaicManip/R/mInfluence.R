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
  #====================
  myFun=function(xpick){
    mod=lm(expr, data)
    
    influ=influence.measures(mod)
    influpts=which(apply(influ$is.inf, 1, any))
    influpts=data[influpts,]
    browser()
    myPanel=function(x,y,...){
      panel.xyplot(x,y, ...)
      lpoints(influpts[[xvar[[xpick]]]], influpts[[yvar]], col="red")
      panel.abline(mod)  
    }
#     if(is.factor(data[[yvar]]))
#       data[[yvar]]=jitter(data[[yvar]])

    xyplot(data[[yvar]]~data[[xvar[[xpick]]]], ylab=yvar, xlab=xvar[[xpick]],
           panel=myPanel)
  }
  #==================
  controls=list(xpick=picker(xvar, label="Dep. Variable to plot"),
                influPick=picker(cooks.distance="Cook's Distance", label="Type of Influence")
                )
  manipulate(myFun(xpick=xpick), 
             controls)
}