mHist=function(data=NULL){
  if (!require(manipulate) | !require(lattice)) stop("Must have manipulate package.")
  newCol=rgb(0,.2,.8,.6)
  oldCol=rgb(.9,.9,0,.2)
  .iter=0
  if(.iter==0)
    oldBreaks<<-NULL
  if(is.null(data)){
    x=rnorm(500, mean=0, sd=5)
  }
  else
    x=data
  #===============
  myFun=function(nBands, cBreak, seed){
    set.seed(seed)
    .iter<<-.iter+1
    
    rng=diff(range(x))
    c.rng = diff(c(min(x),cBreak))
    minratio= c.rng/rng
    maxratio= 1-minratio
    myBreaks = seq(cBreak, diff(range(x))/8+max(x), by=)
    myBreaks = c(seq(min(x)-diff(range(x))/8, cBreak, length=nBands*minratio), myBreaks)
    
    myPan=function(x,...){
       panel.histogram(x, breaks=myBreaks, col=newCol)
        if(!is.null(oldBreaks)){
         panel.histogram(x, col=oldCol, breaks=oldBreaks, border=FALSE)
        }
      }
    
    newHist=histogram(x, panel=myPan, type = "density")
    print(newHist)
       
    oldBreaks<<-myBreaks
  }
       controls = list(nBands=slider(1, 50, step=1, initial=10, label="Number of histogram bins"),
             cBreak=slider(mean(x)-3,mean(x)+3, step=.01, initial=mean(x), label="Center Break"),
             seed=slider(1,100,step=1, initial=1, label="Random seed"))

 #========================
  manipulate(myFun(nBands=nBands, cBreak=cBreak, seed=seed),
             controls
             )
  
  
}
#Alpha slider for the overlay
#do by rather than length for myBreaks
#if data not null, kill seed