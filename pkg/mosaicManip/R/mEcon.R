mEcon=function(xlim=c(0,20)){
  if (!require(manipulate) | !require(lattice) | !require(grid)) stop("Must have manipulate package.")
  x=seq(min(xlim), max(xlim), length=1000)
  myFun=function(sup1, sup2, dem1, dem2){
    supFun=splinefun(c(0,10,20), y=c(0, sup1, sup2)) 
    demFun=splinefun(c(0,10,20), y=c(50, dem1, dem2))
    func=function(x){supFun(x)-demFun(x)}
    equil=uniroot(func, interval=c(0,20)) #Solve for equil qty.
    xEquil=equil$root
    yEquil=supFun(xEquil)
    Supply=supFun(x)          #making my life easy with auto.key by naming vars Supply and Demand
    Demand=demFun(x)
    xx=x     #To avoid duplication in the polygons in the panel function
    panel=function(x,y,...){
      panel.xyplot(x,y,...)
      llines(x=c(xEquil, -999999), y=c(yEquil, yEquil), lty=2, col="black")
      llines(x=c(xEquil, xEquil), y=c(yEquil, -999999), lty=2, col="black")
      leftx=xx[xx<=xEquil]
      ySup=c(supFun(leftx), rep(yEquil, length(leftx)))
      xpts=c(min(leftx),leftx,max(leftx))
      lpolygon(x=c(leftx, rev(leftx), min(leftx)), y=c(rep(yEquil, length(leftx)), supFun(rev(leftx)), yEquil ), 
               col=rgb(0,0,.8,.2), border=FALSE)
      yDem=c(demFun(leftx), rep(yEquil, length(leftx)))
      lpolygon(x=c(min(leftx), leftx, max(leftx)), y=c(yEquil, demFun(leftx), yEquil), 
               col=rgb(.8,0,0,.2), border=FALSE)

      
    }
    xyplot(Supply+Demand~x, auto.key=TRUE, xlab="Quantity", ylab="Price",
           type="l", lwd=3, panel=panel)
  }
  manipulate(myFun(sup1=sup1, sup2=sup2, dem1=dem1, dem2=dem2),
             sup1=slider(1,40, step=.01, initial=25, label="Supply pt 1"),
             sup2=slider(1,60, step=.01, initial=50, label="Supply pt 2"),
             dem1=slider(20, 60, step=.01, initial=25, label="Demand pt 1"),
             dem2=slider(0, 40, step=.01, initial=5, label="Demand pt 2")
             ) 
}