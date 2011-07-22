mEcon=function(xlim=c(0,20)){
  if (!require(manipulate) | !require(lattice) | !require(grid)) stop("Must have manipulate package.")
  x=seq(min(xlim), max(xlim), length=1000)
  myFun=function(sInt, sSlope, dInt, dSlope){
    Supply=sInt+sSlope*x
    Demand=dInt+dSlope*x
    func=function(x){sInt+sSlope*x-dInt-dSlope*x}

    
    equil=which(Supply==Demand)
    xEquil=x[equil]
    sEquil=Supply[equil]

    panel=function(x,y,...){
      panel.xyplot(x,y,...)
      llines(x=c(xEquil, -Inf), y=c(sEquil, sEquil), lty=2)
      llines(x=c(xEquil, xEquil), y=c(sEquil, -Inf), lty=2)
      
    }
    xyplot(Supply+Demand~x, auto.key=TRUE, xlab="Quantity", ylab="Price",
           type="l", lwd=3, panel=panel)
  }
  manipulate(myFun(sInt=sInt, sSlope=sSlope, dInt=dInt, dSlope=dSlope),
             sInt=slider(-50,50, step=.01, initial=0, label="Supply Intercept"),
             sSlope=slider(0,5, step=.01, initial=1, label="Supply slope"),
             dInt=slider(0, 100, step=.01, initial=50, label="Demand Intercept"),
             dSlope=slider(-5, 0, step=.01, initial=-1, label="Demand Slope")
             )
}