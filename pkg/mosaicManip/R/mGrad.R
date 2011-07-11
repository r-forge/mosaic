mGrad = function(expr, ..., xlim = c(0,10), ylim = c(0,10)){
 if(!require(manipulate)) stop("Must use a manipulate-compatible version of R, e.g. RStudio")
 if (!require("mosaic")) stop("Must install mosaic package.")

 vals = list(...)                #Take extra terms, make math function, set xy limits
 fun = mosaic:::.createMathFun(sexpr = substitute(expr), ...)
 ylab = fun$names[2]
 xlab = fun$names[1]
 xlim2 = xlim
 ylim2 = ylim
 if( fun$names[1] %in% names(vals) ) {
   xlim2 = vals[[fun$names[1]]]
   }
 if( fun$names[2] %in% names(vals) ) {
   ylim2 = vals[[fun$names[2]]]
   }
                            # Take partial with respect to the first var
 funx = fun
 funx$RHS = funx$RHS[2]     #"make it just the first variable"
 dx = mosaic:::.doD(funx, ..h..=NULL, numerical=FALSE, method="center",...)
 funy = fun                 #Partial with respect to the second
 funy$RHS = funy$RHS[3]
 dy = mosaic:::.doD(funy, ..h..=NULL, numerical=FALSE, method="center",...)
 #============
 get.aspect.ratio = function() {
  a = par("din")
  b = par("mai")
  xwid=a[1] - (b[2]+b[4])
  ywid=a[2] - (b[1]+b[3])
  return(ywid/xwid  )   
}
 #==============
 make.arrows=function(x, y, xvec, yvec, col="black", wid=3, length=.2){
   if(xvec==0&&yvec==0){
     points(x, y)
   }
   else{
     arrows(x0=x, y0=y, x1=x+xvec, y1=y+yvec, lwd = wid, col=col, length=length)
   }
 }
 
#=============================== 
  myFun = function(npts=npts, delta = delta, scale=scale, nlevels = nlevels, 
                   disp=disp){
    .xset = seq(min(xlim2),max(xlim2),length=npts) #x, y, z
    .yset = seq(min(ylim2),max(ylim2),length=npts)
    .zset = outer(.xset, .yset, fun$fun )          #z is a function of x and y
    
    c.xpt = mean(xlim2)  #center point and vectors
    c.ypt = mean(ylim2)
    
    xpts = seq(c.xpt, max(xlim2), by = delta)  #xpts/ypts are the grid of arrows
    xpts = c(xpts, seq(c.xpt, min(xlim2), by = -delta))
    xpts = rep(xpts, times = length(xpts))
    ypts = seq(c.ypt, max(ylim2), by = delta)
    ypts = c(ypts, seq(c.ypt, min(ylim2), by = -delta))
    ypts = rep(ypts, each = length(ypts))
    
    aspect=get.aspect.ratio()
    aspect=aspect/(diff(range(ylim2))/diff(range(xlim2)))
    
    xvecs = dx(x=xpts, y=ypts)    #Derivatives for the gradient
    yvecs = dy(x=xpts, y=ypts)
    xvecs = scale*xvecs*aspect           #For large gradients, scaling down with manipulate is nicer
    yvecs = (scale*yvecs)/aspect
    
    bigstart = .80; bigend = .20  #rainbow colors are clearer when it goes from one extreme to the other, not red to red
    mylevels = pretty(range(.zset),nlevels)   #number of contours
    image( .xset, .yset, .zset, col=rainbow(npts, alpha=0.8, start=bigstart, end=bigend),
             add=FALSE, xlab=xlab,ylab=ylab,main=NULL )
    contour(.xset, .yset, .zset, col=rgb(0,0,0,.5),lwd=3,add=TRUE, labcex=1.2, 
              levels=mylevels, method="edge")
    
    if(disp== "Gradients"){
        make.arrows(x=xpts, y=ypts, xvec=xvecs, yvec=yvecs, wid = 3, col="black")
      }
      if(disp == "Components"){
        close.xpts=xpts
        close.ypts=ypts
        close.xvecs=xvecs
        close.yvecs=yvecs
        make.arrows(x=close.xpts, y=close.ypts, xvec=close.xvecs, yvec=0, wid = 2, col="red", length=.1)
        make.arrows(x=close.xpts, y=close.ypts, xvec=0, yvec=close.yvecs, wid = 2, col="blue", length=.1)
      }
      if(disp == "Both"){
        make.arrows(x=xpts, y=ypts, xvec=xvecs, yvec=yvecs, wid = 3, col="black")
        close = which(xpts>(diff(range(xlim2))/4+min(xlim2))&xpts<(3*diff(range(xlim2))/4+min(xlim2))
                    &(ypts>(diff(range(ylim2))/4+min(ylim2)))&(ypts<3*(diff(range(ylim2))/4)+min(ylim2)))
        close.xpts=xpts[close]
        close.ypts=ypts[close]
        close.xvecs=xvecs[close]
        close.yvecs=yvecs[close]
        make.arrows(x=close.xpts, y=close.ypts, xvec=close.xvecs, yvec=0, wid = 2, col="red", length=.1)
        make.arrows(x=close.xpts, y=close.ypts, xvec=0, yvec=close.yvecs, wid = 2, col="blue", length=.1)
      }
    
           
  }
#=====================================      
  manipulate(myFun(npts=npts, delta = delta, scale=scale, nlevels = nlevels, disp=disp),
            npts = slider(20,200,initial=140,label = "Number of pixels"),
            delta = slider(0,10, initial = 1, step = .01, label = "Space between arrows"),
            scale = slider(.01, 1, step = .01, initial = .25, label="% Arrow scale"),
            nlevels = slider(5, 50, initial = 20, step = 1, label = "Approx. number of contour lines"),
            disp = picker("Gradients","Components", "Both", label = "Display")
             
            )
}
            
