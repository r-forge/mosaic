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
 
 get.aspect.ratio = function() {
    y = convertY( unit(1,"native"),unitTo= "cm", valueOnly=TRUE)
    x = convertX( unit(1,"native"),unitTo= "cm",valueOnly=TRUE) 
#    y = convertUnit( unit(1,"native"),"cm",typeFrom="dimension", axisFrom="y",axisTo="y",valueOnly=TRUE)
#    x = convertUnit( unit(1,"native"),"cm",typeFrom="dimension", axisFrom="x",axisTo="x",valueOnly=TRUE)
   print(x)
   print(y)
   print(y/x)
   return(y/x)
   
}
 
#=============================== 
  myFun = function(npts=npts, narrows = narrows, scale=scale, nlevels = nlevels, 
                   components=components){
    .xset = seq(min(xlim2),max(xlim2),length=npts) #x, y, z
    .yset = seq(min(ylim2),max(ylim2),length=npts)
    .zset = outer(.xset, .yset, fun$fun )          #z is a function of x and y
    
    c.xpt = mean(xlim2)  #center point and vectors
    c.ypt = mean(ylim2)
    
    xpts = seq(min(xlim2),max(xlim2), length = narrows+1)  #Create grid of evenly spaced points
    xpts = rep(xpts, times = narrows+1)
    ypts = seq(min(ylim2),max(ylim2), length = narrows+1)
    ypts = rep(ypts, each = narrows+1)

    xpts=c(xpts,as.numeric(c.xpt)) 
    ypts=c(ypts,as.numeric(c.ypt))
    xvecs = dx(x=xpts, y=ypts)    #Derivatives for the gradient
    yvecs = dy(x=xpts, y=ypts)
    xvecs = scale*xvecs           #For large gradients, scaling down with manipulate is nicer
    yvecs = scale*yvecs
    
    bigstart = .05; bigend = .95  #rainbow colors are clearer when it goes from one extreme to the other, not red to red
    mylevels = pretty(range(.zset),nlevels)   #number of contours
    image( .xset, .yset, .zset, col=rainbow(npts, alpha=0.8, start=bigstart, end=bigend),
             add=FALSE, xlab=xlab,ylab=ylab,main=NULL )
    contour(.xset, .yset, .zset, col=rgb(0,0,0,.5),lwd=3,add=TRUE, labcex=1.2, 
              levels=mylevels, method="edge")
    
    
    arrows(x0=xpts, y0=ypts, x1=xpts+xvecs, y1=ypts+yvecs, lwd = 3)
  #  arrows(x0=c.xpt, y0=c.ypt, x1=c.xpt+c.xvec, y1=c.ypt+c.yvec, lwd=3)
    get.aspect.ratio()
    if(components){
      close = which(xpts>(diff(range(xlim2))/4+min(xlim2))&xpts<(3*diff(range(xlim2))/4+min(xlim2))
                    &(ypts>(diff(range(ylim2))/4+min(ylim2)))&(ypts<3*(diff(range(ylim2))/4)+min(ylim2)))
      
      close.xpts=xpts[close]
      close.ypts=ypts[close]
      close.xvecs=xvecs[close]
      close.yvecs=yvecs[close]
      arrows(x0=close.xpts, x1=close.xpts+close.xvecs, y0=close.ypts, y1=close.ypts, lwd=2, col="red", length = .1)
      arrows(x0=close.xpts, x1=close.xpts, y0=close.ypts, y1=close.ypts+close.yvecs, lwd=2, col="red", length = .1)
    }       
  }
#=====================================      
 manipulate(myFun(npts=npts, narrows = narrows, scale=scale, nlevels = nlevels, 
                  components=components),
            npts = slider(20,200,initial=140,label = "Number of pixels"),
            narrows = slider(0,10, initial = 5, step = 1, label = "Number of arrows per row"),
            scale = slider(.01, 1, step = .01, initial = .5, label="% Arrow scale"),
            nlevels = slider(5, 50, initial = 20, step = 1, label = "Approx. number of contour lines"),
            components = checkbox(FALSE, label = "Display components of gradient")
            )
}
            
