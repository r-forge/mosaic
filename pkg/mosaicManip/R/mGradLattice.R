LmGrad = function(expr, ..., xlim = c(0,10), ylim = c(0,10)){
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
 #==============================
 make.arrows=function(x, y, xvec, yvec, col="black", wid=3, length=.2){
   if(xvec==0&&yvec==0){
     lpoints(x, y)
   }
   else{
     larrows(x0=x, y0=y, x1=x+xvec, y1=y+yvec, lwd = wid, col=col, length=length)
   }
 }
 
#=============================== 
  myFun = function(npts=npts, delta = delta, scale=scale, nlevels = nlevels, 
                   disp=disp){
    .xset = seq(min(xlim2),max(xlim2),length=npts) #x, y, z
    .yset = seq(min(ylim2),max(ylim2),length=npts)
    .zset = outer(.xset, .yset, fun$fun )          #z is a function of x and y
    #EXPAND.GRID method:
    
    g= expand.grid(x=.xset, y=.yset)
    g$z=fun$fun(g$x,g$y)
    
    c.xpt = mean(xlim2)  #center point and vectors
    c.ypt = mean(ylim2)
    
    xpts = seq(c.xpt, max(xlim2), by = delta)
    xpts = c(xpts, seq(c.xpt, min(xlim2), by = -delta))
    xpts = rep(xpts, times = length(xpts))
    ypts = seq(c.ypt, max(ylim2), by = delta)
    ypts = c(ypts, seq(c.ypt, min(ylim2), by = -delta))
    ypts = rep(ypts, each = length(ypts))

    xpts=c(xpts,as.numeric(c.xpt)) 
    ypts=c(ypts,as.numeric(c.ypt))
    xvecs = dx(x=xpts, y=ypts)    #Derivatives for the gradient
    yvecs = dy(x=xpts, y=ypts)
    xvecs = scale*xvecs           #For large gradients, scaling down with manipulate is nicer
    yvecs = scale*yvecs
    dat = data.frame(x=.xset, y = .yset, z=.zset)
    
    mypanel= function(x,y,z,...){
      
      panel.levelplot(x, y, z, contour=TRUE, ...)
      #panel.contourplot(x,y,z, , ...)
      
      if(disp== "Gradients"){
        make.arrows(x=xpts, y=ypts, xvec=xvecs, yvec=yvecs, wid = 3, col="black")
      }
      if(disp == "Components"){
#         close = which(xpts>(diff(range(xlim2))/4+min(xlim2))&xpts<(3*diff(range(xlim2))/4+min(xlim2))
#                     &(ypts>(diff(range(ylim2))/4+min(ylim2)))&(ypts<3*(diff(range(ylim2))/4)+min(ylim2)))
      
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

    bigstart = .05; bigend = .95  #rainbow colors are clearer when it goes from one extreme to the other, not red to red
#     mylevels = pretty(range(.zset),nlevels)   #number of contours
    #print(contourplot(z~x*y,data=g))
    print(levelplot(z~x*y, data=g, panel = mypanel))
    get.aspect.ratio()

  }
#=====================================      
 manipulate(myFun(npts=npts, delta = delta, scale=scale, nlevels = nlevels, disp=disp),
            npts = slider(20,200,initial=140,label = "Number of pixels"),
            delta = slider(0,10, initial = 2, step = .01, label = "Space between arrows"),
            scale = slider(.01, 1, step = .01, initial = .5, label="% Arrow scale"),
            nlevels = slider(5, 50, initial = 20, step = 1, label = "Approx. number of contour lines"),
            disp = picker("Gradients","Components", "Both", label = "Display")
            )
}
            
#Make one central arrow DONE
#for 5 or 9 around it x and y components    DONE
#Make sure arrows can go in negative space    DONE
#Fix aspect ratio so arrows are always perpendicular

#Redo in lattice!
#Grid as a difference from the center
#Draw Arrows function to stop all these warnings, make the zero vectors just points.DONE
#Draw x component in blue, y in red.DONE
#Picker for components, grad, or both DONE