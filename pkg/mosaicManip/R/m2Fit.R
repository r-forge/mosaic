m2Fit = function(expr, ..., xlim = c(0,1), ylim = c(0,1)){
  if(!require(manipulate)) stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if (!require("mosaic")) stop("Must install mosaic package.")
#   fillcolors = colorscheme(20,alpha=transparency)  
  vals = list(...) 
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
  
  #image( .xset, .yset, .zset, col=rainbow(npts),add=FALSE,xlab=xlab,ylab=ylab,main=NULL )
         
  #=======================       
    myFun = function(xpt=xpt, ypt=ypt, radius = radius, const=const, xyes=xyes, yyes=yyes, xyyes=xyyes,
                     xsqyes=xsqyes, ysqyes=ysqyes, myalpha=myalpha, nlevels = nlevels, npts=npts,
                     color.scheme=rainbow){
      .xset = seq(min(xlim2),max(xlim2),length=npts)
      .yset = seq(min(ylim2),max(ylim2),length=npts)
      .zset = outer(.xset, .yset, fun$fun ) #.zset is a npts x npts matrix
      minbig = min(.zset)
      maxbig = max(.zset) #THESE LINES NEED TO GO IN myFun if npts is a slider
      
      yMat = outer(.yset, rep(1, npts), "*")
      xMat = outer(rep(1, npts), .xset, "*")
      
      dist = (yMat-ypt)^2+(xMat-xpt)^2
      in.Circle = dist<radius^2
      xvals = xMat[in.Circle]
      yvals = yMat[in.Circle]
      zvals = .zset[in.Circle]
      
      A = c()
      if(const){
        A=cbind(A, rep(1, length(zvals)))
      }
      if(xyes){
        A=cbind(A, xvals)
      }
      if(yyes){
        A=cbind(A, yvals)
      }
      if(xyyes){
        A=cbind(A, xvals*yvals)
      }
      if(xsqyes){
        A=cbind(A, xvals^2)
      }
      if(ysqyes){
        A=cbind(A, yvals^2)
      }

      coefs = qr.solve(A, zvals)
      newvals = A %*% coefs
      zNew = matrix(nrow=npts, ncol=npts)
      zNew[in.Circle] = newvals
      zNew[!in.Circle] = NA
      
      bigstart=.1; bigend=.9
      
      maxsmall = max(newvals)
      minsmall = min(newvals)      
      startparam = min(1,max(0,bigstart+ (bigend-bigstart)*(minsmall-minbig)/(maxbig-minbig)))
      endparam = max(0,min(1,bigend - (bigend-bigstart)*(maxbig-maxsmall)/(maxbig-minbig)))
      print(c(minsmall, maxsmall))
      print(c(minbig,maxbig))
      print( c(startparam, endparam))
      RMS = sqrt(mean((newvals-zvals)^2)*pi*radius^2
      mylevels = pretty(range(.zset),nlevels)
      
      
      image( .xset, .yset, .zset, col=color.scheme(npts, alpha=0.8, start=bigstart, end=bigend),add=FALSE, xlab=xlab,ylab=ylab,main=NULL )
      contour(.xset, .yset, .zset, col=rgb(0,0,0,.4),lwd=3,add=TRUE, labcex=1.2, levels=mylevels)
      image( .xset, .yset, zNew, col=color.scheme(npts, start=startparam, end = endparam,alpha = myalpha),add=TRUE, xlab=xlab,ylab=ylab,main=NULL )
      contour(.xset, .yset, zNew, col="black",lwd=5, labcex=1.5, add=TRUE, 
              levels=mylevels)
      title(main = paste("RMS Error:", signif(RMS, 3)))
    }
    #=========================
manipulate(myFun(xpt=xpt, ypt=ypt, radius = radius, const=const, xyes=xyes, yyes=yyes, xyyes=xyyes, 
                 xsqyes=xsqyes, ysqyes=ysqyes, myalpha=myalpha, nlevels=nlevels, npts = npts, 
                color.scheme=col.scheme),
           xpt = slider(min(xlim2),max(xlim2), initial = mean(xlim2), label = "Circle center: x"),
           ypt = slider(min(ylim2),max(ylim2), initial = mean(ylim2), label = "Circle center: y"),
           radius = slider(.01, (min(max(xlim2),max(ylim2))), initial = .5*mean(xlim2)),
           const = checkbox(TRUE, label = "Constant"),
           xyes = checkbox(TRUE, label = "x"),
           yyes = checkbox(FALSE, label = "y"),
           xyyes = checkbox(TRUE, label = "xy"),
           xsqyes = checkbox(FALSE, label = "x^2"),
           ysqyes = checkbox(FALSE, label = "y^2"),
           myalpha = slider(0,1, initial = .5, label = "Circle Transparency"),
           npts = slider(20, 200, initial = 100, label = "Number of pixels"),
           nlevels = slider(5, 50, initial = 20, label = "Approx. number of contour lines"),
           col.scheme = picker( Rainbow=rainbow,Heat=heat.colors,Terrain=terrain.colors,Topo=topo.colors,CM=cm.colors,initial="Rainbow")

           )
  
}

#         for(j in 1:npts){
#       for(k in 1:npts){
#         if(in.Circle[j,k]==FALSE){
#           yMat[j,k] = NA
#           xMat[j,k] = NA
#         }
#       }
#     }
    
#     coefsy = qr.solve(yMat, zvals)
#     coefsx = qr.solve(xMat, zvals)