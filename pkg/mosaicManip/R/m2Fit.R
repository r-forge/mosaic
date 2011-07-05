m2Fit = function(expr, ..., xlim = c(0,1), ylim = c(0,1), npts = 50, nlevels = 15){
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
  .xset = seq(min(xlim2),max(xlim2),length=npts)
  .yset = seq(min(ylim2),max(ylim2),length=npts)
  .zset = outer(.xset, .yset, fun$fun ) #.zset is a npts x npts matrix
  
  image( .xset, .yset, .zset, col=rainbow(npts),add=FALSE,
         xlab=xlab,ylab=ylab,main=NULL )

  contour(.xset, .yset, .zset, col=par("fg"),lwd=2,add=TRUE, nlevels=nlevels)
  #=======================       
    myFun = function(xpt=xpt, ypt=ypt, radius = radius, const=const, xyes=xyes, yyes=yyes, xyyes=xyyes, xsqyes=xsqyes, ysqyes=ysqyes){
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
      browser()
      coefs = qr.solve(A, zvals)
      newvals = A %*% coefs
      zNew = matrix(nrow=npts, ncol=npts)
      zNew[in.Circle] = newvals
      zNew[!in.Circle] = NA
      
      RMS = sqrt(mean(newvals-zvals)^2)
     # image( .xset, .yset, .zset, col=rainbow(npts),add=TRUE, xlab=xlab,ylab=ylab,main=NULL )
      contour(.xset, .yset, zNew, col=par("fg"),lwd=5,add=TRUE, nlevels=nlevels, main = paste("RMS Error:", signif(RMS, 3)))

    }
    #=========================
manipulate(myFun(xpt=xpt, ypt=ypt, radius = radius, const=const, xyes=xyes, 
                 yyes=yyes, xyyes=xyyes, xsqyes=xsqyes, ysqyes=ysqyes),
           xpt = slider(min(xlim2),max(xlim2), initial = mean(xlim2), label = "Circle center: x"),
           ypt = slider(min(ylim2),max(ylim2), initial = mean(ylim2), label = "Circle center: y"),
           radius = slider(.01, .5*(min(max(xlim2),max(ylim2))), initial = .5*mean(xlim2)),
           const = checkbox(TRUE, label = "Constant"),
           xyes = checkbox(TRUE, label = "x"),
           yyes = checkbox(FALSE, label = "y"),
           xyyes = checkbox(TRUE, label = "xy"),
           xsqyes = checkbox(FALSE, label = "x^2"),
           ysqyes = checkbox(FALSE, label = "y^2")
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