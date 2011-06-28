 mDerivs <-
function(expr, xlim=c(0,10), ...) {
#packages 
  if (!require("manipulate")) stop("Must run in a manipulate compatible system, e.g. RStudio")
  if (!require("mosaic")) stop("Must install mosaic package.")
#functions
  vals = list(...)
  .f = mosaic:::.createMathFun( sexpr=substitute(expr), from=0, ...)

  f <- .f$fun
  # Use the expression in the calculating the derivative so that 
  # it can be done symbolically, if possible
  dfdx <- mosaic:::.doD( .f, ...)
  # Try to do the second derivative symbolically, also
  .ff = .f
  .ff$RHS = c("+", .f$RHS, .f$RHS)
  # That the next variable must be global, suggests that mosaic:::.doD should be changed
  # to include its own environment as an (formal) argument to the returned function.
  d2fd2x <<- mosaic:::.doD( .ff, ...) # must be global to work with derivative evaluation prgm. That's a bug!
  d3fd3x <- D(d2fd2x(x)~x, ...)
  antiF <- antiD(f(x)~x,...)
  fset = list(d3fd3x,d2fd2x,dfdx,f,antiF)
  # Labels for the graph
  f.labels = list(expression(partialdiff^3*f/partialdiff^3*x),
                  expression(partialdiff^2*f/partialdiff^2*x), 
                  expression(partialdiff*f/partialdiff*x),
                  expression(f(x)), 
                  expression(integral(f(x)*dx)))
  #colors used in the display
  deriv.color2 = rgb(1,0,0,.2)#red, transparent
  deriv.color = "red" 
  integral.color2 = "blue"
  integral.color= rgb(0,0,1,1)
  integral.color.trans = rgb(0,0,1,.3)
  neg.integral.color2 = "darkslateblue"
  neg.integral.color = rgb(.4,0,1,.3)
  neg.integral.line.color = rgb(.4,0,1,.3)
  gray = rgb(0,0,0,.3)
  
#Greater than 0 function
  positive=function(ha){
    ha>=0
  }
#vplayout for easier layout movement
    vplayout = function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
    }
get.aspect.ratio = function() {
  cat("getting into aspect ratio")
   y = convertUnit( unit(1,"native"),"cm",typeFrom="dimension", axisFrom="y",axisTo="y",valueOnly=TRUE)
   x = convertUnit( unit(1,"native"),"cm",typeFrom="dimension", axisFrom="x",axisTo="x",valueOnly=TRUE)
  cat("and out\n") 
  return(y/x)
}
# ====================
myplot= function(xpos, from, der, anti, fixed, middleFun=NULL){
  # choose the functions to display from the set
  dfdx = fset[[middleFun-1]]
  f = fset[[middleFun+0]]
  antiF = fset[[middleFun+1]]
  vals = list(...)
  dfpanel = function(x, y){
    panel.xyplot(x, y, type="l", lwd = 2, col = deriv.color, ...)
    panel.points(xpos, dfdx(xpos))
    panel.abline(h=0, lty = "dotted")
    panel.lines(x=c(xpos, -9000000), y = c(dfdx(xpos),dfdx(xpos)), col=deriv.color2, lwd = 11)
    grid.text(label=round(dfdx(xpos), 3), x = unit(0, "npc")+unit(10,"mm"), y = unit(dfdx(xpos),"native"), gp = gpar(col = deriv.color, fontsize =10))
    grid.text(f.labels[[middleFun-1]], 
              x=unit(0.075,"npc"), y=unit(0.9,"npc"), hjust="left" )
  }
  # =============
  fpanel = function(x, y){
   newx = x[x < max(xpos, from) & x>min(xpos,from)]
   xpts = c(min(xpos,from),newx,max(xpos,from))
   ypts = c(0,f(newx),0)
   ypos = pmax(0, ypts)
   yneg = pmin(0, ypts)
   if(xpos<from){
     panel.polygon(xpts,ypos,col=neg.integral.color)
     panel.polygon(xpts,yneg,col=integral.color.trans)
   }
   else{
      panel.polygon(xpts,ypos,col=integral.color.trans)
      panel.polygon(xpts,yneg,col=neg.integral.color)}
    panel.xyplot(x, y, type="l", col = "black", lwd = 2, ...)
    panel.points(xpos, f(xpos))
    panel.points(from, f(from))
    panel.abline(h=0, lty = "dotted") 
    grid.text(f.labels[[middleFun+0]], 
              x=unit(0.075,"npc"), y=unit(0.9,"npc"), hjust="left" )
    
    yline = f(xpos)+ dfdx(xpos)*(x - xpos)
    halfwidth=diff(range(x))/3
    segEndsX = xpos + halfwidth*c(-1,1)
    segEndsY = f(xpos) + dfdx(xpos)*halfwidth*c(-1,1)
    panel.lines(segEndsX, segEndsY, col=deriv.color2, lwd=10)
    #panel.lines(x, yline, col=deriv.color2, lwd = 10)
    panel.lines(x=c(xpos, -9000000), y = c(f(xpos),f(xpos)), col=rgb(0,0,0,.3), lwd = 11)
    #Below was old slope text:
    #grid.text(label=paste("Slope = ", signif(dfdx(xpos), 3)), x = unit(1, "npc")-unit(15,"mm"), y = unit(1,"npc")-unit(3,"mm"), gp = gpar(col = deriv.color, fontsize =10)) 

     grid.text(label=paste("Slope = ", signif(dfdx(xpos), 3)), x = unit(xpos+halfwidth/2, "native"), y = unit(f(xpos)+dfdx(xpos)*halfwidth/2,"native"), rot = atan(dfdx(xpos)*get.aspect.ratio())*180/pi, gp = gpar(col = deriv.color, fontsize =10))
     grid.text(label=round(f(xpos), 3), x = unit(0, "npc")+unit(10,"mm"), y = unit(f(xpos),"native"), gp = gpar(col = "black", fontsize =10))
   }
  #======== end of Fpanel =============== 
  antiFpanel = function(x, y){
    panel.xyplot(x, y, type="l", lwd = 2, col =integral.color, ...)
    tupac = subset(data.frame(x,y,pos = positive(y)),pos==FALSE)
    #tupac = subset(tupac, pos == FALSE)
    panel.xyplot(tupac$x, tupac$y, type = "p", pch = ".", cex = 2, col = "purple")
    at.val = antiF(xpos, from=from)
    from.val = antiF( from, from=from)
    panel.points(xpos, antiF(xpos, from = from))
    panel.points(from, antiF(from, from = from), col = "black")
    panel.abline(h=0, lty = "dotted")
    halfwidth=diff(range(x))/10
    # Change this to draw the line over the whole y-scale, 
    # maybe going a little bit past FIX FIX FIX FIX
    segEndsX = xpos + halfwidth*c(-1,1)
    segEndsY = antiF(xpos, from = from) + f(xpos)*halfwidth*c(-1,1)
    # The following should be named "gray"  FIX FIX FIX FIX
    panel.lines(segEndsX, segEndsY, col=gray, lwd = 10)
    if(at.val < 0)
    panel.lines(x=c(xpos, -9000000), y = c(at.val, at.val), col=neg.integral.line.color, lwd = 11)
    else
    panel.lines(x=c(xpos, -9000000), y = c(at.val, at.val), col=rgb(0,0,1,.3), lwd = 11)
   
    grid.text(f.labels[[middleFun+1]], 
              x=unit(0.075,"npc"), y=unit(0.9,"npc"), hjust="left" )
    grid.text(label=paste("Slope = ", signif(f(xpos), 3)), x = unit(xpos+halfwidth/2, "native"), y = unit(at.val+f(xpos)*halfwidth/2,"native"), rot = atan(f(xpos)*get.aspect.ratio())*180/pi, gp = gpar(col = "black", fontsize =10))
    grid.text(label=round(at.val, 3), x = unit(0, "npc")+unit(10,"mm"), y = unit(at.val,"native"), gp = gpar(col = integral.color, fontsize =10))
  }
  #========== end of antiFpanel =========== 

  ##create dataframe of plotting values:
  x = seq(min(xlim), max(xlim),length=1000)
  dat = data.frame(x=x, 
                   f = f(x), 
                   dfdx = dfdx(x), 
                   antiF = antiF(x, from = from))
                   
top.plot = if( diff(range(dat$dfdx))< .001){ # just the ylim part differs
  xyplot(dfdx~x, data = dat, type = "l",   
          ylab = NULL, xlab = NULL, scales = list(x = list(draw = FALSE)), 
          col = deriv.color, panel = dfpanel,
          ylim=c(mean(dat$dfdx)+c(-.001,.001)))
}
else {
   xyplot(dfdx~x, data = dat, type = "l", 
          ylab = NULL, xlab = NULL, scales = list(x = list(draw = FALSE)), 
          col = deriv.color, panel = dfpanel)
}
middle.plot = xyplot(f~x, data = dat, type = "l", 
                     ylab = NULL,
                     xlab = NULL, scales = list(x = list(draw = FALSE)), 
                     col = "black", panel = fpanel)
bottom.plot = xyplot(antiF~x, data = dat, type = "l", 
                     ylab = NULL,  # see indiv. panel functions
                     panel = antiFpanel)  

if(fixed == TRUE)
  { 
    yparam = c(max(antiF(x, from = min(xlim))), min(antiF(x, min(xlim))))
    diff = diff(range(yparam))
    yparam = c((min(antiF(x, from = min(xlim))))-diff/3, (max(antiF(x, from = min(xlim))))+diff/3)
    cc = xyplot(antiF~x, data = dat, type = "l", ylim = yparam, ylab = "Antiderivative of f", panel = antiFpanel)  
  }

if(der == TRUE){
  print(top.plot, position = c(0.0, 0.65, 1, 1), more = TRUE)
}
# Always plot out the function itself                  
print(middle.plot, position = c(0.0, 0.34, 1, 0.69), more = anti) 
  # The more=anti handles the case when anti is FALSE, 
  # and we don't want to continue drawing
if(anti == TRUE){
  print(bottom.plot, position = c(0.0,0,1,0.40))
}
}
 #Making the plot
  manipulate(myplot(xpos, from, der, anti, fixed,middleFun=cfun),
    cfun = picker( "Second Deriv"=2,"First Deriv"=3,"Given Function"=4,
                   initial="Given Function",label="Display in middle"),
    xpos=slider(min(xlim),max(xlim),initial=mean(xlim),step=diff(xlim)/100),
    from = slider(min(xlim),max(xlim), initial = min(xlim), step = diff(xlim)/100),
    der = checkbox(TRUE, "Display Derivative"),
    anti = checkbox(TRUE, "Display Antiderivative"),
    fixed = checkbox(FALSE, "Fix Antiderivative y axis")
            )

}
#Border off on polygons
 #Polygon logic, for antiderivs
 #Red Parabola for antideriv - curvature - SHORT
 #Make deriv labels bigger
 #Slope labels, in bounds using aspect ratio function
