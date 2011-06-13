manipDerivs = function(expr, xlim=c(0,10), ...) {
#packages 
  require("manipulate")
  require("mosaic")
#functions
  vals = list(...)
  .f = mosaic:::.createMathFun( sexpr=substitute(expr), ...)
  f <<- .f$fun
  dfdx <<- newD(f(x)~x,...)  #make these globals to use in plotFun
  antiF <<- newAntiD(f(x)~x,...) # ditto
#colors
   deriv.color2 = rgb(1,0,0,.2)#red, transparent
  deriv.color = "red" 
  integral.color2 = "blue"
  integral.color= rgb(0,0,1,1)
  integral.color.trans = rgb(0,0,1,.6)
   neg.integral.color2 = "darkslateblue"
  neg.integral.color = rgb(.4,0,1,.6)
  
#Greater than 0 function
  positive=function(ha){
    ha>=0
  }
#vplayout for easier layout movement
    vplayout = function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
    }
  
myplot= function(xpos, from, der, anti, fixed){

  vals = list(...)
 dfpanel = function(x, y){
    panel.xyplot(x, y, type="l", lwd = 2, col = deriv.color, ...)
    panel.points(xpos, dfdx(xpos))
    panel.abline(h=0, lty = "dotted")
    panel.lines(x=c(xpos, -9000000), y = c(dfdx(xpos),dfdx(xpos)), col=deriv.color2, lwd = 11)
    grid.text(label=round(dfdx(xpos), 3), x = unit(0, "npc")+unit(10,"mm"), y = unit(dfdx(xpos),"native"), gp = gpar(col = deriv.color, fontsize =10))
  }
  
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
    #x10 = subset(x, x>(xpos-diff(x)/10)&&x<(xpos+diff(x)/10))
    
    yline = f(xpos)+ dfdx(xpos)*(x - xpos)
    halfwidth=diff(range(x))/10
    segEndsX = xpos + halfwidth*c(-1,1)
    segEndsY = f(xpos) + dfdx(xpos)*halfwidth*c(-1,1)
    panel.lines(segEndsX, segEndsY, col=deriv.color2, lwd=10)
    #panel.lines(x, yline, col=deriv.color2, lwd = 10)
    panel.lines(x=c(xpos, -9000000), y = c(f(xpos),f(xpos)), col=rgb(0,0,0,.3), lwd = 11)
    grid.text(label=paste("Slope = ", signif(dfdx(xpos), 3)), x = unit(1, "npc")-unit(15,"mm"), y = unit(1,"npc")-unit(3,"mm"), gp = gpar(col = deriv.color, fontsize =10))
  #  rot = atan(dfdx(xpos))*180/pi,
     grid.text(label=round(f(xpos), 3), x = unit(0, "npc")+unit(10,"mm"), y = unit(f(xpos),"native"), gp = gpar(col = "black", fontsize =10))
   }
    
  antiFpanel = function(x, y){
   panel.xyplot(x, y, type="l", lwd = 2, col =integral.color, ...)
   tupac = data.frame(x,y,pos = positive(y))
    tupac = subset(tupac, pos == FALSE)
    panel.xyplot(tupac$x, tupac$y, type = "p", pch = ".", cex = 2, col = "purple")
    panel.points(xpos, antiF(xpos, from = from))
    panel.points(from, antiF(from, from = from), col = "black")
    panel.abline(h=0, lty = "dotted")
    halfwidth=diff(range(x))/10
    segEndsX = xpos + halfwidth*c(-1,1)
    segEndsY = antiF(xpos, from = from) + f(xpos)*halfwidth*c(-1,1)
    panel.lines(segEndsX, segEndsY, col=rgb(0,0,0,.3), lwd = 10)
    panel.lines(x=c(xpos, -9000000), y = c(antiF(xpos,from = from),antiF(xpos, from=from)), col=rgb(0,0,1,.3), lwd = 11)
    grid.text(label=paste("Slope = ", signif(f(xpos), 3)), x = unit(1, "npc")-unit(15,"mm"), y = unit(1,"npc")-unit(3,"mm"), gp = gpar(col = "black", fontsize =10))
     grid.text(label=round(antiF(xpos), 3), x = unit(0, "npc")+unit(10,"mm"), y = unit(antiF(xpos, from = from),"native"), gp = gpar(col = integral.color, fontsize =10))
  }
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3,1)))  
  pushViewport(viewport(layout.pos.row = 2, name = "B"))
  upViewport()
  pushViewport(viewport(layout.pos.row = 3, name = "C"))
  upViewport()
  pushViewport(viewport(layout.pos.row = 1, name = "A"))


##create dataframe:
  x = seq(min(xlim), max(xlim),length=1000)
  dat = data.frame(x=x, 
                   f = f(x), 
                   dfdx = dfdx(x), 
                   antiF = antiF(x, from = from))
                   

a = xyplot(dfdx~x, data = dat, type = "l", ylab = "df/dx", xlab = NULL, scales = list(x = list(draw = FALSE)), col = deriv.color, panel = dfpanel)
b = xyplot(f~x, data = dat, type = "l", xlab = NULL, scales = list(x = list(draw = FALSE)), col = "black", panel = fpanel)
cc = xyplot(antiF~x, data = dat, type = "l", ylab = "Antiderivative of f", panel = antiFpanel)  

  if(fixed == TRUE)
  { 
    yparam = c(max(antiF(x, from = min(xlim))), min(antiF(x, min(xlim))))
    diff = diff(yparam)
    yparam = c((min(antiF(x, from = min(xlim))))-diff/3, (max(antiF(x, from = min(xlim))))+diff/3)
    cc = xyplot(antiF~x, data = dat, type = "l", ylim = yparam, ylab = "Antiderivative of f", panel = antiFpanel)  
  }

 if(der == TRUE){
print(a, newpage = FALSE)

}
upViewport(2)
 
downViewport("B")
print(b, newpage = FALSE)
upViewport()
if(anti == TRUE){
 downViewport("C")
print(cc, newpage = FALSE)
}
}
 #Making the plot
  manipulate(myplot(xpos, from, der, anti, fixed),
    xpos=slider(min(xlim),max(xlim),initial=mean(xlim),step=diff(xlim)/100),
    from = slider(min(xlim),max(xlim), initial = min(xlim), step = diff(xlim)/100),
    der = checkbox(TRUE, "Display Derivative"),
    anti = checkbox(TRUE, "Display Antiderivative"),
    fixed = checkbox(FALSE, "Fix Antiderivative y axis")
            )

}