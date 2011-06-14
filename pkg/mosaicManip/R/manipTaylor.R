manipTaylor = function(expr, xlim = c(-5, 5), ...){
  #packages 
  require("manipulate")
  require("mosaic")
#functions
   vals = list(...)
   fm = mosaic:::.createMathFun( sexpr=substitute(expr), ...)
   f = fm$fun
x = seq(min(xlim),max(xlim), length = 1000)
#colors
trans.blue = rgb(0,0,1,.1) #least squares rectangle color
line.blue = rgb(0,0,1,.5) #least squares line color
line.red = rgb(1,0,0,.5) #Taylor line color
#Derivatives of function for Taylor Series
dd = list()
dd[[1]] = mosaic:::.d.symbolic(fm)
var = fm$RHS
for(k in 2:10){
  fm$RHS = c("+", var, fm$RHS)
dd[[k]] = mosaic:::.d.symbolic(fm)
}

myplot = function(a, TaylorBeTrue, lsquares, xwid, n){
  #Taylor Series
T = list()
# zeroth order is index 1
T[[1]] = f(a) + 0*x
for (k in 2:11) {
  T[[k]] = T[[k-1]] + (dd[[k-1]](a)*((x-a)^(k-1)))/factorial(k-1)
}

#Least Squares: Row 1 is order 0
myx = seq(a-xwid/2, a+xwid/2, length = 10*n+1)
A = outer(myx, 0:n, "^")
coefs = qr.solve(A, f(myx))
lsq.func = coefs[[1]]
for(j in 2:n)
{lsq.func = lsq.func+x^(j-1)*coefs[[j]]
}

#plotting
mypanel = function(x, y){
  panel.xyplot(x, y, type = "l", col = "black", lwd = 2)
  if(lsquares==TRUE){
    panel.xyplot(x, lsq.func, type = "l", col = line.blue, lwd = 5)
    panel.rect(xright=min(myx), xleft=max(myx), ybottom=-999999, ytop = 999999, col = trans.blue) 
  }
  panel.points(a, f(a), cex = 2)
  if(TaylorBeTrue==TRUE){
  panel.points(x, T[[as.numeric(n)]], type = "l", col = line.red, lwd = 5)
  }
}
x = seq(min(xlim),max(xlim), length = 1000)
xyplot(f(x)~x, panel = mypanel)

}
#manipulate that!
manipulate(myplot(a, TaylorBeTrue, lsquares, xwid, n),
  a = slider(min(xlim),max(xlim), initial = mean(xlim), step = diff(xlim)/100, label = "Position"),
  TaylorBeTrue = checkbox(FALSE, "Activate Taylor Series"),
  n = picker(1,2,3,4,5,6,7,8,9,10,11, label = "Order of the Polynomial"),
  lsquares = checkbox(FALSE, "Activate Least Squares"),
  xwid = slider(0, diff(range(xlim)), initial = mean(diff(range(xlim)))/2, step = diff(xlim/100), label = "Least Squares width")
  
  )  
}