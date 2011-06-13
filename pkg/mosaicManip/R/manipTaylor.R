manipTaylor = function(expr, xlim = c(-5, 5), ...){
  #packages 
  require("manipulate")
  require("mosaic")
#functions
  vals = list(...)
  .f = mosaic:::.createMathFun( sexpr=substitute(expr), ...)
  f <<- .f$fun
x = seq(min(xlim),max(xlim), length = 1000)
#Derivatives of function for Taylor Series
dd = list()
dd[[1]] = D(f~x)
dd[[2]] = D(f~x+x)
dd[[3]] = D(f~x+x+x)
dd[[4]] = D(f~x+x+x+x)
dd[[5]] = D(f~x+x+x+x+x)
dd[[6]] = D(f~x+x+x+x+x+x)
dd[[7]] = D(f~x+x+x+x+x+x+x)
dd[[8]] = D(f~x+x+x+x+x+x+x+x)
dd[[9]] = D(f~x+x+x+x+x+x+x+x+x)
dd[[10]] = D(f~x+x+x+x+x+x+x+x+x+x)
dd[[11]] = D(f~x+x+x+x+x+x+x+x+x+x+x)
dd[[12]] = D(f~x+x+x+x+x+x+x+x+x+x+x+x)

myplot = function(a, taylor, lsquares){
  
T = list()
# zeroth order is index 1
T[[1]] = f(a) + 0*x
for (k in 2:13) {
  T[[k]] = T[[k-1]] + (dd[[k-1]](a)*((x-a)^(k-1)))/factorial(k-1)
}

#plotting
mypanel = function(x, y){
  panel.xyplot(x, y, type = "l", col = "black", lwd = 2)
  panel.points(a, f(a))
  panel.points(x, T[[as.numeric(taylor)]], type = "l", col = "red", lwd = 2)
}
x = seq(min(xlim),max(xlim), length = 1000)
xyplot(f(x)~x, panel = mypanel)

}
#manipulate that!
manipulate(myplot(a, taylor, lsquares),
  a = slider(min(xlim),max(xlim), initial = mean(xlim), step = diff(xlim)/100),
  taylor = picker(list(T0=1,T1=2, T2=3, T3=4, T4=5, T5=6, T6=7, T7=8, T8=9, T9=10, T10=11, T11=12, T12=13)),
  lsquares = checkbox(FALSE, "Least Squares")
  
  #otherwise
  #which( taylor == c("T0","T1","T2"
 # taylor = picker("T0","T1","T2","T3","T4","T5","T6","T7","T8", label = "Order of Taylor Approximation"),

  )
  
}