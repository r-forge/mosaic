mFit = function(data, expr=NULL, ...){

  f1 = function(x) rep.int(1, length(x))
  f2 = function(x) x
  f3 = function(x) x^2
  f4 = function(x) x^3
  f5 = function(x) log(x)
  f6 = function(x, k) exp(k*x)
  f7 = function(x, P) sin(2*pi*x/P)
  f8 = function(x, P) cos(2*pi*x/P)
  f9 = function(x, mu, sd) pnorm(q = x, mean = mu, sd = sd)
  
 #####  
  myPlot = function(xvar, yvar, k, n, P, mu, sd){
   xvals = data[[xvar]]
   yvals = data[[yvar]]

   x = seq(min(xvals),max(xvals), length = 1000)
  
   A = matrix(nrow=length(xvals),ncol = 9)
   A[,1] = f1(xvals)
   A[,2] = f2(xvals)
   A[,3] = f3(xvals)
   A[,4] = f4(xvals)
   A[,5] = f5(xvals)
   A[,6] = f6(x=xvals, k=k)
   A[,7] = f7(x=xvals, P=P)
   A[,8] = f8(x=xvals, P=P)
   A[,9] = f9(x=xvals, mu=mu, sd=sd)
   browser()   
   coefs = qr.solve(A, yvals)

   
  #PLOTTING F'REAL
     xyplot(xvals~yvals, data, xlab = xvar, ylab = yvar)

  }
 #####
 manipulate(myPlot(xvar=xvar, yvar=yvar, k=k, n=n, P=P, mu=mu, sd=sd),
            xvar = picker(as.list(names(data)), label = "X Variable"),
            yvar = picker(as.list(names(data)), label = "Y Variable"),
            k=slider(-10,10, step = .1),
            P=slider(-10,10, step = .1),
            mu = slider(-100,100, step =.1, initial = 50),
            sd = slider(1, 20, step = .1, initial = 3)
            )
}
#