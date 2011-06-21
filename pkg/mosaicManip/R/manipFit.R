mFit = function(data, expr=NULL, ...){

  
   
  myPlot = function(xvar, yvar, k, n, P, mu, sad){
   xvals = data[[xvar]]
   yvals = data[[yvar]]

   x = seq(min(xvals),max(xvals), length = 1000)
   A = matrix(nrow=length(xvals),ncol = 9)
   A[,1] = rep.int(1,length(x))
   A[,2] = x
   A[,3] = x^2
   A[,4] = x^3
   A[,5] = log(x)
   A[,6] = exp(k*x)
   A[,7] = sin(2*pi*x/P)
   A[,8] = cos(2*pi*x/P)
   A[,9] = pnorm(q = x, mean = mu, sd = 5)
   
   coefs = qr.solve(A, yvals)
   browser()
   
  #PLOTTING F'REAL
     xyplot(xvals~yvals, data, xlab = xvar, ylab = yvar)

  }
 
 manipulate(myPlot(xvar, yvar, k, P, mu, sad),
            xvar = picker(as.list(names(data)), label = "X Variable"),
            yvar = picker(as.list(names(data)), label = "Y Variable"),
            k=slider(-10,10, step = .1),
            P=slider(-10,10, step = .1),
            mu = slider(-100,100, step =.1),
            sad = slider(1, 20, step = .1, initial = 3)
            )
}
#min(data[[xvar]],max(data[[xvar]])