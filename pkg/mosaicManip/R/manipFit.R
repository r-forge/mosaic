mFit = function(data, expr=NULL, ...){
  f = list()
  f[[1]] = function(x,...) rep.int(1, length(x))
  f[[2]] = function(x,...) x
  f[[3]] = function(x,...) x^2
  f[[4]] = function(x,...) x^3
  f[[5]] = function(x,...) log(abs(x)+.000001)
  f[[6]] = function(x, k, ...) exp(k*x)
  f[[7]] = function(x, P, ...) sin(2*pi*x/P)
  f[[8]] = function(x, P, ...) cos(2*pi*x/P)
  f[[9]] = function(x, mu, sd, ...) pnorm(q = x, mean = mu, sd = sd)

  
   labels= c("Constant", "x", "x^2", "x^3", "log(x)", "exp(kx)", "sin(2Pi*x/P)", "cos(2Pi*x/P)", "pnorm")
 #####  
  myPlot = function(xvar, yvar, k, n, P, mu, sd, choice){
   xvals = data[[xvar]]
   yvals = data[[yvar]]

   x = seq(min(xvals),max(xvals), length = 1000)
  
   if( sum(funchoice)==0) {
     print("You must select at least one function to fit a curve!")
     bigy = 0*x
   }
   else{
     A = matrix(nrow=length(xvals),ncol = sum(funchoice))
     for (fun.k in which(funchoice)) {
       A[,fun.k] = f[[fun.k]](xvals,k=k,P=P,mu=mu,sd=sd)
     } 
     coefs = qr.solve(A, yvals)
  
     bigA = matrix(nrow=length(x),ncol = sum(funchoice))
     for (fun.k in which(funchoice)) {
       bigA[,fun.k] = f[[fun.k]](x,k=k,P=P,mu=mu,sd=sd)
     }
       browser()
     bigy = bigA %*% coefs
   }
   
  
    mypanel = function(x, y){
      panel.xyplot(x, y)
      panel.xyplot(xvals, bigy, type = "l", col="red")
      }
  #PLOTTING F'REAL
     xyplot(yvals~xvals, data, xlab = xvar, ylab = yvar, panel = mypanel)

  }
 #####
  
   
 funchoice = c(a1=TRUE,a2=FALSE,a4=FALSE,a5=FALSE,a6=FALSE,a7=FALSE,a8=FALSE,a9=FALSE)
   manipulate(myPlot(xvar=xvar, yvar=yvar, k=k, n=n, P=P, mu=mu, sd=sd, choice = choice),
            xvar = picker(as.list(names(data)), label = "X Variable"),
            yvar = picker(as.list(names(data)), label = "Y Variable"),
            choice = checkbox(funchoice, labels),
            k=slider(-2,2, step = .01, initial=0.1),
            P=slider(-10,10, step = .1),
            mu = slider(-20,20, step =.1, initial = 0),
            sd = slider(1, 20, step = .1, initial = 3)
            )
}
#