#The following activity gets students thinking about
#what the confidence level means.  Is there a way to
#use the manipulate function so they can repeatedly
#press a button to get
#a new sample, but also use a slider to vary the level of
#confidence?
ciButton = function(){
  Wald=function(p.hat, n, conf.level, sd=sqrt(p.hat*(1-p.hat))){
    error = (qnorm(.5+conf.level/2)*sd)/(sqrt(n))
    return(list(lower = p.hat-error, upper = p.hat+error))
  }
  Agresti= function(p.hat, n, conf.level){
    sd=sqrt(p.hat*(1-p.hat))
    ntilde = n+qnorm(.5+conf.level/2)
    lower = p.hat-qnorm(.5+conf.level/2)*sqrt(sd/ntilde)
    upper = p.hat+qnorm(.5+conf.level/2)*sqrt(sd/ntilde)
    return(list(lower, upper))
  }

myFun=function(n=n, conf.level=0.95,p=p,ntrials=10,seed=125, int.type=Agresti){
  set.seed(seed)


mypanel=function(x,y){
  outside = 0
  for (k in (ntrials:1) ) {
    p.hat <- rbinom(1,size=n,prob=p)/n
    int <- int.type(p.hat=p.hat,n=n,conf.level=conf.level)
    lower.bound <- int[1]
    upper.bound <- int[2]
    panel.abline(v=p, col = "red")
    lsegments(0, k, 1, k)
    lsegments(x0=lower.bound,y0=k,x1=upper.bound,y1=k, lwd=5)
   # ltext(c(lower.bound, upper.bound), c(k,k), c("(",")"))    OPTIONAL PARENTHENSES ARROWS
    lpoints(p.hat, k, pch = 16)
    if(p<lower.bound|p>upper.bound){
      lpoints(1.02, k, pch = 11, col = "chocolate1", cex = 1.5)
      outside=outside+1
    }
      
  }
  popViewport()
  grid.text(paste("Total failed CIs:", outside), 
            x=unit(.5, "npc"),
            y=unit(.98, "npc"),
            just = "center",
            gp = gpar(fontsize=15, col="chocolate1"))
}


xyplot(0:1 ~ 0:1, panel = mypanel, type="n",
       ylim=rev(c(0,max(ntrials+1,20))),
       xlim=c(-.1, 1.1),
       ylab="Trial Number",
       xlab="Probability")



# plot(0:1,0:1,axes=FALSE,ann=FALSE,type="n")
# segments(0,0.5,1,0.5)
# segments(x0=int[1],y0=0.5,x1=int[2],y1=0.5,lwd=3)
# text(0,0.4,0)
# text(1,0.4,1)
# text(c(lower.bound,upper.bound),rep(0.5,times=2),
#   c("(",")"))
# points(p.hat,0.5,pch=20)
# segments(p,0.3,p,0.7)
# text(p,0.2,"p")
}
#==========
manipulate(myFun(n=n, conf.level=conf.level,p=p, ntrials=ntrials, seed=seed, int.type=Agresti),
           n = slider(5, 500, step = 1, initial=100),
           conf.level = slider(.01, 1.00, step = .01, initial=0.95),
           p = slider(0,1, step = .01, initial=0.8),
           ntrials = slider(1, 100, step = 1, initial = 1),
           seed = slider(100,200, step=1, initial=125),
           int.type = picker("Agresti"=Agresti,"Wald"=Wald, label = "Type of CI")
           )
}