mBias=function(expr, data){
  if (!require(manipulate) | !require(lattice) | !require(grid)| !require(mosaic)) stop("Must have manipulate package.")
  
  A1=FALSE; A2=FALSE; A3=FALSE; A4=FALSE; A5=FALSE; A6=FALSE; A7=FALSE; 
           A8=FALSE; A9=FALSE; A10=FALSE; A11=FALSE; A12=FALSE; A13=FALSE; A14=FALSE;
           A15=FALSE; A16=FALSE; A17=FALSE; A18=FALSE; A19=FALSE; A20=FALSE
  checks=c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                 A15, A16, A17, A18, A19, A20)
  origMod=lm(expr, data)
  origCoefs=coef(origMod)
  yvar=as.character(expr[2])
  xvars.mod=attr(origMod$terms, "term.labels")   #xvars from original model
  
  xvars.data=names(data)
  xvars.data=xvars.data[which(xvars.data!=yvar)]  #all other variables not y in data
  #=============================myFun Starts!
  myFun=function(n, seed, checks){
    set.seed(seed)
    browser()
    newData=resample(data, n)
    newResids=resample(resid(origMod), n)
    newData=newData[which(checks)+1]
    newData[[yvar]]=predict(origMod, newdata=newData)
    newData[[yvar]]=newData[[yvar]]+newResids
    newMod=lm(newData[[yvar]]~., data=newData)    #newMod from newData with only selected terms
    newCIs=confint(newMod)
    limCIs=c(min(newCIs), max(newCIs))
    limCIs=c(min(newCIs)-.1*diff(range(limCIs)), max(newCIs)+.1*diff(range(limCIs)))
    levelNames=rownames(newCIs)

    coefs=c(rep(0, length(levelNames)))
    coefs[which(levelNames==names(origCoefs))]=origCoefs[which(levelNames==names(origCoefs))]
    
#    coefs[which(levelNames!=names(origCoefs))]=0
#     for(z in 1:length(levelNames)){      #coefs is a vector of origCoefs with indices same as newData
#       if(levelNames[z]==names(origCoefs)){
#         coefs[z]=origCoefs[which(levelNames[z]==names(origCoefs))]
#       }
#       else
#         coefs[z]=0
#     }
#     coefs[[names(newData)]]=0
#     coefs[[names(origCoefs)]]=origCoefs[[names(origCoefs)]]
    browser()
    plot( 1:2, type="n", ylim=.5+c(0,nrow(newCIs)),xlim=c(0,1),xaxt="n",yaxt="n",
         ylab="",xlab="",bty="n")
    
    #=====================Begin draw.CI.axis function
    draw.CI.axis = function( true, low, high,k,name ){
      left = min(true,low) - 0.66*abs(high-low)
      right = max(true,high) + 0.66*abs(high-low)
      ticks = signif(pretty( c(left,right), n=5),5)
      axis( side=1,pos=k,at=seq(0,1,length=length(ticks)),
          labels=paste(ticks))
      # convert the units
      axis.left = min(ticks)
      axis.right = max(ticks)
      do.convert = function(val,left,right){ (val - left)/abs(right-left)}
      true = do.convert(true, axis.left, axis.right)
      low = do.convert(low, axis.left, axis.right)
      high = do.convert(high, axis.left, axis.right)
      lines(c(true,true),k+c(-.2,.2),lty=3,col="black")
      points(mean(c(low,high)),k, pch=20,col="red",cex=2)
      text( c(low,high),c(k,k),c("(",")"),col="red")
      lines( c(low,high),c(k,k),lwd=3,col=rgb(1,0,0,.5))
      text( 0.05, k, name, pos=3)
    }
    #=====================End draw.CI.axis function
    for(b in 1:nrow(newCIs)){
      draw.CI.axis( coefs[b], newCIs[b,1], newCIs[b,2], b, levelNames[b] )
    }

  }
  #=========================myFun ends noooooo
  controls=list(n=slider(5, 500, step=1, initial=100, label="n points to sample"),
                seed=slider(1,100,step=1, initial=sample(100,1), label="Random seed")
                )
  for(a in 1:length(xvars.data)){
    controls[[paste("A", a, sep="")]]=checkbox(FALSE, label=xvars.data[a])
  }
  manipulate(myFun(n=n, seed=seed, checks=c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                 A15, A16, A17, A18, A19, A20)),
             controls)
#Fix levelNames. It might be something with checks
#Fix newData so it works with the sampling for n pts.   
}