practice.max=function(seed=NULL, n=0) {
  if( !is.null(seed) ) set.seed(seed)
  if (n==0) nmaxes = ceiling( runif(1,min=4,max=10))
  else nmaxes = n
  xlocs = runif( nmaxes, min=-3,max=3 )
  ylocs = runif( nmaxes, min=-3,max=3 )
  signsmax = runif(nmaxes,min=3,max=10)*sign( runif(nmaxes,min=-1,max=1) )
  xscales = runif( nmaxes, min=.1, max=5 )
  f = function(x,y) {
    res=0  
    for(k in 1:nmaxes){
      res = res +  signsmax[k]*exp(-(xscales[k]*(x-xlocs[k])^2 + (y-ylocs[k])^2)/9) 
    }

    return(res)

  }
  return(f)
}