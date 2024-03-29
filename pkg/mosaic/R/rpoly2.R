# generates a random 2nd degree poly of 2 vars (as a function)

rpoly2 <- function(seed=NULL){
  if( !is.null(seed) ) set.seed(round(seed))
  coefs = runif(5,min=-1,max=1)
  f = function(x,y){
    x*coefs[1]+y*coefs[2]+x*y*coefs[3]+x^2*coefs[4] + y^2*coefs[5]
  }
  return(f)
}
