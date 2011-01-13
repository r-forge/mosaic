D <- function(f,h=1e-4) {
  function(x, ... ) {
    # setting h
    temp = x+h
    h = x - temp
    # finite difference value: rise over run
    return( (f(x+h,...) - f(x, ...) )/h )
  }
}

