D <-
function (f, n = 1, h = 5e-05) 
{
    n = as.integer(n)
    if (!(is.integer(n) && n > 0)) {
        warning("n must be a non-negative integer.")
    }
    if (n == 0) {
        return(f)
    }
    if (n == 1) {
        return(function(x) {
            (f(x + h) - f(x))/h
        })
    }
    return(D(f, n = n - 1, h = h))
}
