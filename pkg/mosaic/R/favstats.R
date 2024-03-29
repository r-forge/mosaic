favstats <-
function (x, na.rm = TRUE) 
{
    qq <- quantile(x, na.rm = na.rm)
    val <- data.frame(qq[1],  qq[2], qq[3], qq[4], qq[5],
			mean(x, na.rm = na.rm), 
			sd(x, na.rm = na.rm), 
			sum(! is.na(x)),
			sum( is.na(x) )
			)
	rownames(val) <- ""
    names(val) <- c("min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing")
    return(val)
}
