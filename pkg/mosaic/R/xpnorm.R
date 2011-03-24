.plotnorm <- function(p, q, mean, sd, xlim, ylim, digits=4, ...) {
	z <- (q - mean) / sd
			z <- (q - mean) / sd 
	zmax = max(4, abs(z) * 1.6)
	if (missing(xlim)) {
		xlim = mean + c(-1, 1) * zmax * sd
	}
	ymax = dnorm(mean, mean = mean, sd = sd)
	if (missing(ylim)) {
		ylim = c(0, 1.4 * ymax)
	}
	xdata = rep(xlim, each = 2)
	ydata = rep(ylim, times = 2)

	plot <- xyplot(ydata ~ xdata, xlim = xlim, ylim = ylim, 
		xlab = "", ylab = "density", 
		panel = function(x, y, ...) {
			panel.mathdensity(dmath = dnorm, args = list(mean = mean, 
			  sd = sd), lwd = 2, n = 100, col = "navy")
			xs <- seq(xlim[1], q, by=diff(xlim)/500)
			panel.xyplot(xs, dnorm(xs, mean, sd) , type='h')
			if (dnorm(q, mean, sd) > 0.5 * ymax) {
			  textloc = c(q, ymax * 0.2)
			  textloc = c(q, 1.2 * ymax)
			}
			else {
			  textloc = c(q, ymax * 0.8)
			  textloc = c(q, 1.2 * ymax)
			}
			panel.segments(q, 0, q, unit(ymax,'native') + unit(1.5,'lines'), 
			  col = "forestgreen", lwd=3)
			#panel.segments(q, textloc[2] + 0.1 * ymax, q, 
			#  ylim[2], col = "forestgreen")
			grid.text(x=q, y=unit(ymax,'native') + unit(2.4,'lines'),  default.units='native',
				paste(round(q, digits)), 
				just = c('center','bottom'), gp=gpar(cex = 1.5))
			grid.text(x=q, y=unit(ymax,'native') + unit(2.4,'lines'), default.units='native',
			  paste("(z=", round(z, 3), ")", sep = ""), 
				just = c('center','top'),  gp=gpar(cex = 1.2))
			grid.lines( gp=gpar(lwd=1.5),
				x=unit.c( unit(q,'native'), unit(q,'native') - unit(2,'char') ),
				y=unit(ymax,'native') + unit(.6,'lines'),
				arrow=arrow(angle=20,length=unit(.75,'char'))
				)
			grid.text(
				x=unit(q,'native') - unit(2,'char'), 
				y=unit(ymax,'native') + unit(.3,'lines'), default.units='native',
				paste(round(p, digits), ""), 
				just = c('right','bottom'),  gp=gpar(cex = 1.2))
			grid.lines( gp=gpar(lwd=1.5),
				x=unit.c( unit(q,'native'), unit(q,'native') + unit(2,'char') ),
				y=unit(ymax,'native') + unit(.6,'lines'),
				arrow=arrow(angle=20,length=unit(.75,'char'))
				)
			grid.text(
				x=unit(q,'native') + unit(2,'char'), 
				y=unit(ymax,'native') + unit(.3,'lines'), default.units='native',
				paste("", round(1 - p, digits)), 
				just = c('left','bottom'),  gp=gpar(cex = 1.2))
		}, ...)

	return(plot)
}


xpnorm <-
function (q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, invisible=FALSE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim, ylim, ...) 
{
    p = pnorm(q, mean = mean, sd = sd) 
    z = (q - mean)/sd
    if (verbose) {
		cat("\n")
		cat(paste("If X ~ N(",mean,",",sd,"), then \n\n",sep=""))
        cat(paste("\tP(X <= ", q, ") = P(Z <= ", round(z, 3), 
            ") = ", round(p,digits), "\n", sep = ""))
        cat(paste("\tP(X >  ", q, ") = P(Z >  ", round(z, 3), 
            ") = ", round(1 - p,digits), "\n", sep = ""))
        cat("\n")
    }
    if (plot & length(q) == 1) {
		print(.plotnorm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, ...))
    }
	if (invisible) { 
    	invisible(pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p))
	}
    return(pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p))
}


xqnorm <-
function (p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim, ylim, invisible=FALSE, ...) 
{
    q = qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail, 
        log.p = log.p)
    z = (q - mean)/sd
    if (verbose) {
        cat(paste("\tP(X <= ", q, ") = ", p, "\n", sep = ""))
        cat(paste("\tP(X >  ", q, ") = ", 1 - p, "\n", sep = ""))
        cat("\n")
    }
    if (plot & length(p) == 1) {
		print(.plotnorm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, ...))
    }
	if (invisible) { 
    	invisible(q)
	}
    return(q)
}
