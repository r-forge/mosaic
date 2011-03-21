xqnorm <-
function (p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 5, 
    lower.tail = TRUE, log.p = FALSE, ...) 
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
        zmax = max(4, abs(z) * 1.6)
        xlim = mean + c(-1, 1) * zmax * sd
        ymax = dnorm(mean, mean = mean, sd = sd)
        ylim = c(0, 1.3 * ymax)
        xdata = rep(xlim, each = 2)
        ydata = rep(ylim, times = 2)
        plot <- xyplot(ydata ~ xdata, xlim = xlim, ylim = ylim, 
            xlab = "", ylab = "density", panel = function(x, 
                y, ...) {
                panel.mathdensity(dmath = dnorm, args = list(mean = mean, 
                  sd = sd), lwd = 2, n = 100, col = "blue")
                if (dnorm(q, mean, sd) > 0.5 * ymax) {
                  textloc = c(q, ymax * 0.2)
                }
                else {
                  textloc = c(q, ymax * 0.8)
                }
                panel.segments(q, 0, q, textloc[2] - 0.1 * ymax, 
                  col = "lightskyblue3")
                panel.segments(q, textloc[2] + 0.1 * ymax, q, 
                  ylim[2], col = "lightskyblue3")
                panel.text(textloc[1], textloc[2], paste(round(q, 
                  digits)), adj = c(0.5, 0), cex = 1.5)
                panel.text(textloc[1], textloc[2] - 0.02 * ymax, 
                  paste("(z=", round(z, 3), ")", sep = ""), adj = c(0.5, 
                    1), cex = 1.2)
                panel.text(q, ymax * 1.1, paste(round(p, digits), 
                  "<-- "), adj = 1, cex = 1.2)
                panel.text(q, ymax * 1.1, paste(" -->", round(1 - 
                  p, digits)), adj = 0, cex = 1.2)
            }, ...)
        print(plot)
    }
    return(q)
}
