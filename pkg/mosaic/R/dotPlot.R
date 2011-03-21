dotPlot <-
function (x, ..., panel = panel.dotPlot) 
{
    histogram(x, type = "count", panel = panel, ...)
}

panel.dotPlot <-
function (x, breaks, equal.widths = TRUE, groups = NULL, nint = round(log2(length(x)) + 
    1), pch = if (is.null(groups)) trellis.par.get("dot.symbol")$pch else trellis.par.get("superpose.symbol")$pch, 
    col = if (is.null(groups)) trellis.par.get("dot.symbol")$col else trellis.par.get("superpose.symbol")$col, 
    lty = trellis.par.get("dot.line")$lty, lwd = trellis.par.get("dot.line")$lwd, 
    col.line = trellis.par.get("dot.line")$col, alpha = trellis.par.get("dot.symbol")$alpha, 
    type = "count", ...) 
{
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")
    x <- as.numeric(x)
    if (length(x) > 0) {
        if (is.null(breaks)) {
            breaks <- if (equal.widths) 
                do.breaks(range(x, finite = TRUE), nint)
            else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- hist(x, breaks = breaks, plot = FALSE, ...)
        y <- h$counts
        nb <- length(breaks)
        if (length(y) != nb - 1) 
            warning("problem with hist computations")
        if (nb > 1) {
            for (bin in 1:(nb - 1)) {
                if (y[bin] <= 0) {
                  next
                }
                xvals <- rep((breaks[bin] + breaks[bin + 1])/2, 
                  y[bin])
                yvals <- 1:y[bin]
                grid.points(pch = if (length(pch) > 0) 
                  pch[1]
                else pch, gp = gpar(fill = col, alpha = alpha, 
                  col = col, lty = lty, lwd = lwd), x = xvals, 
                  y = yvals, default.units = "native")
            }
        }
    }
}
