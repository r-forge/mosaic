xhistogram <-
function (x, data, ..., type = "density", labels = F, density = FALSE, 
	panel=panel.xhistogram,
    fit = NULL, start = NULL, groups=NULL) 
{
    histogram(x, data, groups, ..., panel = panel, type = type, fit = fit, 
        start = start, labels = labels, density = density)
}


panel.xhistogram <-
function (x, 
	dcol = trellis.par.get("plot.line")$col, dlwd = 2, 
    gcol = trellis.par.get("add.line")$col, glwd = 2, 
	fcol = trellis.par.get("superpose.polygon")$col,
	dmath = dnorm, 
    dn = 100, args = NULL, labels = FALSE, density = FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, groups=NULL, breaks, 
	stripes=c('vertical','horizontal','none'), alpha=1, ...) 
{
	stripes <- match.arg(stripes)
	if (!is.null(groups)) {
    	hist.master <- hist(x, plot = FALSE, breaks=breaks, ...)
		nbreaks <- length(hist.master$breaks)
		groups <- factor(groups)
		ngroups <- length(levels(groups))
		props <- (table(groups))/length(groups)
		fcol <- rep(fcol, length=length(props))
		cumdensity= rep(0, length(hist.master$mids))
		cumrdensity= rep(0, length(hist.master$mids))
		for (level in 1:ngroups) {
			hist.level <- hist(
				x[groups==levels(groups)[level] ], 
				plot=FALSE,
				breaks=hist.master$breaks,
				...
			)
			hist.level$density <- hist.level$density * props[level]
			hist.level$rdensity <- hist.level$density / hist.master$density 
			switch( stripes, 
				vertical = 
				grid.rect(
					x=hist.level$breaks[-nbreaks] + cumrdensity*diff(breaks),
					y=0,
					width=diff(hist.level$breaks) * hist.level$rdensity,
					height=hist.master$density,
					just=c('left','bottom'),
					default.units='native',
					gp=gpar(col=fcol[level], fill=fcol[level],alpha=alpha),
					),
				horizontal = 
				grid.rect(
					x=hist.level$breaks[-nbreaks],
					y=0 + cumrdensity* hist.master$density,
					width=diff(hist.level$breaks),
					height=hist.master$density * hist.level$rdensity,
					just=c('left','bottom'),
					default.units='native',
					gp=gpar(col=fcol[level], fill=fcol[level],alpha=alpha),
					),
				none=
				grid.rect(
					x=hist.level$breaks[-nbreaks],
					y=0,
					width=diff(hist.level$breaks),
					height=hist.level$density,
					just=c('left','bottom'),
					default.units='native',
					gp=gpar(col='black', fill=fcol[level],alpha=alpha),
					)
			)
			cumdensity <- cumdensity + hist.level$density
			cumrdensity <- cumrdensity + hist.level$rdensity
			if (stripes != 'none') {
			grid.rect(
				x=hist.master$breaks[-nbreaks],
				y=0,
				width=diff(hist.master$breaks),
				height=hist.master$density,
				just=c('left','bottom'),
				default.units='native',
				gp=gpar(col='black', fill='transparent'),
				)
			}
		}
		print(hist.master)
	} else {
    	panel.histogram(x, type = type, breaks=breaks, ...)
	}
    if (labels) {
        h <- hist(x, plot = FALSE, ...)
        if (type == "count") {
            aa <- max(h$counts) * 0.01
            grid.text(label = as.character(round(h$counts, 3)), 
                x = h$mids, y = aa + h$counts, just = c("centre", 
                  "bottom"), default.units = "native")
        }
        else if (type == "percent") {
            ss <- sum(h$counts)
            aa <- max(0.01 * h$counts/ss)
            cat(h$counts)
            cat("\n")
            cat(h$counts/ss)
            cat("\n")
            cat(ss)
            cat("\n")
            print(h)
            cat("\n")
            grid.text(label = as.character(round(h$counts/ss, 
                3)), x = h$mids, y = aa + (h$counts/ss), just = c("centre", 
                "bottom"), default.units = "native")
        }
        else {
            aa <- max(h$density) * 0.01
            grid.text(label = as.character(round(h$density, 3)), 
                x = h$mids, y = aa + h$density, just = c("centre", 
                  "bottom"), default.units = "native")
        }
    }
    if (!is.null(fit)) {
        x = x[!is.na(x)]
        density = T
        if (is.null(args)) {
			if (! require(MASS) ){
				stop("The MASS package must be loaded to auto-fit distributions.")
			}
            if (is.null(start)) {
                args = fitdistr(x, fit)$estimate
            }
            else {
                args = fitdistr(x, fit, start = start)$estimate
            }
        }
        if (tolower(fit) == "exponential") {
            dmath = dexp
        }
        if (tolower(fit) == "normal") {
            dmath = dnorm
        }
        if (tolower(fit) == "lognormal" | tolower(fit) == "log-normal") {
            dmath = dlnorm
        }
        if (tolower(fit) == "poisson") {
            dmath = dpois
        }
        if (tolower(fit) == "gamma") {
            dmath = dgamma
        }
        if (tolower(fit) == "chisq") {
            dmath = dchisq
        }
        if (tolower(fit) == "chi-squared") {
            dmath = dchisq
        }
    }
    if (is.null(args)) {
        args = list(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))
    }
    if (density) {
        cat("args for density function:\n")
        print(args)
        panel.mathdensity(dmath = dmath, args = args, n = dn, 
            col = dcol, lwd = dlwd)
    }
    if (!missing(v)) {
        for (x in v) {
            panel.abline(v = x, col = gcol, lwd = glwd)
        }
    }
    if (!missing(h)) {
        for (y in h) {
            panel.abline(h = y, col = gcol, lwd = glwd)
        }
    }
}