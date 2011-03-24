\name{xtras}
\alias{xchisq.test}
\alias{xhistogram}
\alias{xqqmath}
\alias{panel.xqqmath}
\alias{panel.xhistogram}
\title{
Augmented functions
}
\description{
These functions all behave similarly to the functions with the initial \code{x}
removed from their names.
}
\usage{
xchisq.test(...)

xhistogram(x, data, ...)

xpnorm(q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 4, 
	lower.tail = TRUE, log.p = FALSE, xlim, ylim, ...)

xqnorm(p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 5, 
	lower.tail = TRUE, log.p = FALSE, ...)

xqqmath(x, data=NULL, panel="panel.xqqmath", ...)

panel.xqqmath(x, qqmathline = !(fitline || idline), idline = FALSE, fitline = FALSE, 
    slope = NULL, intercept = NULL, overlines = FALSE, groups=NULL, ...,
    col.line = trellis.par.get("add.line")$col, pch = 16, lwd = 2, 
    lty = 2 ) 

panel.xhistogram(x, 
	dcol = trellis.par.get("plot.line")$col, dlwd = 2, 
    gcol = trellis.par.get("add.line")$col, glwd = 2, 
	fcol = trellis.par.get("superpose.polygon")$col,
	dmath = dnorm, 
	verbose = FALSE,
    dn = 100, args = NULL, labels = FALSE, density = FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, groups=NULL, breaks, 
	stripes=c('vertical','horizontal','none'), alpha=1, ...) 
}
\arguments{
  \item{\dots}{ arguments passed to other functions. }

  \item{data}{a data frame.}

  \item{density }{boolean -- should a density curve be overlaid on histogram? }

  \item{x}{ A numeric vector or formula. }

  \item{overlines}{boolean. If TRUE, place lines above the data, otherwise
  lines are below (default).
  }

  \item{idline}{boolean. If TRUE, add the line with slope 1 and intercept 0
  to the plot.
  }

  \item{qqmathline}{boolean.  
  	If TRUE, add a line using \code{panel.qqmathline}
	}

  \item{fitline}{boolean.  If TRUE, add a line to the plot, using
  slope and intercept given by \code{slope} and \code{intercept}.
  }


  \item{slope}{ slope for \code{fitline}.  If NULL (the default)
  the mean of the data is used, which works well
  for normal-quantile plots but may not be what you want for fitting
  other distributions. }

  \item{intercept}{ intercept for \code{fitline}.  
  If NULL (the default), 
	the standard deviation of the data is used, which works well
	for normal-quantile plots but may not be what you want for fitting
	other distributions. }
\item{col.line}{like \code{col} but applied only to lines}
\item{xlim, ylim}{display limits}
\item{
q,
p,
groups,
type,
labels,
which,
main,
pch,
lwd,
lty}{as in \code{\link{lattice}} graphics functions}
\item{
caption,
fit,
start,
panel.default,
sub.caption,
print.plots,
ask,
addline.col,
line.col,
symbol.col,
id.n,
labels.id,
cex.id,
cook.levels,
add.smooth,
label.pos,
cex.caption,
mean,
sd,
plot,
verbose,
digits,
lower.tail,
log.p,
panel
}{ to be documented}
}

\author{
Randall Pruim
}

\seealso{
\code{\link{histogram}}, 
\code{\link{chisq.test}}, 
\code{\link{pnorm}}, 
\code{\link{qnorm}}, 
\code{\link{qqmath}}, and
\code{\link{plot}}. 
}
\examples{
# Physicians' Health Study data
phs <- cbind(c(104,189),c(10933,10845)) 
rownames(phs) <- c("aspirin","placebo") 
colnames(phs) <- c("heart attack","no heart attack") 
phs 
xchisq.test(phs) 
if (require(manipulate)) {
  manipulate( xpnorm(score, 500, 100, verbose=verbose),
    score = slider(200,800),
	verbose = checkbox(TRUE, label="Verbose Output")
  )
}
}
