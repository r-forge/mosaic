#setCacheDir("cache")
require(grDevices)
require(datasets)
require(stats)
require(lattice)
require(grid)
require(fastR)
require(abd)
require(mosaic)
require(vcd)
trellis.par.set(theme=col.mosaic())
trellis.par.set(fontsize=list(text=9))
options(keep.blank.line=FALSE)
options(width=85)
options(digits=3)
options(continue=' ')
xyplot <- function(...) { print(lattice::xyplot(...)) }
bwplot <- function(...) { print(lattice::bwplot(...)) }
histogram <- function(...) { print(lattice::histogram(...)) }
xhistogram <- function(...) { print(mosaic::xhistogram(...)) }
qqmath <- function(...) { print(lattice::qqmath(...)) }
xqqmath <- function(...) { print(mosaic::xqqmath(...)) }
dotPlot <- function(...) { print(mosaic::dotPlot(...)) }
barchart <- function(...) { print(lattice::barchart(...)) }
densityplot <- function(...) { print(lattice::densityplot(...)) }
splom <- function(...) { print(lattice::splom(...)) }
print.summary.lm <- function(
	object, 
	digits=max(3, getOption("digits")-3), 
    symbolic.cor=x$symbolic.cor, 
    signif.stars=getOption("show.signif.stars"), 
    ...) {
  output <- capture.output( stats:::print.summary.lm(object, 
        digits=digits, 
		symbolic.cor=symbolic.cor, 
		signif.stars=signif.stars, ...))
    prselect(output, stop="Coefficients:") 
}

print.lm <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{
  output <- capture.output( stats:::print.lm(x, 
        digits=digits, 
		...))
    prselect(output, stop="Coefficients:") 
}
favstats <- mosaic::favstats
