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
trellis.par.set(theme=col.fastR(bw=FALSE))
trellis.par.set(fontsize=list(text=9))
options(keep.blank.line=FALSE)
options(width=90)
xyplot <- function(...) { print(lattice::xyplot(...)) }
bwplot <- function(...) { print(lattice::bwplot(...)) }
histogram <- function(...) { print(lattice::histogram(...)) }
xhistogram <- function(...) { print(mosaic::xhistogram(...)) }
dotPlot <- function(...) { print(mosaic::dotPlot(...)) }
barchart <- function(...) { print(lattice::barchart(...)) }
mosaic <- function(...) { print(vcd::mosaic(...)) }
densityplot <- function(...) { print(lattice::densityplot(...)) }