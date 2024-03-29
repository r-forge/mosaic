\name{mosaic-package}
\alias{mosaic-package}
\alias{mosaic}
\docType{package}
\title{Project MOSAIC (mosaic-web.org) math and stats teaching utilities}
\description{
Data sets and utilities from Project MOSAIC (mosaic-web.org) used to teach mathematics, 
statistics, computation and modeling.  Funded by the NSF, Project MOSAIC is creating a 
community of educators working to tie together aspects of quantitative work that students 
in science, technology, engineering and mathematics will need in their professional lives, 
but which are usually taught in isolation, if at all.
}


\details{
  \tabular{ll}{
	Package: \tab mosaic\cr
	Type: \tab Package\cr
	Version: \tab 0.2-6\cr
	Date: \tab 2012-01-04\cr
	License: \tab GPL\cr
	LazyLoad: \tab yes\cr
	LazyData: \tab yes\cr
}

\tabular{ll}{
Overview\tab \code{file.show(system.file("README", package = "mosaic"))}\cr
News\tab \code{RShowDoc("NEWS", package = "mosaic")}\cr
Acknowledgements\tab \code{RShowDoc("THANKS", package = "mosaic")}\cr
This File\tab \code{?mosaic}\cr
}

}

\author{
Randall Pruim (\url{rpruim@calvin.edu})
}

\keyword{ package }

%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}

\examples{
data(CPS)
str(CPS)
}
