\name{pqrdata}
\alias{pdata}
\alias{qdata}
\alias{rdata}
\alias{ddata}
\title{ The Data Distribution
}
\description{
Density, distribution function, quantile function, and random generation
from data.
}
\usage{
rdata(n, vals, replace = TRUE, ...)
pdata(q, vals, ...) 
qdata((p, vals, ...)
ddata(x, vals, log=FLASE, ...)
}


\value{
ddata gives the (log) density, pexp gives the distribution function, 
qexp gives the quantile function, and rexp generates random deviates
treating \code{vals} as an enumeration of the population.
For \code{rdata}, the sampling can be either with or without replacement.
}

\example{
data(iris)
rdata(10,iris$Species)
ddata('setosa',iris$Species)
pdata(3:6, iris$Sepal.Length)
qdata(.5, iris$Sepal.Length)
}

\keyword{ distribution }
