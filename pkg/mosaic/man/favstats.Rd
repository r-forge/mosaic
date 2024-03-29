\name{favstats}
\alias{favstats}
\title{Some favorite statistical summaries }
\description{
Computes mean, standard deviation, and quartiles of a data vector.
}
\usage{
favstats(x, na.rm = TRUE)
}
\arguments{
  \item{x}{ numeric vector }
  \item{na.rm}{ boolean indicating whether missing data should be ignored }
}
\value{A vector of statistical summaries}

\examples{
favstats(1:10)
favstats(faithful$eruptions)
}
% add key words?
