\name{mFit}
\alias{mFit}
\title{Interactive applet for term selection and nonlinear parameters for fitting.}
\description{
Displays data and a menu of modeling functions.  
Finds a linear combination of the selected modeling functions, with the user setting nonlinear parameters manually.
}
\usage{
mFit(expr,data)
}
\arguments{
  \item{expr}{A formula object giving the dependent and independent variables to use.}
  \item{data}{A data frame containing the values for the variables identified in \code{expr}.}
}
\details{
There are sliders only for nonlinear parameters.  Linear parameters are found automatically.  Only a function of one variable is supported at present.
}
\value{
A function that implements the current state of the parameters and terms selected.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
\dontrun{
data(Kidsfeet)
mFit(width ~ length, data=Kidsfeet)
}
}
\keyword{calculus}
