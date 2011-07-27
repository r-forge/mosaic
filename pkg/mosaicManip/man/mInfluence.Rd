\name{mInfluence}
\alias{mInfluence}
\title{Interactive applet to explore influential points in linear models}
\description{
Displays data and a menu of modeling functions.  
Finds a linear combination of the selected modeling functions, with the user setting nonlinear parameters manually.
}
\usage{
mInfluence(expr, data)
}
\arguments{
  \item{expr}{A formula object giving the dependent and independent variables to use.}
  \item{data}{A data frame containing the values for the variables identified in \code{expr}.}
}
\details{
The linear model described by the \code{expr} term is found and fitted values are plotted against the data. Using the Dep. variable to plot picker the user may choose which variable to plot on the x axis. The user can add a new point to the data set by checking the Add new point checkbox. Using the x and y sliders below it, the user can change the position of the new point. The sliders' units are standard deviations from the mean. The type of influence picker is currently nonfunctional.
}
\value{
A function that allows the user to explore influential points in a linear model. 
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
\dontrun{
data(trees); fetchData(CPS)
mInfluence(Girth~Height, trees)
mInfluence(wage~educ+exper+race+sex+age+sector, data=CPS)
}
}
\keyword{statistics}
