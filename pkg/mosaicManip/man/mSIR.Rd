\name{mSIR}
\alias{mSIR}
\title{Interactive applet for exploring the SIR epidemic disease model}
\description{
Displays a phase portrait of the number of susceptible, infected, and recovered populations against \code{t}
  }
\usage{
mSIR()
}
\arguments{

}
\details{
Using the sliders, a user may control the probability of infection, the number of contacts per day, duration of disease decay, the number of the initial population that is infected, and finally the number of births per day. With these parameters, the model can be substantially altered. For example, if the probability of infection is high and contact is high, the disease sweeps through the population quickly. If probability of infection and contact are low, the disease may not infect every member of the population. If there are many babies added per day and a long disease decay, sometimes a recurring epidemic occurs. The model starts with a population of 1000 initially and runs for 100 days. 
}
\value{
A function that implements and displays the current state of each group in the SIR model at many points in time.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
\dontrun{
mSIR()
}
}
\keyword{differential equations}
\keyword{epidemiology}
\keyword{SIR}