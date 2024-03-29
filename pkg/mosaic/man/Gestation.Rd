\name{Gestation}
\alias{Gestation}
\docType{data}
\title{
Data from the Child Health and Development Studies
}
\description{
Birth weight, date, and gestational period collected as part of the Child Health and Development Studies in 1961 and 1962.  Information about the baby's parents --- age, education, height, weight, and whether the mother smoked is also recorded.  
}
\usage{data(Gestation)}
\format{
  A data frame with 1236 observations on the following variables.
  \describe{
    \item{\code{id}}{identification number}
    \item{\code{pluralty}}{5 = single fetus}
    \item{\code{outcome}}{1 = live birth that survived at least 28 days}
    \item{\code{date}}{birth date where 1096=January 1, 1961}
    \item{\code{gestation}}{length of gestation (in days)}
    \item{\code{sex}}{infant's sex (1=male, 2=female)}
    \item{\code{wt}}{birth weight (in ounces)}
    \item{\code{parity}}{total number of previous pregnancies (including fetal deaths
and still births)}
    \item{\code{race}}{mother's race: 0-5=white 6=mex 7=black 8=asian 9=mixed}
    \item{\code{age}}{mother's age in years at termination of pregnancy}
    \item{\code{ed}}{mother's education:  0= less than 8th grade, 
1 = 8th -12th grade - did not graduate, 
2= HS graduate--no other schooling, 3= HS+trade,
4=HS+some college,
5=College graduate, 
6=Trade school, 7=HS unclear}
    \item{\code{ht}}{mother's height in inches to the last completed inch}
    \item{\code{wt.1}}{mother's prepregnancy weight (in pounds)}
    \item{\code{drace}}{father's race (a factor with levels equivalent to mother's race)}
    \item{\code{dage}}{father's age (in years)}
    \item{\code{ded}}{father'ed education (same coding as mother's education)}
    \item{\code{dht}}{father's height in inches to the last completed inch}
    \item{\code{dwt}}{father's weight (in pounds)}
    \item{\code{marital}}{marital status: 1=married, 2=legally separated, 3=divorced,
  4=widowed, 5=never married}
    \item{\code{inc}}{family yearly income in $2500 increments: 0=under 2500,
  1=2500-4999, ..., 8=12,500-14,999, 9=15000+}
    \item{\code{smoke}}{does mother smoke? 0=never, 1=smokes now, 
    2=until current pregnancy, 3=once did, not now}
    \item{\code{time}}{time since quitting smoking: 0=never smoked, 1=still smokes,
    2=during current preg, 3=within 1 yr, 4=1 to 2 years ago,
    5= 2 to 3 yr ago, 6= 3 to 4 yrs ago, 7=5 to 9yrs ago, 
    8=10+yrs ago, 9=quit and don't know}
    \item{\code{number}}{number of cigs smoked per day for past and current smokers  0=never, 1=1-4, 2=5-9, 3=10-14, 4=15-19, 5=20-29, 6=30-39, 7=40-60, 8=60+, 9=smoke but don't know}
  }
}
\details{
The data were presented by Nolan and Speed to address the question of whether there is a link between maternal smoking and the baby's health.
}
\source{
The admirable book by Deborah Nolan and Terry Speed
describes the data in some detail 
and provides an Internet site for accessing them: 
\url{http://www.stat.berkeley.edu/users/statlabs/}

}
\references{
D Nolan and T Speed. \emph{Stat Labs: Mathematical 
Statistics Through Applications} (2000), Springer-Verlag.
}
\examples{
data(Gestation)
}
\keyword{datasets}
