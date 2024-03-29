\name{Marriage}
\alias{Marriage}
\docType{data}
\title{
Marriage records
}
\description{
Marriage records from the Mobile County, Alabama, probate court.  
}
\usage{data(Marriage)}
\format{
  A data frame with 98 observations on the following variables.
  \describe{
    \item{\code{bookpageID}}{a factor with levels for each book and page (unique identifier)}
    \item{\code{appdate}}{a factor with levels corresponding to each of the dates on which the
application was filed (in the form MO/DY/YY, e.g. 1/22/99 represents January 22, 1999)}
    \item{\code{ceremonydate}}{a factor with levels corresponding to the date of the ceremony}
    \item{\code{delay}}{number of days between the application and the ceremony}
    \item{\code{officialTitle}}{a factor with levels \code{BISHOP} \code{CATHOLIC PRIEST} \code{CHIEF CLERK} \code{CIRCUIT JUDGE } \code{ELDER} \code{MARRIAGE OFFICIAL} \code{MINISTER} \code{PASTOR} \code{REVEREND}}
    \item{\code{person}}{a factor with levels \code{Bride} \code{Groom}}
    \item{\code{dob}}{a factor with levels corresponding to the date of birth of the person}
    \item{\code{age}}{age of the person (in years)}
    \item{\code{race}}{a factor with levels \code{American Indian} \code{Black} \code{Hispanic} \code{White}}
    \item{\code{prevcount}}{the number of previous marriages of the person, as listed on the
application}
    \item{\code{prevconc}}{the way the last marriage ended, as listed on the application}
    \item{\code{hs}}{the number of years of high school education, as listed on the application}
    \item{\code{college}}{the number of years College education, as listed on the application.  Where no number was listed, this field was left blank, unless less than 12 years High School was reported, in which case it was entered as 0.}
    \item{\code{dayOfBirth}}{the day of birth, as a number from 1 to 365 counting from January 1}
    \item{\code{sign}}{the astrological sign, with levels \code{Aquarius} \code{Aries} \code{Cancer} \code{Capricorn} \code{Gemini} \code{Leo} \code{Libra} \code{Pisces} \code{Saggitarius} \code{Scorpio} \code{Taurus} \code{Virgo}}
  }
}
\details{
The calculation of the astrological sign may not correctly sort people directly on the borders between signs.  This variable is not part of the original record.
}
\source{
The records were collected through 
\url{http://www.mobilecounty.org/probatecourt/recordssearch.htm}
}
\examples{
data(Marriage)
}
\keyword{datasets}
