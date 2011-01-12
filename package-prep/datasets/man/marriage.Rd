\name{marriage}
\alias{marriage}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(marriage)}
\format{
  A data frame with 98 observations on the following 20 variables.
  \describe{
    \item{\code{BookpageID}}{a factor with levels \code{B230p1209} \code{B230p1354} \code{B230p1665} \code{B230p1948} \code{B230p539} \code{B230p677} \code{B230p766} \code{B230p892} \code{B230p994} \code{B231p1042} \code{B231p1201} \code{B231p1436} \code{B231p1841} \code{B231p198} \code{B231p406} \code{B231p48} \code{B231p632} \code{B231p713} \code{B232p1079} \code{B232p1211} \code{B232p148} \code{B232p1519} \code{B232p1888} \code{B232p299} \code{B232p522} \code{B232p770} \code{B233p1245} \code{B233p1381} \code{B233p141} \code{B233p1690} \code{B233p1899} \code{B233p268} \code{B233p429} \code{B233p674} \code{B233p903} \code{B234p1292} \code{B234p1485} \code{B234p1621} \code{B234p1966} \code{B234p438} \code{B234p65} \code{B234p687} \code{B234p904} \code{B235p259} \code{B235p404} \code{B235p563} \code{B235p7} \code{B235p837} \code{B235p992}}
    \item{\code{Appdate}}{a factor with levels \code{1/22/99} \code{1/30/98} \code{1/8/97} \code{10/14/98} \code{10/2/98} \code{10/20/97} \code{10/29/96} \code{10/31/97} \code{11/12/96} \code{11/12/98} \code{11/19/96} \code{11/26/97} \code{11/9/98} \code{12/14/98} \code{12/2/96} \code{12/2/98} \code{12/23/98} \code{12/26/96} \code{12/31/97} \code{12/9/96} \code{2/10/97} \code{2/12/98} \code{2/27/98} \code{2/5/99} \code{3/12/97} \code{3/13/98} \code{3/21/97} \code{3/3/97} \code{3/31/98} \code{4/22/97} \code{4/23/98} \code{4/30/97} \code{4/7/97} \code{5/1/98} \code{5/23/97} \code{5/26/98} \code{6/18/97} \code{6/18/98} \code{6/3/97} \code{6/9/98} \code{7/13/98} \code{7/14/97} \code{7/31/98} \code{8/13/98} \code{8/18/97} \code{8/7/97} \code{9/15/98} \code{9/24/97} \code{9/4/97}}
    \item{\code{Ceremonydate}}{a factor with levels \code{1/24/97} \code{1/30/98} \code{1/31/99} \code{10/2/98} \code{10/20/97} \code{10/23/98} \code{10/31/97} \code{11/12/96} \code{11/26/97} \code{11/27/96} \code{11/28/98} \code{11/9/96} \code{11/9/98} \code{12/14/96} \code{12/14/98} \code{12/23/98} \code{12/26/96} \code{12/31/97} \code{12/5/98} \code{12/7/96} \code{2/10/97} \code{2/14/98} \code{2/6/99} \code{3/22/97} \code{3/29/97} \code{3/31/97} \code{3/31/98} \code{3/7/98} \code{4/24/98} \code{4/26/97} \code{4/4/98} \code{4/7/97} \code{5/23/97} \code{5/23/98} \code{5/30/98} \code{5/4/97} \code{6/20/98} \code{6/27/97} \code{6/27/98} \code{6/7/97} \code{7/14/97} \code{7/31/98} \code{8/1/98} \code{8/10/97} \code{8/13/98} \code{8/30/97} \code{9/26/97} \code{9/26/98} \code{9/6/97}}
    \item{\code{Delay}}{a numeric vector}
    \item{\code{Officaltitle}}{a factor with levels \code{BISHOP} \code{CATHOLIC PRIEST} \code{CHIEF CLERK} \code{CIRCUIT JUDGE } \code{ELDER} \code{MARRIAGE OFFICIAL} \code{MINISTER} \code{PASTOR} \code{REVEREND}}
    \item{\code{Person}}{a factor with levels \code{Bride} \code{Groom}}
    \item{\code{Dob}}{a factor with levels \code{1/21/76} \code{1/30/66} \code{1/31/62} \code{1/6/60} \code{10/1/52} \code{10/10/79} \code{10/11/71} \code{10/15/78} \code{10/20/58} \code{10/21/78} \code{10/29/25} \code{10/29/69} \code{10/30/54} \code{11/10/79} \code{11/15/67} \code{11/16/47} \code{11/19/71} \code{11/26/56} \code{11/27/72} \code{11/28/59} \code{11/29/52} \code{12/14/66} \code{12/20/59} \code{12/23/72} \code{12/3/55} \code{12/4/75} \code{12/8/71} \code{2/1/77} \code{2/13/75} \code{2/13/76} \code{2/18/55} \code{2/20/43} \code{2/20/62} \code{2/21/70} \code{2/21/77} \code{2/25/68} \code{2/25/78} \code{2/26/43} \code{2/28/44} \code{2/6/55} \code{3/10/70} \code{3/18/27} \code{3/18/79} \code{3/2/58} \code{3/20/77} \code{3/29/59} \code{3/3/71} \code{3/7/80} \code{3/9/77} \code{4/10/45} \code{4/11/64} \code{4/13/63} \code{4/14/76} \code{4/17/78} \code{4/18/75} \code{4/23/80} \code{4/24/44} \code{4/26/73} \code{4/6/57} \code{4/8/55} \code{5/18/57} \code{5/19/48} \code{5/19/78} \code{5/20/56} \code{5/20/74} \code{5/21/24} \code{5/21/70} \code{5/26/28} \code{5/28/41} \code{5/28/80} \code{6/23/53} \code{6/24/61} \code{6/25/59} \code{6/29/80} \code{6/5/64} \code{6/8/67} \code{7/17/55} \code{7/19/31} \code{7/2/51} \code{7/20/82} \code{7/22/53} \code{8/21/58} \code{8/3/30} \code{8/3/80} \code{8/31/72} \code{8/6/64} \code{8/6/71} \code{8/6/78} \code{9/1/76} \code{9/10/54} \code{9/12/77} \code{9/14/78} \code{9/17/48} \code{9/19/46} \code{9/20/60} \code{9/20/69} \code{9/27/62} \code{9/5/76}}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Race}}{a factor with levels \code{American Indian} \code{Black} \code{Hispanic} \code{White}}
    \item{\code{Prevcount}}{a numeric vector}
    \item{\code{Prevconc}}{a factor with levels \code{Death} \code{Divorce}}
    \item{\code{Hs}}{a numeric vector}
    \item{\code{College}}{a numeric vector}
    \item{\code{Dayofbirth}}{a numeric vector}
    \item{\code{Sign}}{a factor with levels \code{Aquarius} \code{Aries} \code{Cancer} \code{Capricorn} \code{Gemini} \code{Leo} \code{Libra} \code{Pisces} \code{Saggitarius} \code{Scorpio} \code{Taurus} \code{Virgo}}
    \item{\code{X}}{a logical vector}
    \item{\code{X.1}}{a logical vector}
    \item{\code{X.2}}{a logical vector}
    \item{\code{X.3}}{a logical vector}
    \item{\code{X01.01.1900}}{a logical vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(marriage)
## maybe str(marriage) ; plot(marriage) ...
}
\keyword{datasets}
