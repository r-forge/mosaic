\name{HELP}
\alias{HELP}
\alias{HELPbig}
\docType{data}
\title{HELP -- Health Evaluation and Linkage to Primary Care}
\description{ Data recorded for inpatient subjects at a detoxification unit.
}
\usage{data(HELP)

data(HELPbig)}
\details{
The HELP (Health Evaluation and Linkage to Primary Care) study was a clinical
trial for adult inpatients recruited from a detoxification unit. Patients with
no primary care physician were randomized to receive a multidisciplinary
assessment and a brief motivational intervention or usual care, with the goal
of linking them to primary medical care. The Institutional Review Board of
Boston University Medical Center approved all aspects of the study, including
the creation of the de-identified dataset available in this data frame. Variables
included in the HELP dataset are described in Table C.2 (p. 279) of
\cite{SAS and R: Data Management, Statistical Analysis, and Graphics}
by Kleinman and Horton.
}
\source{\url{http://www.math.smith.edu/sasr/datasets.php}}
\references{
Kleinman, K. and Horton, N.J. (2009)
\cite{SAS and R: Data Management, Statistical Analysis, and Graphics},
Chapman & Hall.
}
\examples{
data(HELP)
xtabs(~homeless + female, HELP)
xchisq.test(xtabs(~homeless + female, HELP))
}
\keyword{datasets}
