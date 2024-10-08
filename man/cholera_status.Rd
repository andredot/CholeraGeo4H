% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{cholera_status}
\alias{cholera_status}
\title{Assign current status}
\usage{
cholera_status(db)
}
\arguments{
\item{db}{: Yemen Cholera Cases (ycc_lag) database, variables
should be defined}
}
\value{
a new db with an additional status column that represent
the region configuration, either the outbreak is controlled
(level 0 - preparedness), cases are on the surge but with CFR
under the 1\% threshold (level 1 - early action), or CFR is over
1\%, signalling an unacceptable stress level (level 2 - response).
Level 2 is declared also if the attack rate in the previous 4
weeks has been higher that 2 per 1000 people.
}
\description{
Assign current status
}
\examples{
\dontrun{
  cholera_status(ycc_lag)
}
}
