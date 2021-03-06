% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tpCFret.R
\name{tpCFret}
\alias{tpCFret}
\title{Third Pillar Cash Flows in retirement}
\usage{
tpCFret(
  ret_age = 65,
  c_age,
  w3,
  alpha,
  wealth_at_ret_age,
  retr,
  psi = 0.015,
  warnings = TRUE
)
}
\arguments{
\item{ret_age}{optional, retirement age, can be set anywhere between 60 and 70 (default: 65)}

\item{c_age}{the investor's current age (assuming birthday is calculation-day)}

\item{w3}{third pillar portfolio allocation (given either as vector or as matrix with entries) for all years}

\item{alpha}{parameter to choose fraction of wealth NOT consumed during retirement but kept for investment (and subsequent consumption) - see file 'Consumption_3p.ods'}

\item{wealth_at_ret_age}{lumpsum from second pillar (after tax using `taxCFlumpsum`) plus third pillar (wealth at retirement from `tpCFwork()`
after taking away fraction of third pillar savings that is converted to a life-long pension `nu2`)}

\item{retr}{GivenVar: investment return scenarios (real)}

\item{psi}{optional, spread to take a loan/leverage for third pillar savings}

\item{warnings}{optional: should warnings be given? (default=TRUE)}
}
\value{
list with two elemnts:
-  consumption during pension years (matrix with dim=c(122-ret_age,# of Scenarios))
-  Development of wealth during pension years (matrix with dim=c(122-ret_age,# of Scenarios))
}
\description{
Calculate cash flows from third pillar in retirement
}
\examples{
data(retr)
tpret_ex <- tpCFret(ret_age=65,c_age=42,
w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
           alpha=0.96,wealth_at_ret_age=100000,
           retr=retr[,,1:10],psi=0.015)
tpret_ex <- tpCFret(ret_age=65,c_age=42,
w3=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
           alpha=0.96,wealth_at_ret_age=c(rep(-10000,5),
           rep(10000,5)),retr=retr[,,1:10],psi=0)

}
