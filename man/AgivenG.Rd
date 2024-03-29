% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PgivenG.R
\encoding{UTF-8}
\name{AgivenG}
\alias{AgivenG}
\title{Annual value given Gradient value (Engineering Economics)}
\usage{
AgivenG(
  G,
  n,
  i,
  frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")
)
}
\arguments{
\item{G}{numeric vector that contains the gradient value(s)}

\item{n}{numeric vector that contains the period value(s)}

\item{i}{numeric vector that contains the interest rate(s) as a percent}

\item{frequency}{character vector that contains the frequency used to
obtain the number of periods [annual (1), semiannual (2), quarter (4),
bimonth (6), month (12), daily (365)]}
}
\value{
AgivenG numeric vector that contains the annual value(s) rounded to
   2 decimal places
}
\description{
Compute A given G
}
\details{
\deqn{A = G\left[\frac{1}{i} - \frac{n}{\left(1 + i\right)^n - 1}\right]}

\describe{
\item{\emph{A}}{the "uniform series amount (occurs at the end of each
  interest period)"}
\item{\emph{G}}{the "uniform gradient amount"}
\item{\emph{i}}{the "effective interest rate per interest period"}
\item{\emph{n}}{the "number of interest periods"}
}
}
\examples{

library(iemisc)

# Example 4-20 from the Reference text (pages 153-154)
  AgivenG(1000, 4, 15, "annual") # the interest rate is 15\\%


# Example 4-31 from the Reference text (pages 166-167)
  AgivenG(1000, 4, 20, "semiannual") # the nominal interest rate is 20\\% compounded semiannually




}
\references{
William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 142, 150, 152-154, 164, 166-167.
}
\author{
Irucka Embry
}
