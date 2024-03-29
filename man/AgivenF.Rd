% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AgivenF.R
\encoding{UTF-8}
\name{AgivenF}
\alias{AgivenF}
\alias{AF}
\title{Annual value given Future value (Engineering Economics)}
\usage{
AgivenF(
  F,
  n,
  i,
  frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")
)

AF(
  F,
  n,
  i,
  frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")
)
}
\arguments{
\item{F}{numeric vector that contains the future value(s)}

\item{n}{numeric vector that contains the period value(s)}

\item{i}{numeric vector that contains the interest rate(s) as a percent}

\item{frequency}{character vector that contains the frequency used to
obtain the number of periods [annual (1), semiannual (2), quarter (4),
bimonth (6), month (12), daily (365)]}
}
\value{
AgivenF numeric vector that contains the annual value(s) rounded to
   2 decimal places

AF data.frame of both n (0 to n) and the resulting annual values
   rounded to 2 decimal places
}
\description{
Compute A given F
}
\details{
A is expressed as

	\deqn{A = F\left[\frac{i}{\left(1 + i\right)^n - 1}\right]}

\describe{
\item{\emph{A}}{the "uniform series amount (occurs at the end of each
    interest period)"}
\item{\emph{F}}{the "future equivalent"}
\item{\emph{i}}{the "effective interest rate per interest period"}
\item{\emph{n}}{the "number of interest periods"}
}
}
\examples{

library(iemisc)

# Example for equation 4-12 from the Reference text (page 135-136)
AgivenF(309*10^6, 60, 0.5, "month")

# the interest rate is 0.5\\% per month and n is 60 months
# "$4.4187 million per month" is the answer

AF(309*10^6, 60, 0.5, "annual")
# the interest rate is 0.5\\% per month and n is 60 months


}
\references{
William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 135-136, 142, 164.
}
\author{
Irucka Embry
}
