% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PgivenA.R
\encoding{UTF-8}
\name{PgivenA}
\alias{PgivenA}
\alias{PA}
\title{Present value given Annual value (Engineering Economics)}
\usage{
PgivenA(
  A,
  n,
  i,
  frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")
)

PA(
  A,
  n,
  i,
  frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")
)
}
\arguments{
\item{A}{numeric vector that contains the annual value(s)}

\item{n}{numeric vector that contains the period value(s)}

\item{i}{numeric vector that contains the interest rate(s) as a percent}

\item{frequency}{character vector that contains the frequency used to
obtain the number of periods [annual (1), semiannual (2), quarter (4),
bimonth (6), month (12), daily (365)]}
}
\value{
PgivenA numeric vector that contains the present value(s) rounded
   to 2 decimal places

PA data.frame of both n (0 to n) and the resulting present values
   rounded to 2 decimal places
}
\description{
Compute P given A
}
\details{
P is expressed as

	\deqn{P = A\left[\frac{\left(1 + i\right)^n - 1}{i\left(1 + i\right)^n}\right]}

\describe{
\item{\emph{P}}{the "present equivalent"}
\item{\emph{A}}{the "uniform series amount (occurs at the end of each
    interest period)"}
\item{\emph{i}}{the "effective interest rate per interest period"}
\item{\emph{n}}{the "number of interest periods"}
}
}
\examples{
library("iemisc")
# Example 4-9 from the Reference text (page 133-134)
PgivenA(20000, 5, 15, "annual") # the interest rate is 15\%

PA(20000, 5, 15, "annual") # the interest rate is 15\%


}
\references{
William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 133-134, 142, 164.
}
