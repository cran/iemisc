% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimpIntPaid.R
\encoding{UTF-8}
\name{CompIntPaid}
\alias{CompIntPaid}
\title{Compound Interest Paid (Engineering Economics)}
\usage{
CompIntPaid(
  P,
  n,
  i,
  frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")
)
}
\arguments{
\item{P}{numeric vector that contains the present value(s)}

\item{n}{numeric vector that contains the period value(s)}

\item{i}{numeric vector that contains the interest rate(s) as a percent}

\item{frequency}{character vector that contains the frequency used to
obtain the number of periods [annual (1), semiannual (2), quarter (4),
bimonth (6), month (12), daily (365)]}
}
\value{
CompIntPaid numeric vector that contains the total amount paid at
   the end of n periods rounded to 2 decimal places
}
\description{
Computes the total amount paid at the end of n periods using compound
interest
}
\details{
Compound Interest is expressed as

\deqn{S_n = P\left(1 + i\right)^n}

\describe{
\item{\emph{P}}{the "principal amount (lent or borrowed)"}
\item{\emph{\eqn{S_n}}}{the "total amount paid back"}
\item{\emph{i}}{the "interest rate per interest period"}
\item{\emph{n}}{the "number of interest periods"}
}
}
\examples{

library(iemisc)

# Compound Interest example from SFPE Reference text
CompIntPaid(100, 5, 10, frequency = "annual") # the interest rate is 10\%




}
\references{
\enumerate{
 \item \emph{SFPE Handbook of Fire Protection Engineering}. 3rd Edition, DiNenno, P. J.; Drysdale, D.; Beyler, C. L.; Walton, W. D., Editor(s), page 5-94, 2002. Chapter 7; Section 5; NFPA HFPE-02. See \url{https://web.archive.org/web/20180127185316/http://fire.nist.gov/bfrlpubs/build02/PDF/b02155.pdf}.
 \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 120.
\item Chinyere Onwubiko, \emph{An Introduction to Engineering}, Mission, Kansas: Schroff Development Corporation, 1997, pages 205-206.
}
}
\author{
Irucka Embry
}
