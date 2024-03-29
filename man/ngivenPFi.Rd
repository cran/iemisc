% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngivenPFi.R
\encoding{UTF-8}
\name{ngivenPFi}
\alias{ngivenPFi}
\title{To Find i Given F, n, and P (Engineering Economics)}
\usage{
ngivenPFi(P, F, i)
}
\arguments{
\item{P}{numeric vector that contains the present value(s)}

\item{F}{numeric vector that contains the future value(s)}

\item{i}{numeric vector that contains the interest rate(s) as a percent}
}
\value{
n numeric vector that contains the period value(s)
}
\description{
Compute n given P, F, and i
}
\details{
n is expressed as

	\deqn{n = \frac{\log \left(\frac{F}{P}\right)}{\log \left(1 + i\right)}}

\describe{
\item{\emph{n}}{the "number of interest periods"}
\item{\emph{F}}{the "future equivalent"}
\item{\emph{P}}{the "present equivalent"}
\item{\emph{i}}{the "effective interest rate per interest period"}
}
}
\examples{

# Example for equation 4-7 from the Reference text (page 142)

library(iemisc)

ngivenPFi(P = 500, F = 1000, i = 15)




}
\references{
William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 129, 142.
}
\author{
Irucka Embry
}
