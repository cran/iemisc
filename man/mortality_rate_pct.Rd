% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortality_rate.R
\encoding{UTF-8}
\name{mortality_rate_pct}
\alias{mortality_rate_pct}
\title{Mortality Rate Percent}
\usage{
mortality_rate_pct(mortality_rate, n)
}
\arguments{
\item{mortality_rate}{numeric vector that contains the mortality rate}

\item{n}{numeric vector that contains the population size units (ex., 3 for
1,000 people, 5 for 100,000 people)}
}
\value{
the mortality rate percent as a numeric vector
}
\description{
This function calculates the mortality rate percent which is also known as
the crude death rate percent for a given population.
}
\examples{

# Example from Reference 1

library(iemisc)

mr_2020 <- mortality_rate(369369, 331534662, 5)

mortality_rate_pct(mortality_rate(15, 331534662, 5), 5)

mortality_rate_pct(mr_2020, 5)

mortality_rate_pct(15, 5)



}
\references{
\enumerate{
   \item Giovanni Scerra, Published Sep 26, 2021, "The Math of the Pandemic: COVID-19 Mortality Rate", LinkedIn, \url{https://www.linkedin.com/pulse/math-pandemic-covid-19-mortality-rate-giovanni-scerra-}.
   \item Michael Darcy and Łucja Zaborowska, MD, PhD, Last updated on Nov 05, 2022, "Mortality Rate Calculator", Omni Calculator, \url{https://www.omnicalculator.com/health/mortality-rate}.
 }
}
\author{
Irucka Embry
}
