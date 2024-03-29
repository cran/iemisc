% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortality_rate.R
\encoding{UTF-8}
\name{prop_mortality_ratio}
\alias{prop_mortality_ratio}
\title{Proportional Mortality Ratio}
\usage{
prop_mortality_ratio(cause_deaths, total_deaths)
}
\arguments{
\item{cause_deaths}{numeric vector that contains the deaths from a single
cause}

\item{total_deaths}{numeric vector that contains the total deaths in the
given population}
}
\value{
the proportional mortality ratio (as a percent) as a numeric vector
}
\description{
This function calculates the proportional mortality ratio for a given population.
}
\examples{

# Data from Reference 2 and Reference 3

library(iemisc)

prop_mortality_ratio(cause_deaths = 652486, total_deaths = 3458697)
# annual heart disease deaths & total deaths in the US in 2021




}
\references{
\enumerate{
Michael Darcy and Łucja Zaborowska, MD, PhD, Last updated on Nov 05, 2022, "Mortality Rate Calculator", Omni Calculator, \url{https://www.omnicalculator.com/health/mortality-rate}.
Florida Museum of Natural History: International Shark Attack File, Last updated on 07/19/2022, "Risk of Death: 18 Things More Likely to Kill You Than Sharks", \url{https://www.floridamuseum.ufl.edu/shark-attacks/odds/compare-risk/death/}.
Farida B. Ahmad, MPH, Jodi A. Cisewski, MPH, Robert N. Anderson, PhD, \emph{MMWR Morb Mortal Wkly Rep} 2022, 71:597-600, "Provisional Mortality Data — United States, 2021", \url{https://www.cdc.gov/mmwr/volumes/71/wr/mm7117e1.htm}.
 }
}
\author{
Irucka Embry
}
