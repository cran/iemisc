% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concr_mix_normal_strength.R
\encoding{UTF-8}
\name{concr_mix_normal_strength}
\alias{concr_mix_normal_strength}
\title{Concrete Mix Design for Normal Strength (Normal-weight) Concrete}
\source{
\enumerate{
   \item r - Error when doing bilinear interpolation with 'interp2 {pracma}'; any better way for 2D interpolation? - Stack Overflow answered and edited by Zheyuan Li on Dec 8 2016. See \url{https://stackoverflow.com/questions/41032225/error-when-doing-bilinear-interpolation-with-interp2-pracma-any-better-way}.
   \item r - data.table 1.10.0 - why does a named column index value not work while a integer column index value works without with = FALSE - Stack Overflow answered and edited by Matt Dowle on Dec 8 2016. See \url{https://stackoverflow.com/questions/41032225/error-when-doing-bilinear-interpolation-with-interp2-pracma-any-better-way}.
}
}
\usage{
concr_mix_normal_strength(
  fc,
  slump_use = NULL,
  max_size_aggr,
  FM,
  dry_rod_wt_aggr,
  mc_coarse,
  mc_fine,
  entrainment = c("Nonair", "Air"),
  construction_type = c("Reinforced Foundation walls and footings",
    "Plain footings and caissons", "Slabs, beams and reinforced walls",
    "Building Columns", "Pavements and slabs", "Heavy mass construction"),
  slump_value = c("Maximum", "Maximum + 1", "Minimum"),
  exposure = c("Nonair", "Mild", "Moderate", "Extreme"),
  trial_batch = c("1 cubic yard", "1 cubic foot", "0.5 cubic foot", "0.2 cubic foot",
    "All")
)
}
\arguments{
\item{fc}{numeric vector that contains the concrete compressive strength
(psi)}

\item{slump_use}{numeric vector that contains the amount of slump (in)}

\item{max_size_aggr}{numeric vector that contains the maximum aggregate size
(in)}

\item{FM}{numeric vector that contains the "Fineness Modulus of sand"
(dimensionless)}

\item{dry_rod_wt_aggr}{numeric vector that contains the dry rodded weight of
aggregate (lb/ft^3)}

\item{mc_coarse}{numeric vector that contains the moisture content of the
coarse aggregate (whole number percent)}

\item{mc_fine}{numeric vector that contains the moisture content of the
fine aggregate (whole number percent)}

\item{entrainment}{character vector that contains either Air or Nonair
entrainment}

\item{construction_type}{character vector that contains the intended type of
construction}

\item{slump_value}{character vector that contains the slump value (Maximum,
Maximum + 1, or Minimum). It is "+ 1 in. for methods of consolidation
other than vibration"}

\item{exposure}{character vector that contains the exposure value (Mild,
Moderate, or Extreme) for use with Air entrained concrete mixes or Nonair to
indicate that it is a Nonair entrained concrete mix}

\item{trial_batch}{character vector that contains the volume of the trial
batch mix to return (1 cubic yard, 1 cubic foot, 0.5 cubic foot, 0.2
cubic foot, or All)}
}
\value{
the amounts of cement, sand, gravel, and water in lb, rounded to the
   hundredth, as a \code{\link[base]{list}} to make 1 yd^3, 1 ft^3, 0.5 ft^3,
   or 0.2 ft^3 of normal strength concrete or as a \code{\link[data.table]{data.table}} containing
   all batch volumes.
}
\description{
Calculates the amount of cement, sand, gravel, and water needed for a test
batch volume of normal strength concrete using the volumetric method.
}
\examples{

library(iemisc)

# 'Example 3.1 Mixture Design of Normal-weight Concrete' from Nawy
# (page 23-28)
# Design a concrete mix for 4000 psi concrete strength, beam, and a maximum
# size of aggregate = 3/4 in, with Fineness Modulus of sand = 2.6, the dry
# rodded weight of aggregate = 100 lb/ft^3^, and a moisture content of 3\\%
# for the coarse aggregate and 2\\% for the fine aggregate.

concr_mix_normal_strength(fc = 4000, max_size_aggr = 3 / 4, FM = 2.6,
dry_rod_wt_aggr = 100, mc_coarse = 3, mc_fine = 2, entrainment = "Nonair",
construction_type = "Reinforced Foundation walls and footings", slump_value
= "Maximum", exposure = "Nonair", trial_batch = "1 cubic yard")




}
\references{
Edward G. Nawy, \emph{Reinforced Concrete: A Fundamental Approach}, 5th Edition, Upper Saddle River, New Jersey: Pearson Prentice Hall, 2005, page 23-28.
}
\seealso{
\code{\link{concr_mix_lightweight_strength}} for Concrete Mix Design for
Structural Lightweight Concrete
}
\author{
Irucka Embry, Hans Werner Borchers for the interp1 and interp2 functions from pracma
}
