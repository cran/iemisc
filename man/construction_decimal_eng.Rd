% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construction_fraction.R
\encoding{UTF-8}
\name{construction_decimal_eng}
\alias{construction_decimal_eng}
\title{Construction Decimal Engineering (LibreCAD Style)}
\source{
regex - Replace single backslash in R - Stack Overflow answered and edited by Hong Ooi on Aug 21, 2014. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/25424382/replace-single-backslash-in-r}.
}
\usage{
construction_decimal_eng(measurement)
}
\arguments{
\item{measurement}{character or numeric vector that contains the construction
measurement (decimal or foot + inch)}
}
\value{
the engineering construction measurement value as a character \code{\link[base]{vector}}.
    In LibreCAD, a construction measurement of 1'-2 7/16" = 14.43112 (decimal) =
    1'-2.43112" (engineering).
}
\description{
Convert a construction measurement in US Customary Units (foot + inch) with
or without a fraction into its equivalent as an Engineering value (LibreCAD
style)
}
\examples{

# Please refer to the iemisc: Construction Measurement Examples vignette for
# additional examples

library(iemisc)

librecad1a <- "1 ft 2 7/16\""

construction_decimal_eng(librecad1a)


librecad4a <- 14.43112

construction_decimal_eng(librecad4a)


librecad5a <- 14.4375

construction_decimal_eng(librecad5a)


librecad6a <- 17.71354

construction_decimal_eng(librecad6a)


librecad7a <- 86.000000

construction_decimal_eng(librecad7a)


checkst <- 14.43112

construction_decimal_eng(checkst)

construction_fraction(checkst, type = "librecad", result = "traditional",
fraction = 16)








}
\references{
\enumerate{
   \item LibreCAD, User Manual - Fundamentals: Units: Engineering and Decimal, 7 May 2022, \url{https://librecad-docs-dev.readthedocs.io/en/latest/ref/fundamentals.html#units}.
   \item LibreCAD rs_units.cpp code referencing how to calculate the engineering units.
}
}
\author{
Irucka Embry, R. van Twisk (rs_units.cpp code)
}
