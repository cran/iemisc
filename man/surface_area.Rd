% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surface_area.R
\encoding{UTF-8}
\name{surface_area}
\alias{surface_area}
\title{Calculate the Total Surface Area of Linear Surfaces}
\usage{
surface_area(
  length = NULL,
  width = NULL,
  surface_area_table = NULL,
  lw_units = c("inch", "feet", "survey_foot", "yard", "mile", "centimeter", "meter",
    "kilometer")
)
}
\arguments{
\item{length}{numeric vector containing the length value(s) in one of the
lw_units values.}

\item{width}{numeric vector containing the width value(s) in one of the
lw_units values.}

\item{surface_area_table}{data.frame/data.table/tibble, list, or matrix
containing the length in column 1 and the width in column 2}

\item{lw_units}{character vector containing the units for the length and the
width (default = "feet"). The other possible units are "inch",
"survey_foot", "yard", "mile", "centimeter", "meter", or "kilometer". The
units should be consistent and not mixed.}
}
\value{
surface area as a numeric vector in the provided square units. The
   calculated value will be the same for all units in this function. The
   units specified in this function are used in the \code{\link{rain_garden_driveway}}
   function.
}
\description{
This function computes the total surface area of linear surfaces (total sum
of width x length). This function was created to use in drainage area
calculations; however, it can be used in other calculations as well.
}
\examples{

# Note: the units must be consistent

# Example 1

library(iemisc)

length1 <- c(220, 150, 30)
width1 <- c(75, 89, 80)
surface_area(width = width1, length = length1, lw_units = "meter")


# Example 2

library(iemisc)

length2 <- c(333, 681, 73)
width2 <- c(17.4, 9.5, 8)
surface_area_table = list(Length = length2, Width = width2)
surface_area(surface_area_table = surface_area_table, lw_units = "mile")









}
\seealso{
\code{\link{rain_garden_driveway}} for calculating the rain garden size for driveways
}
\author{
Irucka Embry
}
