% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engr_survey3.R
\encoding{UTF-8}
\name{engr_survey3}
\alias{engr_survey3}
\title{Calculate the Distance between Engineering Survey Points (Length or Number of Stations)}
\usage{
engr_survey3(
  length1,
  station_distance = 100,
  units = c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers"),
  output = c("numeric", "string")
)
}
\arguments{
\item{length1}{character vector that contains the beginning engineering
survey station value}

\item{station_distance}{numeric vector that contains the horizontal distance
between any 2 points along the survey, the default is 100 feet}

\item{units}{character vector that contains the system of units for the
\code{station_distance} (options are \code{foot}, \code{survey_ft}
(United States Customary System) [US survey foot], \code{meters} for
International System of Units meters, or \code{kilometers} for International System
of Units kilometers)}

\item{output}{character vector that contains the system of units for the
horizontal length (options are \code{foot}, \code{survey_ft}
(United States Customary System) [US survey foot], \code{survey_mile}
(United States Customary System) [US survey mile], \code{mile},
\code{meters} for International System of Units meters, or \code{kilometers} for
International System of Units kilometers)}
}
\value{
horizontal length as a numeric vector (ex. 1214.402) or as a
    character vector with the word stations after the number (ex. 1214.402
    stations)
}
\description{
Takes engineering survey points in various units (foot, US survey foot,
meters, or kilometers) and calculates the horizontal length in various units
(foot, US survey foot, US survey mile, mile, meters, or kilometers).
}
\examples{

# Please refer to the iemisc: Engineering Survey Examples vignette for
# additional examples

# Example 1

library(iemisc)

# "What the others said is correct. 1 station is equal to 100 feet. So when
# asked how many stations are in (3.2mi x 5280ft/mi = ) 16,896 feet, you are being
# asked how many 100 foot-segments are in 16,896 feet? The answer of course is
# 16,896ft / 100ft/sta = 168.96 sta." Source: Reference 2

length1 <- "16,896" # feet

engr_survey3(length1, station_distance = 100, units = "foot", output = "numeric")
engr_survey3(length1, station_distance = 100, units = "foot", output = "string")
# the answer provides the number of stations

# Note: Both answers should be the same as 3.2 miles = 16,896 feet.

length2 <- 3.2 # mile

engr_survey3(length2, station_distance = 100, units = "mile", output = "numeric")
engr_survey3(length2, station_distance = 100, units = "mile", output = "string")
# the answer provides the number of stations






}
\references{
\enumerate{
   \item udunits.dat, v 1.18 2006/09/20 18:59:18 steve Exp, \url{https://web.archive.org/web/20230202155021/https://www.unidata.ucar.edu/software/udunits/udunits-1/udunits.txt}. Retrieved thanks to the Internet Archive: Wayback Machine
   \item Engineer Boards: Transportation. "Stationing. Dumb question?" Question asked by By NIKE, August 31, 2013 and answered by ptatohed on September 1, 2013. See \url{https://engineerboards.com/threads/stationing-dumb-question.21935/}.
}
}
\author{
Irucka Embry
}
