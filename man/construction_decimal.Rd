% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construction_fraction.R
\encoding{UTF-8}
\name{construction_decimal}
\alias{construction_decimal}
\title{Construction Decimal}
\source{
\enumerate{
   \item removing all non-numeric characters from a string, but not "." - R help on nabble.com answered by David Winsemius on Jul 26, 2016. See \url{https://web.archive.org/web/20190730141421/http://r.789695.n4.nabble.com/removing-all-non-numeric-characters-from-a-string-but-not-quot-quot-td4723146.html}. Retrieved thanks to the Internet Archive: Wayback Machine
   \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
   \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
   \item regex - Replace single backslash in R - Stack Overflow answered and edited by Hong Ooi on Aug 21, 2014. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/25424382/replace-single-backslash-in-r}.
}
}
\usage{
construction_decimal(
  measurement,
  result = c("traditional", "librecad"),
  output = c("vector", "table")
)
}
\arguments{
\item{measurement}{character vector that contains the construction
measurement (foot + inch)}

\item{result}{character vector that contains the decimal type [options are
traditional (ex. 1.203125 = 1'-2 7/16\"] where the whole number is the
value in ft and the decimal is the value in inches & librecad
(ex. 14.43112 = 1'-2 7/16\"), whereby LibreCAD defines its decimal unit as
"integer part separated from the fractional part of a number by a
decimal". Thus, both the whole number and the decimal is the value in
inches.}

\item{output}{character vector that contains the type of output. The
options are vector (just the single value as a decimal) and table [the
decimal value in inch (in), feet (ft), yard (yd), millimeters (mm),
centimeters (cm), and meters (m)].}
}
\value{
the construction measurement value as a numeric \code{\link[base]{vector}}
    as a decimal or as a table (depends on the output parameters)
}
\description{
Convert a construction measurement in US Customary Units (foot + inch) with
or without a fraction into its equivalent as a decimal
}
\note{
If you only have a measurement in inches, then use \code{\link{frac_to_numeric}}
instead.
}
\examples{

# Please refer to the iemisc: Construction Measurement Examples vignette for
# additional examples

# Example 1

library(iemisc)

construction_decimal("2'-0\"", result = "traditional", output = "vector")

construction_decimal("1'-2 7/16\"", result = "librecad", output = "vector")



# Example 2

library(iemisc)

construction_decimal("0 6", result = "traditional", output = "vector")
# read as 0 feet 6 inches

construction_decimal("0 6", result = "librecad", output = "vector")
# read as 0 feet 6 inches


# Example 3

library(iemisc)

tss1 <- "48'-0 1/2\""
tss2 <- "56-9 1/2\""

sum(construction_decimal(tss1, result = "traditional", output = "vector"),
construction_decimal(tss2, result = "traditional", output = "vector"))


\donttest{
# See Source 2 and Source 3

# Example 4

library(iemisc)

try(construction_decimal(5, result = "traditional", output =
"vector")) # please see the error message

ex_error <- character(0)
try(construction_decimal(ex_error, result = "traditional",
output = "vector")) # please see the error message

try(construction_decimal(NA, result = "traditional", output =
"vector")) # please see the error message

try(construction_decimal("feet", result = "traditional", output =
"vector")) # please see the error message
}



# Example 5

library(iemisc)

app1 <- "5' 2\""

app2 <- "6' 3\""

app3 <- construction_decimal(app1, result = "traditional", output = "vector") *
construction_decimal(app2, result = "traditional", output = "vector")
app3

# If you want to have the fractional value using 16ths, do the following:

construction_fraction(app3, type = "traditional", result = "traditional",
fraction = 16)





}
\references{
\enumerate{
   \item LibreCAD v2.2.0 - User Manual - Fundamentals: Units, 7 May 2022, \url{https://librecad-docs-dev.readthedocs.io/en/latest/ref/fundamentals.html#units}.
   \item Spike, 1 January 2022, "Foot and Inch to Decimal Format Conversion", \url{https://www.spikevm.com/calculators/fraction-decimal-calculators.php}.
}
}
\author{
Irucka Embry
}
