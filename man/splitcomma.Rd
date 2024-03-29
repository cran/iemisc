% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitcomma.R
\encoding{UTF-8}
\name{splitcomma}
\alias{splitcomma}
\title{Split Comma}
\source{
\enumerate{
   \item regex - Split on first comma in string - Stack Overflow answered by flodel on Apr 25 2012. See \url{https://stackoverflow.com/questions/10309122/split-on-first-comma-in-string}.
   \item regex - r regexp - replace title and suffix in any part of string with nothing in large file (&gt; 2 million rows) - Stack Overflow answered by Molx on Apr 16 2015. See \url{https://stackoverflow.com/questions/29680131/r-regexp-replace-title-and-suffix-in-any-part-of-string-with-nothing-in-large}.
}
}
\usage{
splitcomma(string)
}
\arguments{
\item{string}{character vector that contains a phrase separated by a comma
or not. If not, there is no change.}
}
\value{
the character \code{\link[base]{vector}} in the form of First Name Last Name
}
\description{
This function takes a character string in the form of Last Name (Surname),
First Name or Second String, First String and transposes that string to
First Name Last Name (Surname) or First String Second String while removing
the comma.
}
\examples{

# Example 1

install.load::load_package("iemisc", "data.table")

dtxx <- data.table(Names = c("Cooler, Wine", "Juice, Fruit", "Hard Water",
"Hot Bath", "Set, Data"))

dtxx[, Corrected_Names := splitcomma(dtxx$Names)]

dtxx



# Example 2

xtrax <- "FALSER, BRATTIE & SIMX, AGONY"

splitcomma(xtrax)



}
\author{
Irucka Embry
}
