% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/floor_division.R
\encoding{UTF-8}
\name{\%//\%}
\alias{\%//\%}
\title{Floor Division (Python compatible)}
\usage{
x \%//\% y
}
\arguments{
\item{x}{numeric vector}

\item{y}{numeric vector}
}
\value{
The integral part of the quotient is returned.
}
\description{
Performs floor (or integer) division on 2 numeric vectors.
}
\examples{

# Example 1 -- From the Python reference

library(iemisc)

5.0 / 2
# 2.5

5.0 \%//\% 2
# 2.0






}
\references{
Jakub Przywóski: "Python Reference (The Right Way)". // floor division, 2015, Revision 9a3b94e7, \url{https://python-reference.readthedocs.io/en/latest/docs/operators/floor_division.html}.
}
\author{
Irucka Embry
}
