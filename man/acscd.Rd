% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cosd.R
\encoding{UTF-8}
\name{acscd}
\alias{acscd}
\title{Inverse cosecant (in degrees) [GNU Octave/MATLAB compatible]}
\usage{
acscd(x)
}
\arguments{
\item{x}{A numeric vector containing values in degrees}
}
\value{
The inverse cosecant of each element of \code{x} in degrees.
}
\description{
Calculates the value of inverse cosecant for each element of \code{x} in
degrees in a manner compatible with GNU Octave/MATLAB.
}
\note{
Note: If you have a radian (rad) angle value, use \code{\link[pracma]{acsc}} instead.
}
\examples{

library(iemisc)

# Examples from GNU Octave acscd
acscd (seq(0, 90, by = 10))


}
\references{
John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
}
\author{
David Bateman (GNU Octave acscd), Irucka Embry
}
