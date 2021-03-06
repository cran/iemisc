% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cosd.R
\encoding{UTF-8}
\name{secd}
\alias{secd}
\title{Secant (in degrees) [GNU Octave/MATLAB compatible]}
\usage{
secd(x)
}
\arguments{
\item{x}{A numeric vector containing values in degrees}
}
\value{
The secant of each element of \code{x} in degrees.
}
\description{
Calculates the value of secant for each element of \code{x} in degrees in a
manner compatible with GNU Octave/MATLAB.
}
\examples{
library("iemisc")

# Examples from GNU Octave secd
secd (seq(0, 80, by = 10))

secd (c(0, 180, 360))

secd (c(90, 270))



}
\references{
John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
}
\author{
David Bateman (GNU Octave secd), Irucka Embry
}
