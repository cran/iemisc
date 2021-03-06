% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cosd.R
\encoding{UTF-8}
\name{asind}
\alias{asind}
\title{Inverse sine (in degrees) [GNU Octave/MATLAB compatible]}
\usage{
asind(x)
}
\arguments{
\item{x}{A numeric vector containing values in degrees}
}
\value{
The inverse sine of each element of \code{x} in degrees.
}
\description{
Calculates the value of inverse sine for each element of \code{x} in degrees
in a manner compatible with GNU Octave/MATLAB.
}
\examples{
library("iemisc")

# Examples from GNU Octave asind
asind (seq(0, 1, by = 0.1))



}
\references{
John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 359.
}
\author{
David Bateman (GNU Octave asind), Irucka Embry
}
