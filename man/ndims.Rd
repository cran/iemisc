% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/size.R
\encoding{UTF-8}
\name{ndims}
\alias{ndims}
\title{Number of dimensions in an Array (GNU Octave/MATLAB compatible)}
\usage{
ndims(x)
}
\arguments{
\item{x}{An array (array, matrix, vector)}
}
\value{
"Return the number of dimensions of a. For any array, the result
  will always be greater than or equal to 2. Trailing singleton dimensions
  are not counted." Source: Eaton page 46.
}
\description{
Obtain the number of dimensions of an array [arrays, matrices, and vectors]
in a manner compatible with GNU Octave/MATLAB.
}
\examples{

library(iemisc)

# Examples from GNU Octave ndims

b <- matlab::ones(c(4, 1, 2, 1))

ndims(b)





}
\references{
\enumerate{
   \item Samit Basu (2002-2006). FreeMat v4.0, \url{https://freemat.sourceforge.net/help/inspection_ndims.html}.
   \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 46.
}
}
\seealso{
\code{\link{size}}
}
\author{
Irucka Embry, Samit Basu (FreeMat)
}
