% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/size.R
\encoding{UTF-8}
\name{numel}
\alias{numel}
\title{Number of elements (GNU Octave/MATLAB compatible)}
\source{
\enumerate{
   \item r - Add a Column to a Dataframe From a List of Values - Stack Overflow answered by Matthew Plourde on Jun 21 2012. See \url{https://stackoverflow.com/questions/11130037/add-a-column-to-a-dataframe-from-a-list-of-values/11130178}.
   \item r - Why does is.vector() return TRUE for list? - Stack Overflow answered by Andrie on May 17 2011. See \url{https://stackoverflow.com/questions/6032772/why-does-is-vector-return-true-for-list/6032909}.
}
}
\usage{
numel(x, ...)
}
\arguments{
\item{x}{An R object (array, matrix, vector)}

\item{...}{R objects (indices idx1, idx2, ...)}
}
\value{
"Return the number of elements in the R object x. Optionally, if
  indices idx1, idx2, ... are supplied, return the number of elements that
  would result from the indexing a(idx1, idx2, ...)." Source: Eaton page 41.
}
\description{
Obtain the number of elements of R objects [arrays, matrices, and vectors
(including lists)] in a manner compatible with GNU Octave/MATLAB. Some
documentation from \code{\link[base]{length}}.
}
\examples{
library("iemisc")
import::from(pracma, ones)
xx <- list(1:26, 1:10)
numel(xx)

# Examples from GNU Octave numel
a <- 1
b <- ones(2, 3)
numel(a, b)

a <- 2
b <- ones(2, 3)
c <- ones(3, 4)
numel(a, b)
numel(a, b, c)

f <- matrix(c(10, 12, 23, 21, 62, 93), nrow = 2, ncol = 3, byrow = TRUE)
g <- c(2, 4)
numel(f, g)


\dontrun{
# check against GNU Octave
library(RcppOctave) # requires Octave (>= 3.2.4) and its development files
o_source(text = "
xx = {1:26, 1:10}

\\% Examples from GNU Octave numel
a = 1;
b = ones(2, 3);
numel(a, b)

a = 2;
b = ones(2, 3);
c = ones(3, 4);
numel(a, b)
numel(a, b, c)

f = [10 12 23; 21 62 93];
g = [2 4];
numel(f, g)
")
}


}
\references{
\enumerate{
   \item Samit Basu (2002-2006). FreeMat v4.0, \url{http://freemat.sourceforge.net/}.
   \item John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 41.
}
}
\seealso{
\code{\link[matlab]{numel}}, \code{\link[pracma]{numel}}, \code{\link{size}}, \code{\link{length}}
}
\author{
Irucka Embry, Samit Basu (FreeMat)
}
