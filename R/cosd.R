#' Cosine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of cosine for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB. Zero is returned for any "elements
#' where (\code{x} - 90) / 180 is an integer." Source: Eaton.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The cosine of each element of \code{x} in degrees. Zero for any
#' "elements where (\code{x} - 90) / 180 is an integer."
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave cosd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave cosd
#' cosd(seq(0, 80, by = 10))
#'
#' cosd(pi * seq(0, 80, by = 10) / 180)
#'
#' cosd(c(0, 180, 360))
#'
#' cosd(c(90, 270, 45))
#'
#'
#' @importFrom pracma Fix
#'
#' @export
cosd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

  I <- x / 180

  y <- cos (I * pi)

  I <- I + 0.5

y <- ifelse(I == Fix(I) & is.finite(I), 0, y)

  return(y)

}




#' Inverse cosine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse cosine for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse cosine of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 359.
#'
#'
#'
#' @author David Bateman (GNU Octave acosd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave acosd
#' acosd (seq(0, 1, by = 0.1))
#'
#'
#'
#' @export
acosd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

  y <- acos (x) * 180 / pi

  return(y)

}



#' Sine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of sine for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB. Zero is returned for any "elements
#' where \code{x} / 180 is an integer." Source: Eaton.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The sine of each element of \code{x} in degrees. Zero for any
#' "elements where \code{x} / 180 is an integer."
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave sind), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave sind
#' sind(seq(10, 90, by = 10))
#'
#' sind(c(0, 180, 360))
#'
#' sind(c(90, 270))
#'
#'
#' @importFrom pracma Fix
#'
#' @export
sind <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

  I <- x / 180

  y <- sin (I * pi)

y <- ifelse(I == Fix(I) & is.finite(I), 0, y)

  return(y)

}



#' Inverse sine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse sine for each element of \code{x} in degrees
#' in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse sine of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 359.
#'
#'
#'
#' @author David Bateman (GNU Octave asind), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave asind
#' asind (seq(0, 1, by = 0.1))
#'
#'
#'
#' @export
asind <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

  y <- asin (x) * 180 / pi

  return(y)

}




#' Tangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of tangent for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB. Zero is returned for any "elements
#' where \code{x} / 180 is an integer and \code{Inf} for elements where
#' (\code{x} - 90) / 180 is an integer." Source: Eaton.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The tangent of each element of \code{x} in degrees. Zero for any
#' "elements where \code{x} / 180 is an integer and \code{Inf} for elements where
#' (\code{x} - 90) / 180 is an integer."
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave tand), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave tand
#' tand(seq(10, 80, by = 10))
#'
#' tand(c(0, 180, 360))
#'
#' tand(c(90, 270))
#'
#'
#' @importFrom pracma Fix
#'
#' @export
tand <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

  I0 <- x / 180

  I90 <- (x - 90) / 180

  y <- tan (I0 * pi)

y <- ifelse(I0 == Fix(I0) & is.finite(I0), 0, y)

y <- ifelse(I90 == Fix(I90) & is.finite(I90), Inf, y)

  return(y)

}



#' Inverse tangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse tangent for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse tangent of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 359.
#'
#'
#'
#' @author David Bateman (GNU Octave atand), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave atand
#' atand (seq(0, 90, by = 10))
#'
#'
#'
#' @export
atand <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

  y <- 180 / pi * atan (x)

  return(y)

}



#' Secant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of secant for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The secant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave secd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave secd
#' secd (seq(0, 80, by = 10))
#'
#' secd (c(0, 180, 360))
#'
#' secd (c(90, 270))
#'
#'
#'
#' @export
secd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

    y <- 1 / cosd (x)

  return(y)

}




#' Inverse secant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse secant for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse secant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave asecd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave asecd
#' asecd (seq(0, 90, by = 10))
#'
#'
#'
#' @importFrom pracma asec
#'
#'
#' @export
asecd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

    y <- asec (x) * 180 / pi

  return(y)

}



#' Cosecant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of cosecant for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The cosecant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave cscd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave cscd
#' cscd (seq(0, 90, by = 10))
#'
#' cscd (c(0, 180, 360))
#'
#' cscd (c(90, 270))
#'
#'
#' @export
cscd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

    y <- 1 / sind (x)

  return(y)

}



#' Inverse cosecant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse cosecant for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse cosecant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 359.
#'
#'
#'
#' @author David Bateman (GNU Octave acscd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave acscd
#' acscd (seq(0, 90, by = 10))
#'
#'
#' @importFrom pracma acsc
#'
#'
#' @export
acscd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

    y <- acsc (x) * 180 / pi

  return(y)

}





#' Cotangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse secant for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse secant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author David Bateman (GNU Octave cotd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave cotd
#' cotd (seq(0, 80, by = 10))
#'
#' cotd (c(0, 180, 360))
#'
#' cotd (c(90, 270))
#'
#'
#'
#'
#' @export
cotd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

    y <- 1 / tand (x)

  return(y)

}




#' Inverse cotangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse cotangent for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse cotangent of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 359.
#'
#'
#'
#' @author David Bateman (GNU Octave acotd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave acotd
#' acotd (seq(0, 90, by = 10))
#'
#'
#'
#'
#' @export
acotd <- function (x) {

if (nargs() != 1) {

    stop("There should only be one argument.")

}

stopifnot(is.numeric(x)) # from pracma trisolve

    y <- atand (1 / x)

  return(y)

}





#' "Two-argument arc-tangent" (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of the "two-argument arc-tangent" for each element of
#' (\code{y}, \code{x}) in degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#' @param y A numeric vector containing values in degrees
#'
#' @return The "two-argument arc-tangent" of each element of (\code{y}, \code{x})
#'         in degrees. Note: "The arc-tangent of two arguments atan2(y, x)
#'         returns the angle between the x-axis and the vector from the origin
#'         to (x, y), i.e., for positive arguments atan2(y, x) == atan(y/x)."
#'         Source: \code{Trig} (base).
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, and Søren Hauberg (2009). \emph{GNU Octave version 3.0.1 manual: a high-level interactive language for numerical computations}. CreateSpace Independent Publishing Platform. ISBN 1441413006, URL \url{http://www.gnu.org/software/octave/doc/interpreter/}. Page 358.
#'
#'
#'
#' @author Rik Wehbring (GNU Octave atan2d), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # Examples from GNU Octave atan2d
#' atan2d (a <- seq(-1, 1, by = 0.1), b <- seq(1, -1, by = -0.1))
#'
#'
#'
#'
#' @export
atan2d <- function (y, x) {

if (nargs() != 2) {

    stop("There should only be two arguments.")

}

stopifnot(is.numeric((c(y, x)))) # from pracma trisolve

    y <- 180 / pi * atan2 (y, x)

  return(y)

}
