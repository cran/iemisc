#' Trapezoidal cross-section for the Gauckler-Manning-Strickler equation
#'
#' This function solves for one missing variable in the Gauckler-Manning-
#' Strickler equation for a trapezoidal cross-section and uniform flow.
#'
#'
#'
#'
#' Gauckler-Manning-Strickler equation is expressed as
#'
#' \deqn{V = \frac{K_n}{n}R^\frac{2}{3}S^\frac{1}{2}}
#'
#' \describe{
#'	\item{\emph{V}}{the velocity (m/s or ft/s)}
#'	\item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'	\item{\emph{R}}{the hydraulic radius (m or ft)}
#'	\item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'	\item{\emph{\eqn{K_n}}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#'
#'
#'
#' This equation is also expressed as
#'
#' \deqn{Q = \frac{K_n}{n}\frac{A^\frac{5}{3}}{P^\frac{2}{3}}S^\frac{1}{2}}
#'
#' \describe{
#'	\item{\emph{Q}}{the discharge [m^3/s or ft^3/s (cfs)] is VA}
#'	\item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'	\item{\emph{P}}{the wetted perimeter of the channel (m or ft)}
#'	\item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'	\item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'	\item{\emph{\eqn{K_n}}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#'
#'
#'
#' Other important equations regarding the trapezoidal cross-section follow:
#' \deqn{R = \frac{A}{P}}
#'
#' \describe{
#'	\item{\emph{R}}{the hydraulic radius (m or ft)}
#'	\item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'	\item{\emph{P}}{the wetted perimeter of the channel (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{A = y\left(b + my\right)}
#'
#' \describe{
#'	\item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'	\item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'	\item{\emph{m}}{the horizontal side slope}
#'	\item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{P = b + 2y\sqrt{\left(1 + m^2\right)}}
#'
#' \describe{
#'	\item{\emph{P}}{the wetted perimeter of the channel (m or ft)}
#'	\item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'	\item{\emph{m}}{the horizontal side slope}
#'	\item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{B = b + 2my}
#'
#' \describe{
#'	\item{\emph{B}}{the top width of the channel (m or ft)}
#'	\item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'	\item{\emph{m}}{the horizontal side slope}
#'	\item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#' A rough turbulent zone check is performed on the water flowing in the
#' channel using the Reynolds number (Re). The Re equation follows:
#'
#' \deqn{Re = \frac{\rho RV}{\mu}}
#'
#' \describe{
#'	\item{\emph{Re}}{the velocity (m/s or ft/s)}
#'	\item{\emph{\eqn{\rho}}}{density (kg/m^3 or slug/ft^3)}
#'	\item{\emph{R}}{the hydraulic radius (m or ft)}
#'	\item{\emph{V}}{the velocity (m/s or ft/s)}
#'	\item{\emph{\eqn{\mu}}}{dynamic viscosity (* 10^-3 kg/m*s or * 10^-5 lb*s/ft^2)}
#' }
#'
#'
#'
#' @note
#' Assumptions: uniform flow, prismatic channel, and surface water temperature
#' of 20 degrees Celsius (68 degrees Fahrenheit) at atmospheric pressure
#'
#' Note: Units must be consistent
#'
#'
#' @param Q numeric vector that contains the discharge value [m^3/s or ft^3/s],
#'   if known.
#' @param n numeric vector that contains the Manning's roughness coefficient n,
#'   if known.
#' @param b numeric vector that contains the bottom width, if known.
#' @param m numeric vector that contains the "cross-sectional side slope of m:1
#'   (horizontal:vertical)", if known.
#' @param Sf numeric vector that contains the bed slope (m/m or ft/ft),
#'   if known.
#' @param y numeric vector that contains the flow depth (m or ft), if known.
#' @param T numeric vector that contains the temperature (degrees C or degrees
#'   Fahrenheit), if known.
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)]
#'
#' @return the missing parameter (Q, n, b, m, Sf, or y) & area (A), wetted
#'   perimeter (P), velocity (V), top width (B), R (hydraulic radius), and Re
#'   (Reynolds number) as a \code{\link[base]{list}}.
#'
#'
#' @source
#' r - Better error message for stopifnot? - Stack Overflow answered by Andrie on Dec 1 2011. See \url{http://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot}.
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 8, 36, 102, 120, 153-154.
#'    \item Dan Moore, P.E., NRCS Water Quality and Quantity Technology Development Team, Portland Oregon, "Using Mannings Equation with Natural Streams", August 2011, \url{http://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/xsec/manningsNaturally.pdf}.
#'    \item Gilberto E. Urroz, Utah State University Civil and Environmental Engineering, CEE6510 - Numerical Methods in Civil Engineering, Spring 2006, "Solving selected equations and systems of equations in hydraulics using Matlab", August/September 2004, \url{http://ocw.usu.edu/Civil_and_Environmental_Engineering/Numerical_Methods_in_Civil_Engineering/}.
#'    \item Tyler G. Hicks, P.E., \emph{Civil Engineering Formulas: Pocket Guide}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2002, page 423, 425.
#'    \item Andrew Chadwick, John Morfett, and Martin Borthwick, \emph{Hydraulics in Civil and Environmental Engineering}, Fourth Edition, New York City, New York: Spon Press, 2004, pages 132-133.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 26 November 2015, “Manning formula”, \url{https://en.wikipedia.org/wiki/Manning_formula}.
#'    \item John C. Crittenden, R. Rhodes Trussell, David W. Hand, Kerry J. Howe, George Tchobanoglous, \emph{MWH's Water Treatment: Principles and Design}, Third Edition, Hoboken, New Jersey: John Wiley & Sons, Inc., 2012, page 1861-1862.
#'    \item Andrew Chadwick, John Morfett and Martin Borthwick, \emph{Hydraulics in Civil and Environmental Engineering}, Fourth Edition, New York City, New York: Spon Press, Inc., 2004, page 133.
#' }
#'
#' @encoding UTF-8
#'
#'
#'
#'
#' @seealso \code{\link{Manningrect}} for a rectangular cross-section, \code{\link{Manningtri}}
#'   for a triangular cross-section, \code{\link{Manningpara}} for a parabolic
#'   cross-section, and \code{\link{Manningcirc}} for a circular cross-section.
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#' library(iemiscdata)
#' # Exercise 4.1 from Sturm (page 153)
#' Manningtrap(Q = 3000, b = 40, m = 3, Sf = 0.002, n = 0.025, units = "Eng")
#' # Q = 3000 cfs, b = 40 ft, m = 3, Sf = 0.002 ft/ft, n = 0.025,
#' # units = English units
#' # This will solve for y since it is missing and y will be in ft
#'
#'
#' # Modified Exercise 4.5 from Sturm (page 154)
#' Manningtrap(Q = 950, b = 10, m = 2, Sf = 0.022, n = 0.023, units = "SI")
#' # Q = 950 m^3/s, b = 10 m, m = 2, Sf = 0.022 m/m, n = 0.023, units = SI
#' # units
#' # This will solve for y since it is missing and y will be in m
#'
#' # Modified Exercise 4.5 from Sturm (page 154)
#' # See \code{\link[iemiscdata]{nchannel}} for the Manning's n table that the
#' # following example uses
#' # Use the minimum Manning's n value for 1) Natural streams - minor streams
#' # (top width at floodstage < 100 ft), 2) Mountain streams, no vegetation
#' # in channel, banks usually steep, trees and brush along banks submerged at
#' # high stages and 3) bottom: gravels, cobbles, and few boulders.
#' data(nchannel)
#'
#' nlocation <- grep("bottom: gravels, cobbles, and few boulders",
#' nchannel$"Type of Channel and Description")
#' n <- nchannel[nlocation, 2] # 2 for column 2 - Minimum n
#' Manningtrap(Q = 950, b = 10, m = 2, Sf = 0.022, n = n, units = "SI")
#' # Q = 950 m^3/s, b = 10 m, m = 2, Sf = 0.022 m/m, n = 0.03, units = SI
#' # units
#' # This will solve for y since it is missing and y will be in m
#'
#' @importFrom pracma interp1
#' @import data.table
#'
#' @export
Manningtrap <- function (Q = NULL, n = NULL, m = NULL, Sf = NULL, y = NULL, b = NULL, T = NULL, units = c("SI", "Eng")) {

checks <- c(Q, n, m, Sf, y, b)
units <- units

if (length(checks) < 5) {

stop("There are not at least 5 known variables. Try again with at least 5 known variables.")
# Source 1 / only process enough known variables and provide a stop warning if not enough

} else {

if (any(checks == 0)) {

stop("Either Q, n, m, Sf, b, or y is 0. None of the variables can be 0. Try again.")
# Source 1 / only process with a non-zero value for Q, n, m, Sf, b, and y and provide a stop warning if Q, n, m, Sf, b, or y = 0

} else {

if (units == "SI") {

   k <- 1
   
   T <- ifelse(is.null(T), 20, T) # degrees C

   rho = (999.83952 + 16.945176 * T - 7.9870401 * 10 ^ -3 * T ^ 2 - 46.170461 * 10 ^ -6 * T ^ 3 + 105.56302 * 10 ^ -9 * T ^ 4 - 280.54253 * 10 ^ -12 * T ^ 5) / (1 + 16.879850 * 10 ^ -3 * T) # kg / m ^ 3 as density

   if (between(T, 0, 20, incbounds = FALSE)) {

   A <- (1301 / (998.333 + 8.1855 * (T - 20) + 0.00585 * (T - 20) ^ 2)) - 1.30223
   
   mu <- 10 ^ -3 * 10 ^ A # * 10 ^ -3 kg / m * s as dynamic viscosity

   } else if (between(T, 20, 100, incbounds = FALSE)) {
   
   B <- (1.3272 * (20 - T) - 0.001053 * (T - 20) ^ 2) / (T + 105)

   mu <- (1.002 * 10 ^ -3) * (10 ^ B) # * 10 ^ -3 kg / m * s as dynamic viscosity
   
   } else if (T == 0) {
   
   mu <- 1.781 # * 10 ^ -3 kg / m * s as dynamic viscosity
 
   } else if (T == 20) {
   
   mu <- 1.002 # * 10 ^ -3 kg / m * s as dynamic viscosity

   } else if (T == 100) {
   
   mu <- 0.282 # * 10 ^ -3 kg / m * s as dynamic viscosity
   
   }
   
} else if (units == "Eng") {

   k <- 3.2808399 ^ (1 / 3)

   T <- 32
   
   T <- ifelse(is.null(T), 68, T) # degrees F
  
  x <- c(32, 49, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 212)
  y1 <- c(1.94, 1.94, 1.94, 1.938, 1.936, 1.934, 1.931, 1.927, 1.923, 1.918, 1.913, 1.908, 1.902, 1.896, 1.89, 1.883, 1.876, 1.868, 1.86)
  y2 <- c(3.746, 3.229, 2.735, 2.359, 2.05, 1.799, 1.595, 1.424, 1.284, 1.168, 1.069, 0.981, 0.905, 0.838, 0.78, 0.726, 0.678, 0.637, 0.593)

  rho <- interp1(x, y1, T, method = "spline") # slug / ft ^ 3 as density
  mu <- interp1(x, y2, T, method = "spline") * 10 ^ -5 # * 10 ^ -5 lb * s / ft ^ 2
   
} else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {

stop("Incorrect unit system. Try again.")
# Source 1 / only process with a specified unit and provide a stop warning if not

}

if (missing(Q)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P

Q <- (k / n) * (A ^ (5 / 3)) / (P ^ (2 / 3)) * sqrt(Sf)

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow is in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is not in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

return(list(Q = Q, V = V, A = A, P = P, B = B, R = R, Re = Re))


} else if (missing(n)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P

n <- (k / Q) * (A ^ (5 / 3)) / (P ^ (2 / 3)) * sqrt(Sf)

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow is in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is not in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

return(list(n = n, V = V, A = A, P = P, B = B, R = R, Re = Re))


} else if (missing(m)) {

mfun <- function(m) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

muse <- uniroot(mfun, interval = c(0, 30))

m <- muse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow is in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is not in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

return(list(m = m, V = V, A = A, P = P, B = B, R = R, Re = Re))


} else if (missing(b)) {

b <- function(b) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

b <- uniroot(b, interval = c(0, 100))

b <- b$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow is in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is not in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

return(list(b = b, V = V, A = A, P = P, B = B, R = R, Re = Re))


} else if (missing(y)) {

yfun <- function(y) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

yuse <- uniroot(yfun, interval = c(0, 100))

y <- yuse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow is in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is not in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

return(list(y = y, V = V, A = A, P = P, B = B, R = R, Re = Re))


} else if (missing(Sf)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P

V <- Q / A

Re <- (rho * R * V) / mu

Sf <- (Q / ((k / n) * (A ^ (5 / 3)) / (P ^ (2 / 3)))) ^ 2

if (Re > 2000) {

cat("\nFlow is in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is not in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

return(list(Sf = Sf, V = V, A = A, P = P, B = B, R = R, Re = Re))
}
}
}
}
