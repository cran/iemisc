#' Circular cross-section using the Gauckler-Manning-Strickler equation
#'
#' Manningcirc and Manningcircy solve for a missing variable for a circular
#' cross-section. The \code{\link[stats]{uniroot}} function is used to obtain the
#' missing parameter.
#'
#' The Manningcirc function solves for one missing variable in the Gauckler-
#' Manning equation for a circular cross-section and uniform flow. The
#' possible inputs are Q, n, Sf, y, and d. If y or d are not initially known,
#' then Manningcircy can solve for y or d to use as input in the Manningcirc
#' function.
#'
#' The Manningcircy function solves for one missing variable in the Gauckler-
#' Manning equation for a circular cross-section and uniform flow. The possible
#' inputs are y, d, y_d (ratio of y/d), and theta.
#'
#'
#'
#'
#' Gauckler-Manning-Strickler equation is expressed as
#'
#' \deqn{V = \frac{K_n}{n}R^\frac{2}{3}\sqrt{S}}
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
#' \deqn{Q = \frac{K_n}{n}\frac{A^\frac{5}{3}}{P^\frac{2}{3}}\sqrt{S}}
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
#' Other important equations regarding the circular cross-section follow:
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
#' \deqn{A = \left(\theta - \sin \theta\right) \frac{d^2}{8}}
#'
#' \describe{
#'	\item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'	\item{\emph{d}}{the diameter of the cross-section (m or ft)}
#'	\item{\emph{\eqn{\theta}}}{see the equation defining this parameter}
#' }
#'
#'
#' \deqn{\theta = 2 \arcsin\left[1 - 2\left(\frac{y}{d}\right)\right]}
#' \describe{
#'	\item{\emph{\eqn{\theta}}}{see the equation defining this parameter}
#'	\item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'	\item{\emph{d}}{the diameter of the cross-section (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{d = 1.56 \left[\frac{nQ}{K_n\sqrt{S}}\right]^\frac{3}{8}}
#' \describe{
#'	\item{\emph{d}}{the initial diameter of the cross-section [m or ft]}
#'	\item{\emph{Q}}{the discharge [m^3/s or ft^3/s (cfs)] is VA}
#'	\item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'	\item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'	\item{\emph{\eqn{K_n}}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#' Note: This will only provide the initial conduit diameter, check the design
#'       considerations to determine your next steps.
#'
#'
#'
#'
#' \deqn{P = \frac{\theta d}{2}}
#'
#' \describe{
#'	\item{\emph{P}}{the wetted perimeter of the channel (m or ft)}
#'	\item{\emph{\eqn{\theta}}}{see the equation defining this parameter}
#'	\item{\emph{d}}{the diameter of the cross-section (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{B = d \sin\left(\frac{\theta}{2}\right)}
#'
#' \describe{
#'	\item{\emph{B}}{the top width of the channel (m or ft)}
#'	\item{\emph{\eqn{\theta}}}{see the equation defining this parameter}
#'	\item{\emph{d}}{the diameter of the cross-section (m or ft)}
#' }
#'
#'
#'
#' \deqn{D = \frac{A}{B}}
#'
#' \describe{
#'	\item{\emph{D}}{the hydraulic depth (m or ft)}
#'	\item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'	\item{\emph{B}}{the top width of the channel (m or ft)}
#' }
#'
#'
#' A rough turbulent zone check is performed on the water flowing in the
#' channel using the Reynolds number (Re). The Re equation follows:
#'
#' \deqn{Re = \frac{\rho RV}{\mu}}
#'
#' \describe{
#'	\item{\emph{Re}}{Reynolds number (dimensionless)}
#'	\item{\emph{\eqn{\rho}}}{density (kg/m^3 or slug/ft^3)}
#'	\item{\emph{R}}{the hydraulic radius (m or ft)}
#'	\item{\emph{V}}{the velocity (m/s or ft/s)}
#'	\item{\emph{\eqn{\mu}}}{dynamic viscosity (* 10^-3 kg/m*s or * 10^-5 lb*s/ft^2)}
#' }
#'
#'
#'
#' A critical flow check is performed on the water flowing in the channel
#' using the Froude number (Fr). The Fr equation follows:
#'
#' \deqn{Fr = \frac{V}{\left(\sqrt{g * D}\right)}}
#'
#' \describe{
#'	\item{\emph{Fr}}{the Froude number (dimensionless)}
#'	\item{\emph{V}}{the velocity (m/s or ft/s)}
#'	\item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'	\item{\emph{D}}{the hydraulic depth (m or ft)}
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
#'
#'
#' @param Q numeric vector that contains the discharge value [m^3/s or ft^3/s],
#'   if known.
#' @param n numeric vector that contains the Manning's roughness coefficient n,
#'   if known.
#' @param d numeric vector that contains the diameter value (m or ft),
#'   if known.
#' @param Sf numeric vector that contains the bed slope (m/m or ft/ft),
#'   if known.
#' @param y numeric vector that contains the flow depth (m or ft), if known.
#' @param y_d numeric vector that contains the filling ration (y/d), if known.
#' @param theta numeric vector that contains the angle theta (radians), if
#'        known.
#' @param T numeric vector that contains the temperature (degrees C or degrees
#'   Fahrenheit), if known.
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)]
#'
#' @return the missing parameter (Q, n, or Sf) & theta, area (A), wetted
#'   perimeter (P), top width (B), velocity (V), R (hydraulic radius), Re
#'   (Reynolds number), and Fr (Froude number) as a \code{\link[base]{list}} for the
#'   Manningcirc function.
#'
#' @return the missing parameter (d or y) & theta, area (A), wetted
#'   perimeter (P), top width (B), velocity (V), and R (hydraulic radius) as a
#'   \code{\link[base]{list}} for the Manningcircy function.
#'
#'
#' @source
#' r - Better error message for stopifnot? - Stack Overflow answered by Andrie on Dec 1 2011. See \url{http://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot}.
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 2, 8, 36, 102, 120, 123-125, 153-154.
#'    \item Dan Moore, P.E., NRCS Water Quality and Quantity Technology Development Team, Portland Oregon, "Using Mannings Equation with Natural Streams", August 2011, \url{http://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/xsec/manningsNaturally.pdf}.
#'    \item Gilberto E. Urroz, Utah State University Civil and Environmental Engineering, CEE6510 - Numerical Methods in Civil Engineering, Spring 2006, "Solving selected equations and systems of equations in hydraulics using Matlab", August/September 2004, \url{http://ocw.usu.edu/Civil_and_Environmental_Engineering/Numerical_Methods_in_Civil_Engineering/}.
#'    \item Tyler G. Hicks, P.E., \emph{Civil Engineering Formulas: Pocket Guide}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2002, page 423, 425.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 26 November 2015, “Manning formula”, \url{https://en.wikipedia.org/wiki/Manning_formula}.
#'    \item John C. Crittenden, R. Rhodes Trussell, David W. Hand, Kerry J. Howe, George Tchobanoglous, \emph{MWH's Water Treatment: Principles and Design}, Third Edition, Hoboken, New Jersey: John Wiley & Sons, Inc., 2012, page 1861-1862.
#'    \item Andrew Chadwick, John Morfett and Martin Borthwick, \emph{Hydraulics in Civil and Environmental Engineering}, Fourth Edition, New York City, New York: Spon Press, Inc., 2004, page 133.
#'    \item Robert L. Mott and Joseph A. Untener, \emph{Applied Fluid Mechanics}, Seventh Edition, New York City, New York: Pearson, 2015, page 376, 377-378, 392.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 5 May 2016, “Gravitational acceleration”, \url{https://en.wikipedia.org/wiki/Gravitational_acceleration}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 29 May 2016, “Conversion of units”, \url{https://en.wikipedia.org/wiki/Conversion_of_units}.
#' }
#'
#' @encoding UTF-8
#'
#'
#'
#'
#' @seealso \code{\link{Manningtrap}} for a trapezoidal cross-section, \code{\link{Manningrect}} for a
#'   rectangular cross-section, \code{\link{Manningtri}} for a triangular cross-section,
#'   and \code{\link{Manningpara}} for a parabolic cross-section.
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#' library(iemiscdata)
#'
#' # Practice Problem 14.12 from Mott (page 392)
#' y <- Manningcircy(y_d = 0.5, d = 6, units = "Eng")
#'
#' # See npartfull in iemiscdata for the Manning's n table that the
#' # following example uses
#' # Use the normal Manning's n value for 1) Corrugated Metal, 2) Stormdrain.
#'
#' data(npartfull)
#'
#' # We are using the culvert as a stormdrain in this problem
#' nlocation <- grep("Stormdrain",
#' npartfull$"Type of Conduit and Description")
#'
#' n <- npartfull[nlocation, 3] # 3 for column 3 - Normal n
#'
#' Manningcirc(d = 6, Sf = 1 / 500, n = n, y = y$y, units = "Eng")
#' # d = 6 ft, Sf = 1 / 500 ft/ft, n = 0.024, y = 3 ft, units = "Eng"
#' # This will solve for Q since it is missing and Q will be in ft^3/s
#'
#'
#'
#' # Example Problem 14.2 from Mott (page 377-378)
#' y <- Manningcircy(y_d = 0.5, d = 200/1000, units = "SI")
#'
#' # See npartfull in iemiscdata for the Manning's n table that the
#' # following example uses
#' # Use the normal Manning's n value for 1) Clay, 2) Common drainage tile.
#'
#' data(npartfull)
#'
#' nlocation <- grep("Common drainage tile",
#' npartfull$"Type of Conduit and Description")
#'
#' n <- npartfull[nlocation, 3] # 3 for column 3 - Normal n
#'
#' Manningcirc(Sf = 1/1000, n = n, y = y$y, d = 200/1000, units = "SI")
#' # Sf = 1/1000 m/m, n = 0.013, y = 0.1 m, d = 200/1000 m, units = SI units
#' # This will solve for Q since it is missing and Q will be in m^3/s
#'
#'
#'
#' # Example 4.1 from Sturm (page 124-125)
#' Manningcircy(y_d = 0.8, d = 2, units = "Eng")
#'
#' y <- Manningcircy(y_d = 0.8, d = 2, units = "Eng")
#' # defines all list values within the object named y
#'
#' y$y # gives the value of y
#'
#'
#'
#' # Modified Exercise 4.1 from Sturm (page 153)
#' # Note: The Q in Exercise 4.1 is actually found using the Chezy equation,
#' # this is a modification of that problem
#' # See nchannel in iemiscdata for the Manning's n table that the
#' # following example uses
#' # Use the normal Manning's n value for 1) Natural streams - minor streams
#' # (top width at floodstage < 100 ft), 2) Mountain streams, no vegetation
#' # in channel, banks usually steep, trees and brush along banks submerged at
#' # high stages and 3) bottom: gravels, cobbles, and few boulders.
#'
#' data(nchannel)
#'
#' nlocation <- grep("bottom: gravels, cobbles, and few boulders",
#' nchannel$"Type of Channel and Description")
#'
#' n <- nchannel[nlocation, 3] # 3 for column 3 - Normal n
#'
#' Manningcirc(Sf = 0.002, n = n, y = y$y, d = 2, units = "Eng")
#' # Sf = 0.002 ft/ft, n = 0.04, y = 1.6 ft, d = 2 ft, units = English units
#' # This will solve for Q since it is missing and Q will be in ft^3/s
#'
#'
#'
#' # Modified Exercise 4.5 from Sturm (page 154)
#' library(NISTunits)
#'
#' ysi <- NISTftTOmeter(y$y)
#'
#' dsi <- NISTftTOmeter(2)
#'
#' Manningcirc(Sf = 0.022, n = 0.023, y = ysi, d = dsi, units = "SI")
#' # Sf = 0.022 m/m, n = 0.023, y = 0.48768 m, d = 0.6096 m, units = SI units
#' # This will solve for Q since it is missing and Q will be in m^3/s
#'
#'
#' @importFrom pracma interp1
#' @import data.table
#'
#' @name Manningcirc
NULL

#' @export
#' @rdname Manningcirc
Manningcirc <- function (Q = NULL, n = NULL, Sf = NULL, y = NULL, d = NULL, T = NULL, units = c("SI", "Eng")) {

checks <- c(Q, n, Sf, y, d)
units <- units

if (length(checks) < 4) {

stop("There are not at least 4 known variables. Try again with at least 4 known variables.")
# Source 1 / only process enough known variables and provide a stop warning if not enough

} else {

if (any(checks == 0)) {

stop("Either Q, n, Sf, y, or d is 0. None of the variables can be 0. Try again.")
# Source 1 / only process with a non-zero value for Q, n, Sf, y, and d and provide a stop warning if Q, n, Sf, y, or d = 0

} else {

if (units == "SI") {

   k <- 1

   g <- 9.80665 # m / s^2

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

   mu <-  0.282 # * 10 ^ -3 kg / m * s as dynamic viscosity

   }

} else if (units == "Eng") {

   k <- 3.2808399 ^ (1 / 3)

   g <- 9.80665 * (3937 / 1200) # ft / sec^2

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

theta <- 2 * acos(1 - (2 * (y / d)))
A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P
D <- A / B

Qfun <- function(Q) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}

Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")

Q <- Quse$root

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr == 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}

return(list(Q = Q, V = V, A = A, P = P, R = R, Re = Re, Fr = Fr))


} else if (missing(n)) {

theta <- 2 * acos(1 - (2 * (y / d)))
A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P
D <- A / B

nfun <- function(n) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}

nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")

n <- nuse$root

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr == 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}

return(list(n = n, V = V, A = A, P = P, R = R, Re = Re, Fr = Fr))


} else if (missing(Sf)) {

theta <- 2 * acos(1 - (2 * (y / d)))
A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P
D <- A / B

Sffun <- function(Sf) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}

Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")

Sf <- Sfuse$root

V <- Q / A

Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr == 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}

return(list(Sf = Sf, V = V, A = A, P = P, R = R, Re = Re, Fr = Fr))
}
}
}
}


#' @export
#' @rdname Manningcirc
Manningcircy <- function (y = NULL, d = NULL, y_d = NULL, theta = NULL, Sf = NULL, Q = NULL, units = c("SI", "Eng")) {

checks <- c(y, d, y_d, theta, Sf, Q)
units <- units

if (length(checks) < 1) {

stop("There are not at least 1 known variables. Try again with at least 1 known variables.")
# Source 1 / only process enough known variables and provide a stop warning if not enough

} else {

if (any(checks == 0)) {

stop("Either y, d, theta, y_d, Sf, or Q is 0. None of the variables can be 0. Try again.")
# Source 1 / only process with a non-zero value for y, d, theta, y_d, Sf, and Q and provide a stop warning if y, d, theta, y_d, Sf, or Q = 0

} else {

if (units == "SI") {

   k <- 1

} else if (units == "Eng") {

   k <- 3.2808 ^ (1 / 3)

} else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {

stop("Incorrect unit system. Try again.")
# Source 1 / only process with a specified unit and provide a stop warning if not

}

if (missing(y) & missing(y_d)) {

y <- (d / 2) * (1 - cos(theta / 2))

theta <- 2 * acos(1 - (2 * (y / d)))
A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P

return(list(y = y, theta = theta, A = A, P = P, B = B, R = R))

} else if (missing(y) & missing(y_d) & missing(theta)) {

rh <- (n * Q) / (k * sqrt(Sf))

thetafun <- function (theta) ((theta - sin(theta)) * (d ^ 2 / 8)) * (((theta - sin(theta)) * (d ^ 2 / 8) / ((theta * d) / 2)) ^ (2 / 3)) - rh

thetause <- uniroot(thetafun, c(-1000, 1000), extendInt = "yes")
theta <- thetause$root

y <- (d / 2) * (1 - cos(theta / 2))

A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P

return(list(y = y, theta = theta, A = A, P = P, B = B, R = R))

} else if (missing(d)) {

if (missing(Q) & missing(Sf))

stop("Q and Sf are needed to compute d. Try again with a value for Q and Sf.")
# Source 1 / only process enough known variables and provide a stop warning if not enough

d <- 1.56 * ((n * Q) / (k * sqrt(Sf))) ^ (3 / 8)

theta <- 2 * acos(1 - (2 * (y / d)))
A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P

return(list(d = d, theta = theta, A = A, P = P, B = B, R = R))

} else if (missing(theta) & missing(y)) {

theta <- 2 * acos(1 - (2 * (y_d)))
y <- (d / 2) * (1 - cos(theta / 2))

A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P

return(list(theta = theta, y = y, A = A, P = P, B = B, R = R))

} else if (missing(theta)) {

theta <- 2 * acos(1 - (2 * (y / d)))

A <- (theta - sin(theta)) * (d ^ 2 / 8)
P <- ((theta * d) / 2)
B <- d * sin(theta / 2)
R <- A / P

return(list(theta = theta, A = A, P = P, B = B, R = R))
}
}
}
}
