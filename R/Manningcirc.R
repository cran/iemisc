#' Circular cross-section using the Gauckler-Manning-Strickler equation
#'
#' @description
#' Manningcirc and Manningcircy solve for a missing variable for a circular
#' cross-section. The \code{\link[stats]{uniroot}} function is used to obtain the
#' missing parameters.
#'
#' The Manningcirc function solves for one missing variable in the Gauckler-
#' Manning equation for a circular cross-section and uniform flow. The
#' possible inputs are Q, n, Sf, y, and d. If y or d are not initially known,
#' then Manningcircy can solve for y or d to use as input in the Manningcirc
#' function.
#'
#'
#'
#'
#' @details
#' Gauckler-Manning-Strickler equation is expressed as
#'
#' \deqn{V = \frac{K_n}{n}R^\frac{2}{3}\sqrt{S}}
#'
#' \describe{
#'   \item{\emph{V}}{the velocity (m/s or ft/s)}
#'   \item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'   \item{\emph{K_n}}{the conversion constant -- 1.0 for SI and
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
#'   \item{\emph{Q}}{the discharge {m^3/s or ft^3/s (cfs)} is VA}
#'   \item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'   \item{\emph{P}}{the wetted perimeters of the channel (m or ft)}
#'   \item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'   \item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'   \item{\emph{K_n}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#'
#'
#'
#' Other important equations regarding the circular cross-section follow:
#'
#' \deqn{R = \frac{A}{P}}
#'
#' \describe{
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'   \item{\emph{P}}{the wetted perimeters of the channel (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{A = \left(\\theta - \sin \\theta\right) \frac{d^2}{8}}
#'
#' \describe{
#'   \item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'   \item{\emph{d}}{the diameters of the cross-section (m or ft)}
#'   \item{\emph{\\theta}}{see the equation defining this parameters}
#' }
#'
#'
#' \deqn{\\theta = 2 \arcsin\left[1 - 2\left(\frac{y}{d}\right)\right]}
#' 
#' \describe{
#'   \item{\emph{\\theta}}{see the equation defining this parameters}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'   \item{\emph{d}}{the diameters of the cross-section (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{d = 1.56 \left[\frac{nQ}{K_n\sqrt{S}}\right]^\frac{3}{8}}
#' 
#' \describe{
#'   \item{\emph{d}}{the initial diameters of the cross-section [m or ft]}
#'   \item{\emph{Q}}{the discharge {m^3/s or ft^3/s (cfs)} is VA}
#'   \item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'   \item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'   \item{\emph{K_n}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#' Note: This will only provide the initial conduit diameters, check the design
#'       considerations to determine your next steps.
#'
#'
#'
#'
#' \deqn{P = \frac{\\theta d}{2}}
#'
#' \describe{
#'   \item{\emph{P}}{the wetted perimeters of the channel (m or ft)}
#'   \item{\emph{\\theta}}{see the equation defining this parameters}
#'   \item{\emph{d}}{the diameters of the cross-section (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{B = d \sin\left(\frac{\\theta}{2}\right)}
#'
#' \describe{
#'   \item{\emph{B}}{the top width of the channel (m or ft)}
#'   \item{\emph{\\theta}}{see the equation defining this parameters}
#'   \item{\emph{d}}{the diameters of the cross-section (m or ft)}
#' }
#'
#'
#'
#' \deqn{D = \frac{A}{B}}
#'
#' \describe{
#'   \item{\emph{D}}{the hydraulic depth (m or ft)}
#'   \item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'   \item{\emph{B}}{the top width of the channel (m or ft)}
#' }
#'
#'
#'
#' \deqn{Z = \frac{\sqrt{2}}{2}my^2.5}
#'
#' \describe{
#'   \item{\emph{Z}}{the Section factor (m or ft)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'   \item{\emph{m}}{the horizontal side slope}
#' }
#'
#'
#'
#' \deqn{E = y + \frac{Q^2}{2gA^2}}
#'
#' \describe{
#'   \item{\emph{E}}{the Specific Energy (m or ft)}
#'   \item{\emph{Q}}{the discharge {m^3/s or ft^3/s (cfs)} is VA}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#' }
#'
#'
#'
#'
#' \deqn{VH = \frac{V^2}{2g}}
#'
#' \describe{
#'   \item{\emph{VH}}{the Velocity Head (m or ft)}
#'   \item{\emph{V}}{the velocity (m/s or ft/s)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#' }
#'
#'
#'
#' A rough turbulent zone check is performed on the water flowing in the
#' channel using the Reynolds number (Re). The Re equation follows:
#'
#' \deqn{Re = \frac{\\rho RV}{\\mu}}
#'
#' \describe{
#'   \item{\emph{Re}}{Reynolds number (dimensionless)}
#'   \item{\emph{\\rho}}{density (kg/m^3 or slug/ft^3)}
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{V}}{the velocity (m/s or ft/s)}
#'   \item{\emph{\\mu}}{dynamic viscosity (* 10^-3 kg/m*s or * 10^-5 lb*s/ft^2)}
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
#'   \item{\emph{Fr}}{the Froude number (dimensionless)}
#'   \item{\emph{V}}{the velocity (m/s or ft/s)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{D}}{the hydraulic depth (m or ft)}
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
#' @param Q numeric vector that contains the discharge value {m^3/s or ft^3/s},
#'   if known.
#' @param n numeric vector that contains the Manning's roughness coefficient n,
#'   if known.
#' @param d numeric vector that contains the diameters value (m or ft),
#'   if known.
#' @param Sf numeric vector that contains the bed slope (m/m or ft/ft),
#'   if known.
#' @param y numeric vector that contains the flow depth (m or ft), if known.
#' @param T numeric vector that contains the temperature (degrees C or degrees
#'   Fahrenheit), if known.
#' @param units character vector that contains the system of units {options are
#'   \code{SI} for International System of Units or \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)}
#'
#' @return the missing parameters (Q, n, or Sf) & theta, area (A), wetted
#'   perimeters (P), velocity (V), top width (B), hydraulic depth (D), hydraulic radius (R), E (Specific Energy), Vel_Head (Velocity Head), Z (Section Factor), Reynolds number (Re), and Froude number (Fr) as a \code{\link[base]{list}}. for the
#'   Manningcirc function.
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 2, 8, 36, 102, 120, 123-125, 153-154.
#'    \item Dan Moore, P.E., NRCS Water Quality and Quantity Technology Development Team, Portland Oregon, "Using Mannings Equation with Natural Streams", August 2011, \url{https://web.archive.org/web/20210416091858/https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/xsec/manningsNaturally.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item Gilberto E. Urroz, Utah State University Civil and Environmental Engineering - OCW, CEE6510 - Numerical Methods in Civil Engineering, Spring 2006 (2006). Course 3. "Solving selected equations and systems of equations in hydraulics using Matlab", August/September 2004, \url{https://digitalcommons.usu.edu/ocw_cee/3/}.
#'    \item Tyler G. Hicks, P.E., \emph{Civil Engineering Formulas: Pocket Guide}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2002, page 423, 425.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 26 November 2015, "Manning formula", \url{https://en.wikipedia.org/wiki/Manning_formula}.
#'    \item John C. Crittenden, R. Rhodes Trussell, David W. Hand, Kerry J. Howe, George Tchobanoglous, \emph{MWH's Water Treatment: Principles and Design}, Third Edition, Hoboken, New Jersey: John Wiley & Sons, Inc., 2012, page 1861-1862.
#'    \item Andrew Chadwick, John Morfett and Martin Borthwick, \emph{Hydraulics in Civil and Environmental Engineering}, Fourth Edition, New York City, New York: Spon Press, Inc., 2004, page 133.
#'    \item Robert L. Mott and Joseph A. Untener, \emph{Applied Fluid Mechanics}, Seventh Edition, New York City, New York: Pearson, 2015, page 376, 377-378, 392.
#'    \item Ven Te Chow, Ph.D., \emph{Open-Channel Hydraulics}, McGraw-Hill Classic Textbook Reissue, New York City, New York: McGraw-Hill Book Company, 1988, pages 21, 40-41.
#'    \item Gary P. Merkley, "BIE6300 - Irrigation & Conveyance Control Systems, Spring 2004", 2004, Biological and Irrigation Engineering - OCW. Course 2, \url{https://digitalcommons.usu.edu/ocw_bie/2/}.
#'    \item The NIST Reference on Constants, Units, and Uncertainty, Fundamental Constants Data Center of the NIST Physical Measurement Laboratory, "standard acceleration of gravity g_n", \url{https://physics.nist.gov/cgi-bin/cuu/Value?gn}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 15 May 2019, "Conversion of units", \url{https://en.wikipedia.org/wiki/Conversion_of_units}.
#' }
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
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
#' 
#' # Practice Problem 14.12 from Mott (page 392)
#'
#' install.load::load_package("iemisc", "iemiscdata")
#'
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
#'
#' install.load::load_package("iemisc", "iemiscdata")
#'
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
#'
#' install.load::load_package("iemisc", "iemiscdata")
#'
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
#'
#' install.load::load_package("iemisc", "iemiscdata")
#'
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
#'
#' install.load::load_package("iemisc", "units")
#'
#'
#' # create a numeric vector with the units of feet
#' yeng <- set_units(y$y, ft)
#' 
#' 
#' # create a numeric vector to convert from feet to meters
#' ysi <- yeng
#' 
#' 
#' # create a numeric vector with the units of meters
#' units(ysi) <- make_units(m)
#' 
#' 
#' # create a numeric vector with the units of feet
#' deng <- set_units(2, ft)
#' 
#' 
#' # create a numeric vector to convert from feet to meters
#' dsi <- deng
#' 
#' 
#' # create a numeric vector with the units of meters
#' units(dsi) <- make_units(m)
#'
#' Manningcirc(Sf = 0.022, n = 0.023, y = drop_units(ysi), d = drop_units(dsi), units = "SI")
#' # Sf = 0.022 m/m, n = 0.023, y = 0.48768 m, d = 0.6096 m, units = SI units
#' # This will solve for Q since it is missing and Q will be in m^3/s
#'
#'
#'



#' @title Circular Cross-section Using the Gauckler-Manning-Strickler Equation 1
#' @name Manningcirc
#'
#'
#' @seealso \code{\link{Manningcircy}}
#'
#'
#' @importFrom data.table data.table setnames setattr
#' @importFrom fpCompare %==%
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom units set_units make_units drop_units
#' @importFrom round round_r3
#' @importFrom stats uniroot
#' @export
Manningcirc <- function (Q = NULL, n = NULL, Sf = NULL, y = NULL, d = NULL, T = NULL, units = c("SI", "Eng")) {

K <- NULL
# due to NSE notes in R CMD check

checks <- c(Q, n, Sf, y, d)

units <- units



# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either Q, n, Sf, y, or d is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(units, "S==1"), msg = "There is not an unit type or more than 1 unit type. Please specify either 'SI' or 'Eng'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The unit system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not


# units
if (units == "SI") {

# use the temperature to determine the density & absolute and kinematic viscosities
T <- ifelse(is.null(T), 20, T) # degrees C

assert_that(qtest(T, "N+(0,)"), msg = "Either T is equal to or less than 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 0 C / 32 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(T, "degree_C")


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


# create viscosities based on temperatures
# saturated liquid density at given temperature in degrees Celsius (SI units)
rho_SI <- density_water(T, "SI")

# absolute or dynamic viscosity at given temperature in degrees Celsius and density of rho (SI units)
mu_SI <- dyn_visc_water(T, "SI")

# kinematic viscosity at given temperature in degrees Celsius and density of rho (SI units)
nu_SI <- kin_visc_water(rho_SI, mu_SI, rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s")


k <- 1

g <- 9.80665 # m / s^2

rho <- rho_SI

nu <- nu_SI

mu <- mu_SI

# unit weight of water at given temperature in degrees Celsius and density of rho (SI units)
gamma <- unit_wt(rho = rho, units = "SI")


density_water_units <- "kg/m^3"

dyn_visc_water_units <- "Pa * s or kg/m*s"

kin_visc_water_units <- "m^2/s"


result_units <- c("m", "m^2", "m", "m", "m", "m", "m", "m/s", "m^3/s", "dimensionless", "m/m", "degrees Celsius", "Kelvin", density_water_units, dyn_visc_water_units, kin_visc_water_units, "dimensionless", "dimensionless", "m/m", "m/m", "m/m", "m", "m", "m", "m", "m^3/s", "m", "m", "pascal (N/m^2)", "pascal (N/m^2)")


} else if (units == "Eng") {

# use the temperature to determine the density & absolute and kinematic viscosities
T <- ifelse(is.null(T), 68, T) # degrees F

assert_that(qtest(T, "N+(32,)"), msg = "Either T is equal to or less than 32 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_F <- T

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to Kelvin
T_K <- T_F


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


# create viscosities based on temperatures
# saturated liquid density at given temperature in degrees Fahrenheit (US Customary units)
rho_Eng <- density_water(T, "Eng", Eng_units = "slug/ft^3")


# absolute or dynamic viscosity at given temperature in degrees Fahrenheit and density of rho (US Customary units)
mu_Eng <- dyn_visc_water(T, "Eng", Eng_units = "slug/ft/s")


# kinematic viscosity at given temperature in degrees Fahrenheit and density of rho (US Customary units)
nu_Eng <- kin_visc_water(rho_Eng, mu_Eng, rho_units = "slug/ft^3", mu_units = "slug/ft/s")



k <- 3.2808399 ^ (1 / 3)

g <- 9.80665 * (3937 / 1200) # ft / sec^2

gc <- 9.80665 * (3937 / 1200) # lbm-ft/lbf-sec^2

rho <- rho_Eng

nu <- nu_Eng

mu <- mu_Eng

# unit weight of water at given temperature in degrees Fahrenheit and density of rho (US Customary units)
gamma <- unit_wt(rho = rho, units = "Eng", Eng_units = "slug/ft^3")


density_water_units <- "slug/ft^3"

dyn_visc_water_units <- "slug/ft*s"

kin_visc_water_units <- "ft^2/s"


result_units <- c("ft", "ft^2", "ft", "ft", "ft", "ft", "ft", "ft/sec (fps)", "ft^3/sec (cfs)", "dimensionless", "ft/ft", "degrees Fahrenheit", "Kelvin", density_water_units, dyn_visc_water_units, kin_visc_water_units, "dimensionless", "dimensionless", "ft/ft", "ft/ft", "ft/ft", "ft", "ft", "ft", "ft", "ft^3/sec (cfs)", "ft", "ft", "lb/ft^2", "lb/ft^2")

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

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

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

cat("
Flow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation
 is acceptable to use.

")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

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

cat("
Flow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation
 is acceptable to use.

")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}

return(list(Sf = Sf, V = V, A = A, P = P, R = R, Re = Re, Fr = Fr))
}
}








#' @title Circular Cross-section Using the Gauckler-Manning-Strickler Equation 2
#' @name Manningcircy
#'
#' @description
#' The Manningcircy function solves for one missing variable in the Gauckler-
#' Manning equation for a circular cross-section and uniform flow. The possible
#' inputs are y, d, y_d (ratio of y/d), and theta.
#'
#' @param Q numeric vector that contains the discharge value {m^3/s or ft^3/s},
#'   if known.
#' @param d numeric vector that contains the diameters value (m or ft),
#'   if known.
#' @param Sf numeric vector that contains the bed slope (m/m or ft/ft),
#'   if known.
#' @param y numeric vector that contains the flow depth (m or ft), if known.
#' @param y_d numeric vector that contains the filling ration (y/d), if known.
#' @param theta numeric vector that contains the angle theta (radians), if
#'        known.
#' @param units character vector that contains the system of units {options are
#'   \code{SI} for International System of Units or \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)}
#'
#'
#'
#' @return the missing parameters (d or y) & theta, area (A), wetted
#'   perimeters (P), top width (B), velocity (V), hydraulic depth (D), hydraulic radius (R), E (Specific Energy), Vel_Head (Velocity Head), Z (Section Factor), Reynolds number (Re), and Froude number (Fr) as a \code{\link[base]{list}}. for the Manningcircy function.
#'
#' @seealso \code{\link{Manningcirc}} for the examples section
#'
#'
#' @importFrom data.table data.table setnames setattr
#' @importFrom fpCompare %==%
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom units set_units make_units drop_units
#' @importFrom round round_r3
#' @importFrom stats uniroot
#' @export
Manningcircy <- function (y = NULL, d = NULL, y_d = NULL, theta = NULL, Sf = NULL, Q = NULL, units = c("SI", "Eng")) {

checks <- c(y, d, y_d, theta, Sf, Q)

units <- units



# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either y, d, y_d, theta, Sf, or Q is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(units, "S==1"), msg = "There is not an unit type or more than 1 unit type. Please specify either 'SI' or 'Eng'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The unit system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not


if (units == "SI") {

   k <- 1

} else if (units == "Eng") {

   k <- 3.2808 ^ (1 / 3)

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
# only process with enough known variables and provide an error message if the check fails

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
