% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Manningcirc.R
\encoding{UTF-8}
\name{Manningcircy}
\alias{Manningcircy}
\title{Circular Cross-section Using the Gauckler-Manning-Strickler Equation 2}
\usage{
Manningcircy(
  y = NULL,
  d = NULL,
  y_d = NULL,
  theta = NULL,
  Sf = NULL,
  Q = NULL,
  units = c("SI", "Eng")
)
}
\arguments{
\item{y}{numeric vector that contains the flow depth (m or ft), if known.}

\item{d}{numeric vector that contains the diameters value (m or ft),
if known.}

\item{y_d}{numeric vector that contains the filling ration (y/d), if known.}

\item{theta}{numeric vector that contains the angle theta (radians), if
known.}

\item{Sf}{numeric vector that contains the bed slope (m/m or ft/ft),
if known.}

\item{Q}{numeric vector that contains the discharge value (m^3/s or ft^3/s),
if known.}

\item{units}{character vector that contains the system of units [options are
\code{SI} for International System of Units or \code{Eng} for English units
(United States Customary System in the United States and Imperial Units in
the United Kingdom)]}
}
\value{
the missing parameters (d or y) & theta, area (A), wetted
  perimeters (P), top width (B), velocity (V), hydraulic depth (D), hydraulic radius (R), E (Specific Energy), Vel_Head (Velocity Head), Z (Section Factor), Reynolds number (Re), and Froude number (Fr) as a \code{\link[base]{list}}. for the Manningcircy function.
}
\description{
The Manningcircy function solves for one missing variable in the Gauckler-
Manning equation for a circular cross-section and uniform flow. The possible
inputs are y, d, y_d (ratio of y/d), and theta.
}
\seealso{
\code{\link{Manningcirc}} for the examples section
}
\author{
Irucka Embry
}
