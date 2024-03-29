% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygon_area.R
\encoding{UTF-8}
\name{polygon_area}
\alias{polygon_area}
\title{Polygon Area (using the Shoelace Formula)}
\usage{
polygon_area(x, y, plot = c(0, 1), fill = NULL, color = NULL)
}
\arguments{
\item{x}{numeric vector that contains the x coordinates of the vertices.
Regardless of rather the user starts in a clockwise or a counter-clockwise
direction, the result will be positive.}

\item{y}{numeric vector that contains the y coordinates of the vertices.
Regardless of rather the user starts in a clockwise or a counter-clockwise
direction, the result will be positive.}

\item{plot}{integer vector that contains 0, 1 only. 0 represents
do not plot the polygon and 1 is for plotting the polygon.}

\item{fill}{character vector that contains the inside color of the polygon.
The possible colors are those that are accepted by ggplot2. The default
fill color is black.}

\item{color}{character vector that the border color of the polygon. The
possible colors are those that are accepted by ggplot2. The default fill
color is black.}
}
\value{
the area of the polygon as a positive numeric vector.
}
\description{
Calculate the area of a polygon using the shoelace formula.
}
\examples{

# Example 1 from Source 2

library(iemisc)

x <- c(4,  4,  8,  8, -4, -4)
y <- c(6, -4, -4, -8, -8, 6)

polygon_area(x, y, plot = 1)

# compare with pracma's and geometry's polyarea

pracma::polyarea(x, y)

geometry::polyarea(x, y)





# Example 2

library(iemisc)

type38 <- construction_decimal("46'-10 1/2\"", result = "traditional", output = "vector")

x38 <- c(0, 25, sum(25, type38, 10), sum(25, type38, 10, 25))

y38 <- c(0, rep((3 + 1 / 3), 2), 0)

polygon_area(x38, y38, plot = 1, fill = "darkseagreen3", color = "aquamarine4")

# compare with pracma's and geometry's polyarea

pracma::polyarea(x38, y38)

geometry::polyarea(x38, y38)





# Example 3

install.load::load_package("iemisc", "data.table")

coords <- fread("
X,   Y
0,	0
34,	4
58,	4
84,	6.7
184,	0", header = TRUE)

polygon_area(coords$X, coords$Y, plot = 1, color = "#00abff", fill = NA)
# "Use NA for a completely transparent colour." (from ggplot2 color function)

# compare with pracma's and geometry's polyarea

pracma::polyarea(coords$X, coords$Y)

geometry::polyarea(coords$X, coords$Y)





# Example 4 from pracma

library(iemisc)

Xx <- c(0, 4, 4, 0)

Yy <- c(0, 0, 4, 4)

polygon_area(Xx, Yy, 1, color = "goldenrod1", fill = "#00abff")

# compare with pracma's and geometry's polyarea

pracma::polyarea(Xx, Yy)

geometry::polyarea(Xx, Yy)





# Example 5 from pracma

library(iemisc)

Xx1 <- c(0, 4, 2)

Yy1 <- c(0, 0, 4)

polygon_area(Xx1, Yy1, 1, color = "rosybrown", fill = "papayawhip")

# compare with pracma's and geometry's polyarea

pracma::polyarea(Xx1, Yy1)

geometry::polyarea(Xx1, Yy1)




}
\references{
John D Page, From Math Open Reference: Algorithm to find the area of a polygon. See \url{https://web.archive.org/web/20221006001150/https://www.mathopenref.com/coordpolygonarea2.html}. Provided by Internet Archive: Wayback Machine to avoid the connection timeout.
}
\seealso{
\code{\link[pracma]{polyarea}} and \code{\link[geometry]{polyarea}}
}
\author{
John D Page for the JavaScript code, Irucka Embry (R code)
}
