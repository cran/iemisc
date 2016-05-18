# iemisc

R package that contains Irucka Embry's miscellaneous functions: statistical analysis [RMS, coefficient of variation (CV), approximate and relative error, range, harmonic mean, geometric mean], engineering economics (benefit-cost, future value, present value, annual value, gradients, interest, periods, etc.), geometry (sphere volume and right triangle), environmental/water resources engineering (Manning's n, Gauckler-Manning-Strickler equation), a
version of linear interpolation for use with NAs, & GNU Octave/MATLAB compatible size, numel, and length functions.


# Installation

```R
install.packages("iemisc")
```


# Examples

```R
library(iemisc)
require(stats)

set.seed(200) # makes the example reproducible

samp <- rnorm(200) # sample


# Calculate the sample harmonic mean (SHM) of the 200 values
# Using the default value of na.rm = FALSE
# using a matrix of the numeric vector obs1
samp1 <- matrix(data = samp, nrow = length(samp), ncol = 1, byrow = FALSE,
dimnames = list(c(rep("", length(samp))), "Sample"))

shm(samp1)



# Compute the relative error of the 210 values
set.seed(210) # makes the example reproducible
true <- rnorm(210) # true
approx <- rnorm(210) # approximation

relerror(true, approx)
```


# Disclaimer

This software is provided "AS IS." See the GPL License for more information.


# License

iemisc is distributed under the GPL-3 (or later) license, as stated in the DESCRIPTION file. For more info, see the [GNU General Public License (GPL) page](https://gnu.org/licenses/gpl.html).