# iemisc 1.0.0 (10 February 2023)

* created the following new functions: "%//%", "%qsin%", CompIntCharg, Manningtrap_critical, Mod_octave, Re1, Re2, Re3, Re4, Rem, SimpIntCharg, air_stripper, colebrook, concr_mix_lightweight_strength, concr_mix_normal_strength, construction_decimal, construction_decimal_eng, construction_fraction, density_water, dyn_visc_water, engr_survey, engr_survey2, engr_survey3, engr_survey4, engr_survey_batch, engr_survey_reverse, f1, f2, f3, f4, f5, f6, f7, f8, frac_to_numeric, fractdiff, igivenICPn, iscolumn, isrow, kin_visc_water, lat_long2state, lat_long2utm, maxmre, mortality_rate, mortality_rate_pct, mre, ndims, polygon_area, project_midpoint, prop_mortality_ratio, prop_solver, reduce_single_digit, sat_vapor_pressure, sat_vapor_pressure_ice, sec, secprop, sp_gravity, sp_volume, splitcomma, splitremove, surf_tens_water, unit_wt, weighted_C, and  weighted_CN
* These were the previous functions available as of `iemisc` 0.9.9: AF, AP, AgivenF, AgivenFcont, AgivenG, AgivenP, AgivenPcont, CompIntPaid, EffInt, FA, FP, FgivenA, FgivenAcont, FgivenP, FgivenPcont, Manningcirc, Manningcircy, Manningpara, Manningrect, Manningtrap, Manningtri, PA, PF, PgivenA, PgivenA1, PgivenAcont, PgivenF, PgivenFcont, PgivenFivary, PgivenG, SimpIntPaid, acosd, acotd, acscd, approxerror, asecd, asind, atan2d, atand, benefitcost, cosd, cotd, cscd, cv, igivenPFn, length_octave, n, na.interp1, nc1, nc2, nc3, nc4, ngivenPFi, numel, ranges, relerror, righttri, rms, secd, sgm, shm, sind, size, tand, and volsphere
* Revised the CITATION file
* Removed `Rcpp` (>= 0.11.5),`ie2misc`, `NISTunits`, and `optiRum` from Suggests & `testit` and `rgdal` (being retired in 2023) from Imports
* Added the following packages to Imports: `anytime`, `assertthat`, `berryFunctions`, `checkmate`, `foreach`, `geosphere`, `ggplot2`, `ggpubr`, `lubridate`, `matlab`, `matlab2r`, `measurements`, `methods`, `mgsub`, `qdapRegex`, `qdapTools`, `ramify`, `rivr`, `roperators`, `round`, `sf`, `signal`, `sjmisc`, `units `, `USA.state.boundaries`, and `utils`
* Added `assertthat` and `checkmate` to Imports to simplify the internal testing of the package functions (previously if-else statements were used). Made revisions to all of the functions to use the newly imported packages.
* Added the following packages to Suggests: `aiRthermo`, `callr`, `flextable`, `fractional`, `fracture`, `geometry`, `hydraulics`, `import`, `linguisticsdown`, `maps`, `MASS`, `rando`, `sampler`, `spelling`, and `tinytest`
* Added `rando` to Suggests. Replaced set.seed and rnorm from `stats` with set_n and r_norm from `rando` in the examples.
* Created passing tests for many of the functions using `tinytest`
* Revised existing vignettes and created new vignettes. Added more examples and/or vignettes for the pre-existing functions. Many of the examples and/or the functions themselves for the previous functions were revised too.
* Used the Internet Archive Wayback Machine to retrieve lost URLs, as needed
* Changed most import statements to importFrom statements after an issue was filed about `data.table` and `zoo` conflicts (Issue #3 by Toby Dylan Hocking)
* Now using the try function for examples that may possibly fail (source references are in the function source declarations)
* Replaced round with round_r3 throughout the functions
* Added the f1, f2, f3, f4, f5, f6, f7, f8, and colebrook functions to deal with the Darcy friction factor (f)
* Added the Re1, Re2, Re3, and Re4 functions to calculate the Reynolds number
* Added the weighted_C and weighted_CN functions for the weighted C factor and the weighted curve number, respectively
* Added the Manningtrap_critical function to calculate the critical values for a trapezoidal open channel cross-section.
* Added the sat_vapor_pressure, sat_vapor_pressure_ice, density_water, dyn_visc_water, kin_visc_water, sp_gravity, sp_volume, unit_wt, and surf_tens_water functions to replace the former dependency on the archived `IAPWS95` package and for use in the open channel flow functions (Manning...)
* Added the engr_survey, engr_survey2, engr_survey3, engr_survey4, engr_survey_batch, engr_survey_reverse, lat_long2state, project_midpoint, and lat_long2utm functions for dealing with engineering survey measurements mostly in Kentucky and Tennessee, but also globally for other functions
* Added the air_stripper function to size air strippers for removing volatile organic compounds from liquids
* Added the concr_mix_lightweight_strength [Concrete Mix Design for Structural Lightweight Concrete] and concr_mix_normal_strength [Concrete Mix Design for Normal Strength Concrete] functions. The trial batch volume can be specified for the concr_mix_normal_strength function. For the use of Table 3.3 (determining the water-cement ratio by weight) in the concr_mix_normal_strength function, `pracma`s interp1 is now used for the cases where the compressive strength value is not present in the Table (see the Reference source for the Table).
* Added secprop function to calculate and plot section properties. It was written for compatibility with a GNU Octave/MATLAB function.
* Added polygon_area function to calculate the area of a polygon using the Shoelace formula and to plot the polygon
* Added SimpIntCharg and CompIntCharg functions to calculate the simple interest and the compound interest charged only
* Added the igivenICPn function to calculate the simple interest rate given interest charged, number of years, and principal value
* Changed the output table format for benefitcost function from a `data.frame` to a `data.table`
* Added the construction_decimal, construction_fraction, construction_decimal_eng, and frac_to_numeric functions to work with construction measurements. Also, frac_to_numeric can work with plain fractions.
* Added ndims, isrow, iscolumn, sec, fractdiff, Mod_octave, and Rem functions & modified the size function for GNU Octave/MATLAB compatibility
* Added floor division ("%//%") function for Python compatibility
* Added "%qsin%" function to perform a quick search
* Added the splitcomma and splitremove functions to transform Second String, First String to First String Second String and remove certain characters from a string, respectively
* Added maxmre and mre functions to calculate the maximum mean relative error (MAXRE) and mean relative error (MRE), respectively
* Added mortality_rate, mortality_rate_pct, and prop_mortality_ratio functions to determine mortality rates
* Added prop_solver function to solve for the missing value in a proportion
* Added reduce_single_digit function to sum all digits of a number to a single digit (useful for Numerology)
* Added various people as authors/contributors for their respective codes in both the DESCRIPTION file and in the functions themselves


# iemisc 0.9.9

* CRAN request for `IAPWS95` package to be corrected or archived which would impact this package. The functions requiring `IAPWS95` have been re-written so that `IAPWS95` can be removed from Imports


# iemisc 0.9.8

* CRAN request for `listless` package to be corrected or archived which would impact this package. The functions requiring `listless` have been re-written so that `listless` can be removed from Imports
* Request from Matt Dowle to have `data.table` as Imports rather than Depends
* Added English United States (en-us) as the language in the DESCRIPTION file
* Removed the `financial` package as Suggests as it was removed from CRAN
* Added quotes around package names (standard evaluation) in README.md and in all function examples


# iemisc 0.9.7

* CRAN fix of example from PgivenG


# iemisc 0.9.6

* Revised the vignette and changed the vignette filename and title
* Added `iemiscdata` and `import` as imported R packages (Issue #1 by jangorecki)
* Added `iemiscdata` and `import` as imports in functions as needed (Issue #1 by jangorecki)
* Added the concr_mix_normal_strength function [Concrete Mix Design for Normal Strength (Normal-weight) Concrete] -- Although that was the plan for version 0.9.6, this did not happen until version 1.0.0
* Added `gsubfn` and `fpCompare` as imported R packages for the concr_mix_normal_strength function -- Although that was the plan for version 0.9.6, this did not happen until version 1.0.0


# iemisc 0.9.5

* Added GNU Octave/MATLAB compatible trigonometric functions in degrees (cosd, acosd, sind, asind, tand, atand, secd, asecd, cscd, acscd, cotd, acotd, atan2d)


# iemisc 0.9.2

* Added `ie2misc`, `ie2miscdata` as suggested R packages


# iemisc 0.9.1

* Added more examples to the README.md
* Revised the examples in these functions: Manningtri, Manningtrap, Manningrect, Manningpara, Manningcirc
* Updated the Open Channel Flow problems vignette


# iemisc 0.9.0

* Updated these functions: Manningtri, Manningtrap, Manningrect, Manningpara, Manningcirc
* Added an Open Channel Flow problems vignette
* Added `install.load` as a suggested R package


# iemisc 0.5.2

* Updated these functions: size, righttri, Manningtri, Manningtrap, Manningrect, Manningpara, Manningcirc


# iemisc 0.5.1

* Renamed lengths to length_octave
* Updated these functions: size, righttri, Manningtri, Manningtrap, Manningrect, Manningpara, Manningcirc
* Added `iemiscdata` as a suggested R package


# iemisc 0.5.0

* Initial release
