# iemisc 0.9.8

* CRAN request for `listless` package to be corrected or archived which would impact this package. The functions requiring `listless` have been re-written so that `listless` can be removed from Imports
* Request from Matt Dowle to have `data.table` as Imports rather than Depends
* Added English United States (en-us) as the language in the DESCRIPTION
* Removed `financial` package as Suggests as it was removed from CRAN
* Added quotes around package names (standard evaluation) in README.md and in all function examples


# iemisc 0.9.7

* CRAN fix of example from PgivenG


# iemisc 0.9.6

* Revised the vignette and changed the vignette filename and title
* Added `iemiscdata` and `import` as imported R packages (Issue #1 by jangorecki)
* Added `iemiscdata` and `import` as imports in functions as needed (Issue #1 by jangorecki)
* Added the concr_mix_normal_strength function [Concrete Mix Design for Normal Strength (Normal-weight) Concrete]
* Added `gsubfn` and `fpCompare` as imported R packages for the concr_mix_normal_strength function


# iemisc 0.9.5

* Added GNU Octave/MATLAB compatible trigonometric functions in degrees (`cosd`, `acosd`, `sind`, `asind`, `tand`, `atand`, `secd`, `asecd`, `cscd`, `acscd`, `cotd`, `acotd`, `atan2d`)


# iemisc 0.9.2

* Added `ie2misc`, `ie2miscdata` as suggested R packages


# iemisc 0.9.1

* Added more examples to the README.md
* Revised the examples in these functions: `Manningtri`, `Manningtrap`, `Manningrect`, `Manningpara`, `Manningcirc`
* Updated the Open Channel Flow problems vignette


# iemisc 0.9.0

* Updated these functions: `Manningtri`, `Manningtrap`, `Manningrect`, `Manningpara`, `Manningcirc`
* Added an Open Channel Flow problems vignette
* Added `install.load` as a suggested R package


# iemisc 0.5.2

* Updated these functions: `size`, `righttri`, `Manningtri`, `Manningtrap`, `Manningrect`, `Manningpara`, `Manningcirc`


# iemisc 0.5.1

* Renamed lengths to length_octave
* Updated these functions: `size`, `righttri`, `Manningtri`, `Manningtrap`, `Manningrect`, `Manningpara`, `Manningcirc`
* Added `iemiscdata` as a suggested R package


# iemisc 0.5.0

* Initial release
