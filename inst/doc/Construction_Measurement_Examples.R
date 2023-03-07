## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  install.packages(c("install.load", "iemisc", "units", "knitr"))
#  # install the packages and their dependencies

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
# load the required packages
install.load::load_package("iemisc", "units")
# load needed packages using the load_package function from the install.load package (it is assumed that you have already installed these packages)

import::from(fpCompare, "%==%")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

trxt <- "1 1/3"

frac_to_numeric(trxt)


tlrxy <- "4 1/8 inches"

frac_to_numeric(tlrxy)


tmrxy <- "12 13/16 inches"

frac_to_numeric(tmrxy)

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
t1 <- "34'-3 1/2\""
t2 <- "34-3 1/2\""
t3 <- "34' 3 1/2\""
t4 <- "34'-3 1/2"
t5 <- "34-3 1/2"
t6 <- "34 3 1/2"
t7 <- "34 ft 3 1/2 in"
t8 <- "34 3 1/2"
t9 <- "34 fts 3 1/2 in"
t10 <- "34 foot 3 1/2 in"
t11 <- "34 foot 3 1/2 inch"
t12 <- "34 foot 3 1/2 in"
t13 <- "34 feet 3 1/2 in"
t14 <- "34 feet 3 1/2 inch"
t15 <- "34 feet 3 1/2 in"
t16 <- "34 FEEt 3 1/2 IN"

# a)

(construction_decimal(t1, result = "traditional", output = "vector")
* construction_decimal(t2, result = "traditional", output = "vector")
* 4) / 43560 # acres

# b)

(construction_decimal(t1, result = "traditional", output = "vector")
^ 2 * 4) / 43560 # acres


# a) and b) can be expressed using the following as well:

t1_ft2 <- set_units((construction_decimal(t1, result = "traditional",
output = "vector") * construction_decimal(t2, result = "traditional",
output = "vector") * 4), US_survey_foot ^ 2)

t1_acres <- t1_ft2

units(t1_acres) <- make_units(acre); t1_acres


t1_ft2s <- set_units((construction_decimal(t1, result = "traditional",
output = "vector") ^ 2 * 4), US_survey_foot ^ 2)

t1_acress <- t1_ft2s

units(t1_acress) <- make_units(acre)
t1_acress


sum(construction_decimal(t1, result = "traditional", output = "vector"),
construction_decimal(t2, result = "traditional", output = "vector"),
construction_decimal(t3, result = "traditional", output = "vector"),
construction_decimal(t4, result = "traditional", output = "vector"),
construction_decimal(t5, result = "traditional", output = "vector"))


# traditional result for t1 - t16

construction_decimal(t1, result = "traditional", output = "vector")

construction_decimal(t2, result = "traditional", output = "vector")

construction_decimal(t3, result = "traditional", output = "vector")

construction_decimal(t4, result = "traditional", output = "vector")

construction_decimal(t5, result = "traditional", output = "vector")

construction_decimal(t6, result = "traditional", output = "vector")

construction_decimal(t7, result = "traditional", output = "vector")

construction_decimal(t8, result = "traditional", output = "vector")

construction_decimal(t9, result = "traditional", output = "vector")

construction_decimal(t10, result = "traditional", output = "vector")

construction_decimal(t11, result = "traditional", output = "vector")

construction_decimal(t12, result = "traditional", output = "vector")

construction_decimal(t13, result = "traditional", output = "vector")

construction_decimal(t14, result = "traditional", output = "vector")

construction_decimal(t15, result = "traditional", output = "vector")

construction_decimal(t16, result = "traditional", output = "vector")


# librecad result result for t1 - t16

construction_decimal(t1, result = "librecad", output = "vector")

construction_decimal(t2, result = "librecad", output = "vector")

construction_decimal(t3, result = "librecad", output = "vector")

construction_decimal(t4, result = "librecad", output = "vector")

construction_decimal(t5, result = "librecad", output = "vector")

construction_decimal(t6, result = "librecad", output = "vector")

construction_decimal(t7, result = "librecad", output = "vector")

construction_decimal(t8, result = "librecad", output = "vector")

construction_decimal(t9, result = "librecad", output = "vector")

construction_decimal(t10, result = "librecad", output = "vector")

construction_decimal(t11, result = "librecad", output = "vector")

construction_decimal(t12, result = "librecad", output = "vector")

construction_decimal(t13, result = "librecad", output = "vector")

construction_decimal(t14, result = "librecad", output = "vector")

construction_decimal(t15, result = "librecad", output = "vector")

construction_decimal(t16, result = "librecad", output = "vector")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

m1 <- "33'-3 1/2\""
m2 <- "32'-1"
m3 <- "32'-1"
m4 <- "32'-1"
m5 <- "32'-1"
m6 <- "33'-3 1/2\""

msum <- sum(construction_decimal(m1, result = "traditional",
output = "vector"), construction_decimal(m2, result = "traditional",
output = "vector"), construction_decimal(m3, result = "traditional",
output = "vector"), construction_decimal(m4, result = "traditional",
output = "vector"), construction_decimal(m5, result = "traditional",
output = "vector"), construction_decimal(m6, result = "traditional",
output = "vector"))
msum


construction_decimal("194'-11", result = "traditional", output =
"vector") %==% msum # check

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

# Calculate the square area

sum(construction_decimal("0 3", result = "traditional", output = "vector"), construction_decimal("0 8", result = "traditional", output = "vector"), construction_decimal("0 6", result = "traditional", output = "vector")) * sum(construction_decimal("0 2 5/8", result = "traditional", output = "vector"), 3 * construction_decimal("2 6 3/4", result = "traditional", output = "vector"), construction_decimal("0 2 5/8", result = "traditional", output = "vector"))

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

# Calculate the amount of surface area fill, in acres, for riprap
# placement and the volume of fill in yards ^ 3 along a streambank

bank <- set_units(construction_decimal("72 3 1/3", result =
"traditional", output = "vector"), "US_survey_foot")
# 72 feet 3 1/3 inches
bank
 
riprap <- set_units(construction_decimal("0 15.0", result = "traditional",
output = "vector"), "US_survey_foot")
riprap

riprap_yd <- riprap
units(riprap_yd) <- make_units(yd)
riprap_yd

OHWM_width <- set_units(25, "US_survey_foot")
OHWM_width

width <- set_units(47, "US_survey_foot")
width


bank_area1 <- width * bank
bank_area1

bank_area2 <- bank_area1
units(bank_area2) <- make_units(yd^2)
bank_area2

bank_area3 <- bank_area1
units(bank_area3) <- make_units(acres)
bank_area3

vol_bank <- riprap_yd * bank_area2
vol_bank


bank_area_OHWM1 <- OHWM_width * bank
bank_area_OHWM1

bank_area_OHWM2 <- bank_area1
units(bank_area_OHWM2) <- make_units(yd^2)
bank_area_OHWM2

bank_area_OHWM3 <- bank_area1
units(bank_area_OHWM3) <- make_units(acres)
bank_area_OHWM3

vol_bank_OHWM <- riprap_yd * bank_area_OHWM2
vol_bank_OHWM


fill_ft2 <- bank_area_OHWM1
fill_ft2

fill_acres <- bank_area_OHWM3
fill_acres

fill_yd2 <- bank_area_OHWM2
fill_yd2

fill_yd3 <- vol_bank_OHWM
fill_yd3

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

# Examples from the Spike Reference

psst <- "7' 4 5/16\""

pssts <- "0 3 3/8\""

wall1 <- "12' 7\""

wall2 <- "40' 9\""

construction_decimal(psst, result = "traditional", output = "vector")

construction_decimal(pssts, result = "traditional", output = "vector")

construction_decimal(wall1, result = "traditional", output = "vector")

construction_decimal(wall2, result = "traditional", output = "vector")

# Since pssts is a fraction representing 3 3/8 inches, it is better to
# use the frac_to_numeric function instead of the construction_decimal
# function after removing the 0 representing the feet measurement is removed.
# The following now matches the decimal value from the Spike Reference.

pssts1 <- "3 3/8\""

frac_to_numeric(pssts1)

# or more simply

pssts1b <- "3 3/8 in"

frac_to_numeric(pssts1b)

frac_to_numeric(pssts1) %==% frac_to_numeric(pssts1b)

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

librecad1 <- "1 2 7/16\""

construction_decimal(librecad1, result = "traditional", output = "vector")

knitr::kable(format(construction_decimal(librecad1, result = "traditional", output = "table"), digits = 6, nsmall = 0))

construction_decimal(librecad1, result = "librecad", output = "vector")

knitr::kable(format(construction_decimal(librecad1, result = "librecad", output = "table"), digits = 4, nsmall = 0))


librecad2 <- "6' 8 3/4 in"

construction_decimal(librecad2, result = "traditional", output = "vector")

knitr::kable(format(construction_decimal(librecad2, result = "traditional", output = "table"), digits = 6, nsmall = 6))

construction_decimal(librecad2, result = "librecad", output = "vector")

knitr::kable(format(construction_decimal(librecad2, result = "librecad", output = "table"), digits = 2, nsmall = 2))


librecad3 <- "6'-5 3/256\""

construction_decimal(librecad3, result = "traditional", output = "vector")

knitr::kable(format(construction_decimal(librecad3, result = "traditional", output = "table"), digits = 6, nsmall = 6))

construction_decimal(librecad3, result = "librecad", output = "vector")

knitr::kable(format(construction_decimal(librecad3, result = "librecad", output = "table"), digits = 5, nsmall = 5))

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

checker <- "6'-5 3/256 in"

checkers <- construction_decimal(checker, result = "traditional", output = "vector")

checkers

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 0)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 2)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 4)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 8)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 16)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 32)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 64)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 100)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 128)

construction_fraction(checkers, type = "traditional", result = "traditional", fraction = 256)

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

checkin <- 77.6875

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 0)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 2)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 4)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 8)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 16)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 32)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 64)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 100)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 128)

construction_fraction(checkin, type = "librecad", result = "traditional", fraction = 256)

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

librecad1a <- "6' 8 3/4 in"

construction_decimal_eng(librecad1a)


librecad2a <- "6'-5 3/256\""

construction_decimal_eng(librecad2a)

## ---- echo = FALSE, out.width = '100%'----------------------------------------
linguisticsdown::include_graphics2("https://i.creativecommons.org/l/by-sa/4.0/88x31.png")

