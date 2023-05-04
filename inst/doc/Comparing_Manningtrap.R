## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  install.packages(c("install.load", "iemisc", "pander"))
#  # install the packages and their dependencies

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
# load the required package
install.load::load_package("iemisc", "pander")

## ---- echo = FALSE, out.width = '100%'----------------------------------------
linguisticsdown::include_graphics2("compare.png")

## ---- echo = FALSE, out.width = '100%'----------------------------------------
linguisticsdown::include_graphics2("trapezoid1.png")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

uuc1 <- Manningtrap_critical(y = 1.454, b = 4, m = 3, Sf = 0.02, n = 0.0550, units = "Eng", type = "symmetrical", critical = "accurate", output = "data.table")

pander(uuc1, missing = "")

## ---- echo = FALSE, out.width = '100%'----------------------------------------
linguisticsdown::include_graphics2("2019-05-22_08_24_13-ChannelAnalysis.png")

## ---- echo = FALSE, out.width = '100%'----------------------------------------
linguisticsdown::include_graphics2("trapezoid2.png")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

uuc2 <- Manningtrap_critical(Q = 745, b = 8, m1 = 2, m2 = 1.5, Sf = 0.01, n = 0.0150, units = "Eng", type = "non-symmetrical", critical = "accurate", output = "data.table")

pander(uuc2, missing = "")

