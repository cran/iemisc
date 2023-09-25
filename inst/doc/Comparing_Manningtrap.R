## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  
#  install.packages(c("install.load", "iemisc", "pander", "data.table"))
#  # install the packages and their dependencies

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

# load the required package
install.load::load_package("iemisc", "pander", "data.table")

## ---- echo = FALSE, out.width = '100%'----------------------------------------

linguisticsdown::include_graphics2("compare.png")


## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

fhwa_ex1 <- data.table(Parameter = c("Flow", "Depth", "Area of Flow", "Wetted Perimeter", "Hydraulic Radius", "Average Velocity", "Top Width (T)", "Side slope 1 (Z1)", "Side slope 2 (Z2)", "Channel width (B)", "Longitudinal slope", "Manning's roughness", "Froude Number", "Critical Depth", "Critical Velocity", "Critical Slope", "Critical Top Width", "Max Shear Stress", "Avg. Shear Stress"), Value = c(44.001, 1.454, 12.161, 13.197, 0.921, 3.618, 12.725, 3.0, 3.0, 4.0, 0.02, 0.0550, 0.652, 1.163, 5.052, 0.04979, 10.978, 1.815, 1.150), Unit = c("cfs", "ft", "sq ft", "ft", "ft", "fps", "ft", "", "", "ft", "ft/ft", "", "",  "ft", "fps", "ft/ft", "ft", "lb/ft^2", "lb/ft^2"))


pander(fhwa_ex1, missing = "")

## ---- echo = FALSE, out.width = '100%'----------------------------------------

linguisticsdown::include_graphics2("trapezoid1.png")


## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

fang_ex1 <- data.table(Parameter = c("Flow", "Depth", "Area of Flow", "Wetted Perimeter", "Hydraulic Radius", "Average Velocity", "Top Width (T)", "Side slope 1 (Z1)", "Side slope 2 (Z2)", "Channel width (B)", "Longitudinal slope", "Manning's roughness", "Froude Number", "Critical Depth", "Critical Velocity", "Critical Slope", "Critical Top Width", "Max Shear Stress", "Avg. Shear Stress"), Value = c(43.9882, 1.454, 12.16, 13.2, "", 3.6179, 12.72, 3.0, 3.0, 4.0, 0.02, 0.0550, 0.65, 1.17, "", 0.0492, "", "", ""), Unit = c("cfs", "ft", "sq ft", "ft", "ft", "fps", "ft", "", "", "ft", "ft/ft", "", "", "ft", "fps", "ft/ft", "ft", "lb/ft^2", "lb/ft^2"))


pander(fang_ex1, missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

uuc1 <- Manningtrap_critical(y = 1.454, b = 4, m = 3, Sf = 0.02, n = 0.0550, units = "Eng", type = "symmetrical", critical = "accurate", output = "data.table")

pander(uuc1, missing = "")


uuc1a <- Manningtrap_critical(y = 1.454, b = 4, m = 3, Sf = 0.02, n = 0.0550, units = "Eng", type = "symmetrical", critical = "accurate", output = "list")

uuc1b <- Manningtrap_critical(y = 1.454, b = 4, m = 3, Sf = 0.02, n = 0.0550, units = "Eng", type = "symmetrical", critical = "approximate", output = "list")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

compare_ex1 <- data.table(Parameter = c("Flow", "Depth", "Area of Flow", "Wetted Perimeter", "Hydraulic Radius", "Average Velocity", "Top Width (T)", "Side slope 1 (Z1)", "Side slope 2 (Z2)", "Channel width (B)", "Longitudinal slope", "Manning's roughness", "Normal Froude Number", "Critical Froude Number", "Critical Depth", "Critical Velocity", "Critical Slope", "Critical Top Width", "Max Shear Stress", "Avg. Shear Stress"), `FHWA Value` = c(44.001, 1.454, 12.161, 13.197, 0.921, 3.618, 12.725, 3.0, 3.0, 4.0, 0.02, 0.0550, 0.652, "", 1.163, 5.052, 0.04979, 10.978, 1.815, 1.150), `Wang Value` = c(43.9882, 1.454, 12.16, 13.2, "", 3.6179, 12.72, 3.0, 3.0, 4.0, 0.02, 0.0550, 0.65, "", 1.17, "", 0.0492, "", "", ""), `iemisc Value (Accurate Critical Froude)` = c(uuc1a$Q, uuc1a$y, uuc1a$A, uuc1a$P, uuc1a$R, uuc1a$V, uuc1a$B, uuc1a$m, uuc1a$m, uuc1a$b, uuc1a$Sf, uuc1a$n, uuc1a$Fr, uuc1a$Frc, uuc1a$yc, uuc1a$Vc, uuc1a$Sfc, uuc1a$Bc, uuc1a$taud, uuc1a$tau0), `iemisc Value (Approximate Critical Froude)` = c(uuc1b$Q, uuc1b$y, uuc1b$A, uuc1b$P, uuc1b$R, uuc1b$V, uuc1b$B, uuc1b$m, uuc1b$m, uuc1b$b, uuc1b$Sf, uuc1b$n, uuc1b$Fr, uuc1b$Frc, uuc1b$yc, uuc1b$Vc, uuc1b$Sfc, uuc1b$Bc, uuc1b$taud, uuc1b$tau0), Unit = c("cfs", "ft", "sq ft", "ft", "ft", "fps", "ft", "", "", "ft", "ft/ft", "", "", "", "ft", "fps", "ft/ft", "ft", "lb/ft^2", "lb/ft^2"))


pander(compare_ex1, missing = "")

## ---- echo = FALSE, out.width = '100%'----------------------------------------

linguisticsdown::include_graphics2("2019-05-22_08_24_13-ChannelAnalysis.png")


## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

fhwa_ex2 <- data.table(Parameter = c("Flow", "Depth", "Area of Flow", "Wetted Perimeter", "Hydraulic Radius", "Average Velocity", "Top Width (T)", "Side slope 1 (Z1)", "Side slope 2 (Z2)", "Channel width (B)", "Longitudinal slope", "Manning's roughness", "Froude Number", "Critical Depth", "Critical Velocity", "Critical Slope", "Critical Top Width", "Max Shear Stress", "Avg. Shear Stress"), Value = c(745.000, 3.298, 45.419, 21.320, 2.130, 16.408, 19.543, 2.0, 1.5, 8.0, 0.01, 0.0150, 1.896, 4.639, 9.965, 0.0257, 24.235, 2.058, 1.329), Unit = c("cfs", "ft", "sq ft", "ft", "ft", "fps", "ft", "", "", "ft", "ft/ft", "", "", "ft", "fps", "ft/ft", "ft", "lb/ft^2", "lb/ft^2"))



pander(fhwa_ex2, missing = "")

## ---- echo = FALSE, out.width = '100%'----------------------------------------

linguisticsdown::include_graphics2("trapezoid2.png")


## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

fang_ex2 <- data.table(Parameter = c("Flow", "Depth", "Area of Flow", "Wetted Perimeter", "Hydraulic Radius", "Average Velocity", "Top Width (T)", "Side slope 1 (Z1)", "Side slope 2 (Z2)", "Channel width (B)", "Longitudinal slope", "Manning's roughness", "Froude Number", "Critical Depth", "Critical Velocity", "Critical Slope", "Critical Top Width", "Max Shear Stress", "Avg. Shear Stress"), Value = c(745, 3.3, 45.51, 21.34, "", 16.371, 19.56, 2.0, 1.5, 8.0, 0.01, 0.0150, 1.89, 4.64, "", 0.0026, "",  "", ""), Unit = c("cfs", "ft", "sq ft", "ft", "ft", "fps", "ft", "", "", "ft", "ft/ft", "", "", "", "ft", "fps", "ft/ft", "ft", "lb/ft^2", "lb/ft^2"))


pander(fang_ex1, missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

uuc2 <- Manningtrap_critical(Q = 745, b = 8, m1 = 2, m2 = 1.5, Sf = 0.01, n = 0.0150, units = "Eng", type = "non-symmetrical", critical = "accurate", output = "data.table")

pander(uuc2, missing = "")



uuc2a <- Manningtrap_critical(Q = 745, b = 8, m1 = 2, m2 = 1.5, Sf = 0.01, n = 0.0150, units = "Eng", type = "non-symmetrical", critical = "accurate", output = "list")

uuc2b <- Manningtrap_critical(Q = 745, b = 8, m1 = 2, m2 = 1.5, Sf = 0.01, n = 0.0150, units = "Eng", type = "non-symmetrical", critical = "approximate", output = "list")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

compare_ex2 <- data.table(Parameter = c("Flow", "Depth", "Area of Flow", "Wetted Perimeter", "Hydraulic Radius", "Average Velocity", "Top Width (T)", "Side slope 1 (Z1)", "Side slope 2 (Z2)", "Channel width (B)", "Longitudinal slope", "Manning's roughness", "Normal Froude Number", "Critical Froude Number", "Critical Depth", "Critical Velocity", "Critical Slope", "Critical Top Width", "Max Shear Stress", "Avg. Shear Stress"), `FHWA Value` =  c(745.000, 3.298, 45.419, 21.320, 2.130, 16.408, 19.543, 2.0, 1.5, 8.0, 0.01, 0.0150, 1.896, "", 4.639, 9.965, 0.0257, 24.235, 2.058, 1.329), `Wang Value` = c(745, 3.3, 45.51, 21.34, "", 16.371, 19.56, 2.0, 1.5, 8.0, 0.01, 0.0150, 1.89, "", 4.64, "", 0.0026, "",  "", ""), `iemisc Value (Accurate Critical Froude)` = c(uuc2a$Q, uuc2a$y, uuc2a$A, uuc2a$P, uuc2a$R, uuc2a$V, uuc2a$B, uuc2a$m1, uuc2a$m2, uuc2a$b, uuc2a$Sf, uuc2a$n, uuc2a$Fr, uuc2a$Frc, uuc2a$yc, uuc2a$Vc, uuc2a$Sfc, uuc2a$Bc, uuc2a$taud, uuc2a$tau0), `iemisc Value (Approximate Critical Froude)` = c(uuc2b$Q, uuc2b$y, uuc2b$A, uuc2b$P, uuc2b$R, uuc2b$V, uuc2b$B, uuc2b$m1, uuc2b$m2, uuc2b$b, uuc2b$Sf, uuc2b$n, uuc2b$Fr, uuc2b$Frc, uuc2b$yc, uuc2b$Vc, uuc2b$Sfc, uuc2b$Bc, uuc2b$taud, uuc2b$tau0), Unit = c("cfs", "ft", "sq ft", "ft", "ft", "fps", "ft", "", "", "ft", "ft/ft", "", "", "", "ft", "fps", "ft/ft", "ft", "lb/ft^2", "lb/ft^2"))



pander(compare_ex2, missing = "")

