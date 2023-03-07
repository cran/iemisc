## ----eval = FALSE-------------------------------------------------------------
#  install.packages(c("install.load", "iemisc", "pander"))
#  # install the packages and their dependencies

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
install.load::load_package("iemisc", "pander")

panderOptions("table.continues", "")
panderOptions("table.caption.prefix", "")

# values to match the Reference document
T = 20; pTe = 1; contam1 = c("Benzene", "Toluene", "Trichloroethylene"); Cai = c(750, 1000, 750); Cae = c(10, 100, 100); contam2 = c("Benzene", "Toluene", "Trichloroethylene"); cas = c("71-43-2", "108-88-3", "79-01-6"); Ha = c(309.2, 353.1, 506.1); Q = 440; loading = 45; ns = 2; DL = c(8.91 * 10 ^ -10, NA_real_, NA_real_); DG = c(9.37 * 10 ^ -6, NA_real_, NA_real_); dP = 0.0508; at = 157; Sc = 0.033; cf = 15; R = 3.5; dP_units = "inch"; at_units = "ft^2/ft^3"; Sc_units = "kg/s^2"; contaminants_table = 1; removal_requirements_table = 1; critical_contaminant_table = 1

air1 <- air_stripper(T = T, pTe = pTe, contam1 = contam1, Cai = Cai, Cae = Cae, contam2 = contam2, cas = cas, Ha = Ha, Q = Q, loading = loading, ns = ns, DL = DL, DG = DG, dP = dP, at = at, Sc = Sc, cf = cf, R = R, T_units = "SI", dP_units = "inch", at_units = "ft^2/ft^3", Sc_units = "kg/s^2", contaminants_table = 1, removal_requirements_table = 1, critical_contaminant_table = 1)



# Changes to reflect the manufacturer's values
T = 20; pTe = 1; contam1 = c("Benzene", "Toluene", "Trichloroethylene"); Cai = c(750, 1000, 750); Cae = c(10, 100, 100); contam2 = c("Benzene", "Toluene", "Trichloroethylene"); cas = c("71-43-2", "108-88-3", "79-01-6"); Ha = c(309.2, 353.1, 506.1); Q = 440; loading = 45; ns = 2; DL = c(8.91 * 10 ^ -10, NA_real_, NA_real_); DG = c(9.37 * 10 ^ -6, NA_real_, NA_real_); dP = 2; at = 48; Sc = 0.033; cf = 16; R = 3.5; T_units = "SI"; dP_units = "inch"; at_units = "ft^2/ft^3"; Sc_units = "kg/s^2"; contaminants_table = 1; removal_requirements_table = 1; critical_contaminant_table = 1

air2 <- air_stripper(T = T, pTe = pTe, contam1 = contam1, Cai = Cai, Cae = Cae, contam2 = contam2, cas = cas, Ha = Ha, Q = Q, loading = loading, ns = ns, DL = DL, DG = DG, dP = dP, at = at, Sc = Sc, cf = cf, R = R,  T_units = "SI", dP_units = "inch", at_units = "ft^2/ft^3", Sc_units = "kg/s^2", contaminants_table = 1, removal_requirements_table = 1, critical_contaminant_table = 1)

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air1[[1]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air1[[2]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air1[[3]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air1[[4]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air2[[1]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air2[[2]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air2[[3]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air2[[4]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------

install.load::load_package("iemisc")

air3 <- air_stripper(T = 20, pTe = 1, contam1 = "Ammonia", Cai = 333, Cae = 2.8, contam2 = "Ammonia", cas = "7664-41-7", Ha = 0.75, Q = 150, loading = 45, ns = 2, DL = 8.91 * 10 ^ -10, DG = 9.37 * 10 ^ -6, dP = 145, at = 65, Sc = 0.033, cf = 76 *6, R = 1.5,  T_units = "SI", dP_units = "mm", at_units = "m^2/m^3", Sc_units = "kg/s^2", contaminants_table = 1, removal_requirements_table = 1, critical_contaminant_table = 1)


air4 <- air_stripper(T = 25, pTe = 1, contam1 = "Ammonia", Cai = 700, Cae = 2.8, contam2 = "Ammonia", cas = "7664-41-7", Ha = 0.75, Q = 440, loading = 45, ns = 3, DL = 2.1E-09, DG = 9.8E-06, dP = 6.35, at = 940, Sc = 0.061, cf = 1600, R = 1.5,  T_units = "SI", dP_units = "mm", at_units = "m^2/m^3", Sc_units = "kg/s^2", contaminants_table = 1, removal_requirements_table = 1, critical_contaminant_table = 1)

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air3[[1]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air3[[2]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air3[[3]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air3[[4]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air4[[1]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air4[[2]], missing = "")

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air4[[3]])

## ---- warning = FALSE, message = FALSE, tidy = TRUE---------------------------
pander(air4[[4]])

## ---- echo = FALSE, out.width = '100%'----------------------------------------
linguisticsdown::include_graphics2("https://i.creativecommons.org/l/by-sa/4.0/88x31.png")

