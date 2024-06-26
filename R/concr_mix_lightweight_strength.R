#' Concrete Mix Design for Structural Lightweight Concrete
#'
#' Calculates the amount of cement, sand, gravel, and water needed for a test
#' batch volume of structural lightweight concrete using the weight method.
#' Note: Currently, this function only works with air-entrained concrete as the
#' author has not found a table to compute the weight of concrete for nonair-
#' entrained concrete.
#'
#'
#' @param fc numeric vector that contains the concrete compressive strength
#'    (psi)
#' @param slump_use numeric vector that contains the amount of slump (in)
#' @param max_size_aggr numeric vector that contains the maximum aggregate size
#'    (in)
#' @param FM numeric vector that contains the "Fineness Modulus of sand"
#'    (dimensionless)
#' @param dry_rod_wt_aggr numeric vector that contains the dry rodded weight of
#'    aggregate ["oven-dry loose weight of coarse aggregate"] (lb/ft^3)
#' @param sgf_coarse numeric vector that contains the "specific gravity factor"
#'    of the coarse aggregate (dimensionless)
#' @param absorp_coarse numeric vector that contains the absorption of the
#'    coarse aggregate (whole number percent)
#' @param absorp_fine numeric vector that contains the absorption of the
#'    fine aggregate (whole number percent)
#' @param entrainment character vector that contains either Air or Nonair
#'    entrainment
#' @param construction_type character vector that contains the intended type of
#'    construction
#' @param slump_value character vector that contains the slump value (Maximum,
#'    Maximum + 1, Minimum, or Minimum + 1). It is "+ 1 in. for methods of
#'    consolidation other than vibration"
#' @param exposure character vector that contains the exposure value (Mild,
#'    Moderate, or Extreme) for use with Air entrained concrete mixes
#' @param structure_type character vector that contains the severe exposure
#'    value ["Thin sections (railings, curbs, sills, ledges, ornamental work) and
#'    sections with less than 1 in. cover over steel" or "All other structures"]
#'    for use with Air entrained concrete mixes with severe exposure
#' @param severe_exposure character vector that contains the severe exposure
#'    value ("Structure wet continuously or frequently and exposed to freezing and
#'    thawing" or "Structure exposed to sea water or sulfates") for use with Air
#'    entrained concrete mixes with severe exposure
#' @param trial_batch character vector that contains the volume of the trial
#'    batch mix to return (1 cubic yard, 1 cubic foot, 0.5 cubic foot, 0.2
#'    cubic foot, or All)
#'
#'
#' @return the amounts of cement, sand, gravel, and water in lb, rounded to the
#'    hundredth, as a \code{\link[base]{list}} to make 1 yd^3, 1 ft^3, 0.5 ft^3,
#'    or 0.2 ft^3 of structural lightweight concrete or as a \code{\link[data.table]{data.table}} containing
#'    all batch volumes.
#'
#'
#'
#'
#' @source
#' \enumerate{
#'    \item r - Error when doing bilinear interpolation with 'interp2 {pracma}'; any better way for 2D interpolation? - Stack Overflow answered and edited by Zheyuan Li on Dec 8 2016. See \url{https://stackoverflow.com/questions/41032225/error-when-doing-bilinear-interpolation-with-interp2-pracma-any-better-way}.
#'    \item r - data.table 1.10.0 - why does a named column index value not work while a integer column index value works without with = FALSE - Stack Overflow answered and edited by Matt Dowle on Dec 8 2016. See \url{https://stackoverflow.com/questions/41032225/error-when-doing-bilinear-interpolation-with-interp2-pracma-any-better-way}.
#' }
#'
#'
#'
#' @references
#' ACI Committee 211, \emph{Standard Practice for Selecting Proportions for Structural Lightweight Concrete (ACI 211.2-98)}, American Concrete Institute, Farmington Hills, MI, 18 pages. 1998, 211.2-98.
#'
#' @author Irucka Embry, Hans Werner Borchers for the interp1 and interp2 functions from pracma
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @seealso \code{\link{concr_mix_normal_strength}} for Concrete Mix Design for Normal Strength
#' (Normal-weight) Concrete
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example A from Section 3.2.3 using the 'Weight method (specific gravity
#' # pycnometers)' from ACI Committee 211
#' # Design a concrete mix for 3500 psi concrete strength, 'floor slab of a
#' # multistory structure subjected to freezing and thawing during
#' # construction', and a maximum size of aggregate = 3/4 in, with Fineness
#' # Modulus of sand = 2.80, 'the oven-dry loose weight of coarse aggregate' =
#' # 47 lb/ft^3 with a specific gravity factor = 1.50, and a absorption of
#' # 11.0\% for the coarse aggregate and 1.0\% for the fine aggregate.
#'
#' concr_mix_lightweight_strength(fc = 3500, max_size_aggr = 3 / 4, FM = 2.80,
#' sgf_coarse = 1.50, dry_rod_wt_aggr = 47, absorp_coarse = 11.0,
#' absorp_fine = 1.0, entrainment = "Air", construction_type = "Floor slabs",
#' slump_value = "Maximum", exposure = "Extreme", structure_type = "Other",
#' severe_exposure = "Wet", trial_batch = "1 cubic foot")
#'
#'
#' @importFrom data.table setnames setattr := setDT between melt
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom fpCompare %==%
#' @importFrom round round_r3
#' @importFrom pracma interp1
#' @importFrom pracma interp2
#'
#'
#' @export
concr_mix_lightweight_strength <- function (fc, slump_use = NULL, max_size_aggr, FM, sgf_coarse, dry_rod_wt_aggr, absorp_coarse, absorp_fine, entrainment = c("Nonair", "Air"), construction_type = c("Beams and reinforced walls", "Building columns", "Floor slabs"), slump_value = c("Maximum", "Maximum + 1", "Minimum", "Minimum + 1"), exposure = c("Mild", "Moderate", "Extreme"), structure_type = c("Thin section", "Other"), severe_exposure = c("Wet", "Sea water"), trial_batch = c("1 cubic yard", "1 cubic foot", "0.5 cubic foot", "0.2 cubic foot", "All")) {


entrainment <- entrainment

construction_type <- construction_type

slump_value <- slump_value

exposure <- exposure

structure_type <- structure_type

severe_exposure <- severe_exposure

trial_batch <- trial_batch



# Check fc, max_size_aggr, FM, sgf_coarse, dry_rod_wt_aggr, absorp_coarse, absorp_fine, entrainment, construction_type, slump_value, exposure, structure_type, severe_exposure, trial_batch
assert_that(qtest(fc, "N==1(0,)"), msg = "fc is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(max_size_aggr, "N==1(0,)"), msg = "max_size_aggr is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(FM, "N==1(0,)"), msg = "FM is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(sgf_coarse, "N==1(0,)"), msg = "sgf_coarse is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(dry_rod_wt_aggr, "N==1(0,)"), msg = "dry_rod_wt_aggr is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(absorp_coarse, "N==1(0,)"), msg = "absorp_coarse is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(absorp_fine, "N==1(0,)"), msg = "absorp_fine is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(isTRUE(any(c("Nonair", "Air") %in% entrainment)), msg = "The entrainment has not been identified correctly. Please try again.")
# only process with a specified entrainment and provide a stop warning if not

assert_that(isTRUE(any(c("Beams and reinforced walls", "Building columns", "Floor slabs") %in% construction_type)), msg = "The construction_type has not been identified correctly. Please try again.")
# only process with a specified construction type and provide a stop warning if not

assert_that(isTRUE(any(c("Maximum", "Maximum + 1", "Minimum", "Minimum + 1") %in% slump_value)), msg = "The slump_value  has not been identified correctly. Please try again.")
# only process with a specified slump value and provide a stop warning if not

assert_that(isTRUE(any(c("Mild", "Moderate", "Extreme") %in% exposure)), msg = "The exposure has not been identified correctly. Please try again.")
# only process with a specified exposure and provide a stop warning if not

assert_that(isTRUE(any(c("Thin section", "Other") %in% structure_type)), msg = "The structure_type has not been identified correctly. Please try again.")
# only process with a specified structure type and provide a stop warning if not

assert_that(isTRUE(any(c("Wet", "Sea water") %in% severe_exposure)), msg = "The severe_exposure has not been identified correctly. Please try again.")
# only process with a specified severe exposure and provide a stop warning if not

assert_that(isTRUE(any(c("1 cubic yard", "1 cubic foot", "0.5 cubic foot", "0.2 cubic foot", "All") %in% trial_batch)), msg = "The trial_batch has not been identified correctly. Please try again.")
# only process with a specified trial batch and provide a stop warning if not




# Tables
# Table 3.2.2.1 Recommended slumps for various types of construction
slump <- data.table(V1 = c("Beams and reinforced walls", "Building columns", "Floor slabs"), V2 = c(4, 4, 3), V3 = c(1, 1, 1))
setnames(slump, c("Types of Construction", "Maximum Slump (in)", "Minimum Slump (in)"))


# Table 3.2.2.2 Approximate mixing water and air content requirements for different slumps and nominal maximum sizes of aggregates*

# Water, lb/yd^3 of concrete for indicated nominal maximum sizes of aggregates

# Air-entrained concrete
water_air <- data.table(V1 = c("1 to 2", "3 to 4", "5 to 6", "Recommended average+ total air content, percent, for level of exposure"), V2 = c(305, 340, 355,  NA_real_), V3 = c(295, 325, 335,  NA_real_), V4 = c(280, 305, 315,  NA_real_))
setnames(water_air, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in."))


# Recommended average total air content, percent, for level of exposure
avg_air_content <- data.table(V1 = c("Mild exposure", "Moderate exposure", "Extreme exposure"), V2 = c(4.5, 6.0, 7.5), V3 = c(4.0, 5.5, 7.0), V4 = c(4.0, 5.0, 6.0))
setnames(avg_air_content, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in."))


# Non-air-entrained concrete
water_nonair <- data.table(V1 = c("1 to 2", "3 to 4", "5 to 6", "Approximate amount of entrapped air in non-air-entrained concrete, percent"), V2 = c(350, 385, 400, 3), V3 = c(335, 365, 375, 2.5), V4 = c(315, 340, 350, 2))
setnames(water_nonair, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in."))


# Table 3.2.2.3(a) Relationships between water-cement ratio and compressive strength of concrete*

# Approximate water-cement ratio, by weight (last 2 columns)
wca <- data.table(V1 = c(6000, 5000, 4000, 3000, 2000), V2 = c(0.41, 0.48, 0.57, 0.68, 0.82), V3 = c( NA_real_, 0.40, 0.48, 0.59, 0.74))
setnames(wca, c("Compressive strength at 28 days (psi)", "Non-air-entrained concrete", "Air-entrained concrete"))


# Table 3.2.2.3(b) Maximum permissible water-cement ratios for concrete in severe exposures*
wcb <- data.table(V1 = c("Thin sections (railings, curbs, sills, ledges, ornamental work) and sections with less than 1 in. cover over steel", "All other structures"), V2 = c(0.45, 0.50), V3 = c(0.40, 0.45))
setnames(wcb, c("Types of structure", "Structure wet continuously or frequently and exposed to freezing and thawing%", "Structure exposed to sea water or sulfates"))


# Table 3.2.2.4 Volume of coarse aggregate per unit of volume of concrete*
# Volume of oven-dry loose coarse aggregates* per unit volume of concrete for different fineness moduli of sand (last 4 columns)
vol_coarse <- data.table(V1 = c(3 / 8, 1 / 2, 3 / 4), V2 = c(0.58, 0.67, 0.74), V3 = c(0.56, 0.65, 0.72), V4 = c(0.54, 0.63, 0.70), V5 = c(0.52, 0.61, 0.68))
setnames(vol_coarse, c("Maximum size of aggregate, in.", "2.40", "2.60", "2.80", "3.00"))


# Table 3.2.2.5 First estimate of weight of fresh lightweight concrete comprised of lightweight coarse aggregate and normal weight fine aggregate
# First estimate of lightweight concrete weight, lb/yd^3* (last 3 columns)
# Air-Entrained (last 3 columns)
wt_concrete <- data.table(V1 = c(1.00, 1.20, 1.40, 1.60, 1.80, 2.00), V2 = c(2690, 2830, 2980, 3120, 3260, 3410), V3 = c(2630, 2770, 2910, 3050, 3200, 3340), V4 = c(2560, 2710, 2850, 2990, 3130, 3270))
setnames(wt_concrete, c("Specific gravity factor", "4 percent", "6 percent", "8 percent"))


# Concrete Mix Design for Structural Lightweight Concrete
# Reference:

# Method 1 (weight method, specific gravity pycnometers) -- Lightweight coarse aggregate and normal weight fine aggregate
# Procedure:

if (missing(slump_use)) {

# 1. Decide on slump from the slump (Table 3.2.2.1), if not already known
# determine the slump

if (slump_value == "Maximum") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 2][[1]] # 1 for column 2 - Maximum slump

} else if (slump_value == "Maximum + 1") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 2][[1]] + 1 # 2 for column 2 - Maximum slump

} else if (slump_value == "Minimum") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 3][[1]] # 3 for column 3 - Minimum slump

} else if (slump_value == "Minimum + 1") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 3][[1]] + 1 # 3 for column 3 - Minimum slump

}

} else if (!missing(slump_use)) {

# Check for slump_use
assert_that(!is.character(slump_use), msg = "The object `slump_use` is not a number. `slump_use` cannot be a string. Please try again.")

assert_that(any(!(slump_use %==% 0 == TRUE)), msg = "slump_use is 0. slump_use can not be 0. Please try again.")
# only process with a non-zero value for R and provide a stop warning if R = 0

assert_that(is.finite(slump_use), msg = "slump_use is NA. Please try again.")

slump_use <- slump_use

}



# 2. Decide on the maximum size of aggregate using the following guidelines:
# Maximum size not greater than 1/5 narrower dimension between sides of forms
#			Not greater than 1/3 depth of slab
#			Not greater 3/4 of "clear spacing between individual reinforcing bars, bundles of bars, or pretensioning strands"


# 3. Decide on amount of water and air (Table 3.2.2.2)
# determine the water content
# look at the entrainment
# determine the total air content, if air entrained


if (entrainment == "Nonair") {

water_nonair_col_numeric <- gsub(" in.", "", colnames(water_nonair)[2:ncol(water_nonair)])

water_nonair_col_numeric <- unlist(lapply(water_air_col_numeric, frac_to_numeric))

water_nonair_column <- which(water_nonair_col_numeric %==% max_size_aggr)+1L


# determine the water content
if (between(slump_use, 1, 2)) {

water <- water_nonair[1, ..water_nonair_column][[1]] # Source 2

} else if(between(slump_use, 3, 4)) {

water <- water_nonair[2, ..water_nonair_column][[1]] # Source 2

} else if(between(slump_use, 5, 6)) {

water <- water_nonair[3, ..water_nonair_column][[1]] # Source 2

}

} else if (entrainment == "Air") {

water_air_col_numeric <- gsub(" in.", "", colnames(water_air)[2:ncol(water_air)])

water_air_col_numeric <- unlist(lapply(water_air_col_numeric, frac_to_numeric))

water_air_column <- which(water_air_col_numeric %==% max_size_aggr)+1L


# determine the water content
if (between(slump_use, 1, 2)) {

water <- water_air[1, ..water_air_column][[1]] # Source 2

} else if(between(slump_use, 3, 4)) {

water <- water_air[2, ..water_air_column][[1]] # Source 2

} else if(between(slump_use, 5, 6)) {

water <- water_air[3, ..water_air_column][[1]] # Source 2

}

if (exposure == "Mild") {

avg_air_content_col_numeric <- gsub(" in.", "", colnames(avg_air_content)[2:ncol(avg_air_content)])

avg_air_content_col_numeric <- unlist(lapply(avg_air_content_col_numeric, frac_to_numeric))

water_avg_air_column <- which(avg_air_content_col_numeric %==% max_size_aggr)+1L

total_air <- avg_air_content[1, ..water_avg_air_column][[1]] # Source 2


} else if(exposure == "Moderate") {

avg_air_content_col_numeric <- gsub(" in.", "", colnames(avg_air_content)[2:ncol(avg_air_content)])

avg_air_content_col_numeric <- unlist(lapply(avg_air_content_col_numeric, frac_to_numeric))

water_avg_air_column <- which(avg_air_content_col_numeric %==% max_size_aggr)+1L

total_air <- avg_air_content[2, ..water_avg_air_column][[1]] # Source 2


} else if(exposure == "Extreme") {

avg_air_content_col_numeric <- gsub(" in.", "", colnames(avg_air_content)[2:ncol(avg_air_content)])

avg_air_content_col_numeric <- unlist(lapply(avg_air_content_col_numeric, frac_to_numeric))

water_avg_air_column <- which(avg_air_content_col_numeric %==% max_size_aggr)+1L

total_air <- avg_air_content[3, ..water_avg_air_column][[1]] # Source 2

}
}


wc1 <- wca[, 1][[1]]
wc2 <- wca[, 2][[1]]
wc3 <- wca[-1, 3][[1]]

# 4. a. Select water/cement ratio w/c (Table 3.2.2.3(a))

if (entrainment == "Nonair") {

# determine the water/cement ratio

water_cement <- interp1(wc1, wc2, xi = fc, method = "linear")

} else if (entrainment == "Air") {

# determine the water/cement ratio

water_cement <- interp1(wc1, c(0, wc3), xi = fc, method = "linear")
# Replace NA with 0 for this portion only


# 4. b. If severe exposure, then check Table 3.2.2.3(b)

if(exposure == "Extreme") {

exp_search <- which(grepl(structure_type, wcb[, 1][[1]], ignore.case = TRUE))

water_cement_search <- which(grepl(severe_exposure, colnames(wcb), ignore.case = TRUE))

water_cement_max <- wcb[exp_search, ..exp_search][[1]] # Source 2

water_cement <- ifelse(water_cement_max < water_cement, water_cement_max, water_cement)

}
}


# 5. Calculate cement amount ( = c/w * weight of water)

cement <- water / water_cement


# 6. Choose the amount of lightweight coarse aggregate (Table 3.2.2.4)

vol_coarse_col_numeric <- as.numeric(colnames(vol_coarse[, 2:ncol(vol_coarse), with = FALSE]))

z <- cbind(vol_coarse[, 2][[1]], vol_coarse[, 3][[1]], vol_coarse[, 4][[1]], vol_coarse[, 5][[1]])

gravel_yd3 <- interp2(x = vol_coarse[, 1][[1]], y = vol_coarse_col_numeric, Z = t(z), xp = max_size_aggr, yp = FM, method = "linear") # Source 1

gravel <- gravel_yd3 * 27 * dry_rod_wt_aggr

# "saturated surface dry (SSD) weight of gravel"
gravel_SSD <- (1 + absorp_coarse / 100) * gravel


# 7. Calculate the amount of fine aggregate using Estimate table the estimated weight of fresh concrete (Table 3.2.2.5) and the known weights of water, cement, and course aggregates

# There is no table specifying Nonair entrained concrete, so this will have to do until a table can be located (or a formula) to determine the concrete for Nonair entrainment

wt_concrete_col_numeric <- as.numeric(gsub(" percent", "", colnames(wt_concrete)[2:ncol(wt_concrete)]))

z1 <- cbind(wt_concrete[, 2][[1]], wt_concrete[, 3][[1]], wt_concrete[, 4][[1]])

concrete <- interp2(x = wt_concrete[, 1][[1]], y = wt_concrete_col_numeric, Z = t(z1), xp = sgf_coarse, yp = total_air, method = "linear") # Source 1


# "saturated surface dry (SSD) weight of sand"
sand_SSD <- concrete - water - cement - gravel_SSD


# "Oven-dry weight of sand"
sand <- sand_SSD / (1 + absorp_fine / 100)



# for 1 yd^3 of concrete
if (trial_batch == "1 cubic yard") {

return(list(cement = round_r3(cement, d = 2), sand = round_r3(sand_SSD, d = 2), gravel = round_r3(gravel_SSD, d = 2), water = round_r3(water, d = 2)))


# for 1 ft^3 of concrete
} else if (trial_batch == "1 cubic foot") {

return(list(cement = round_r3(cement / (3 ^ 3), d = 2), sand = round_r3(sand_SSD / (3 ^ 3), d = 2), gravel = round_r3(gravel_SSD / (3 ^ 3), d = 2), water = round_r3(water / (3 ^ 3), d = 2)))


# for 0.5 ft^3 of concrete
} else if (trial_batch == "0.5 cubic foot") {

return(list(cement = round_r3(cement * 0.5 / (3 ^ 3), d = 2), sand = round_r3(sand_SSD * 0.5 / (3 ^ 3), d = 2), gravel = round_r3(gravel_SSD * 0.5 / (3 ^ 3), d = 2), water = round_r3(water * 0.5 / (3 ^ 3), d = 2)))


# for 0.2 ft^3 of concrete
} else if (trial_batch == "0.2 cubic foot") {

return(list(cement = round_r3(cement * 0.2 / (3 ^ 3), d = 2), sand = round_r3(sand_SSD * 0.2 / (3 ^ 3), d = 2), gravel = round_r3(gravel_SSD * 0.2 / (3 ^ 3), d = 2), water = round_r3(water * 0.2 / (3 ^ 3), d = 2)))


# for All mixes of concrete
} else if (trial_batch == "All") {

concrete_mix <- list(cement = round_r3(cement, d = 2), sand = round_r3(sand_SSD, d = 2), gravel = round_r3(gravel_SSD, d = 2), water = round_r3(water, d = 2))
concrete_mix <- melt(setDT(concrete_mix), measure.vars = 1:4)
setnames(concrete_mix, 2, "V1")
concrete_mix[, `:=` (v1 = round_r3(V1 / (3 ^ 3), d = 2), v2 = round_r3(V1 * 0.5 / (3 ^ 3), d = 2), v3 = round_r3(V1 * 0.2 / (3 ^ 3), d = 2))]
setnames(concrete_mix, c("Materials", "Amount (lb) for 1 yd^3", "Amount (lb) for 1 ft^3", "Amount (lb) for 0.5 ft^3", "Amount (lb) for 0.2 ft^3"))


col.names <- c("Materials", "Amount (lb) for 1 yd^3", "Amount (lb) for 1 ft^3", "Amount (lb) for 0.5 ft^3", "Amount (lb) for 0.2 ft^3")

# code block below modified from data.table function
setattr(concrete_mix, "col.names", setnames(concrete_mix, col.names))
setattr(concrete_mix, "class", c("data.table", "data.frame"))
print(concrete_mix)

}

# 9. Trial mix
# 10. End

}
