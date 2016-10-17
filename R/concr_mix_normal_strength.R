#' Concrete Mix Design for Normal Strength Concrete
#'
#' Calculates the amount of cement, sand, gravel, and water needed for 1 yd^3
#' of normal strength concrete
#'
#'
#' @param fc numeric vector that contains the concrete compressive strength
#' (psi)
#' @param slump_use numeric vector that contains the amount of slump (in)
#' @param max_size_aggr numeric vector that contains the maximum aggregate size
#' (in)
#' @param FM numeric vector that contains the "Fineness Modulus of sand"
#' (dimensionless)
#' @param dry_rod_wt_aggr numeric vector that contains the dry rodded weight of
#' aggregate (lb/ft^3)
#' @param mc_coarse numeric vector that contains the moisture content of the
#' coarse aggregate (whole number percent)
#' @param mc_fine numeric vector that contains the moisture content of the
#' fine aggregate (whole number percent)
#' @param entrainment character vector that contains either Air or Nonair
#' entrainment
#' @param construction_type character vector that contains the intended type of
#' construction
#' @param slump_value character vector that contains the slump value (Maximum,
#' Maximum + 1 inch, or Minimum)
#'
#' @return the amounts of cement, sand, gravel, and water in lb, rounded to the nearest ten, as a
#'   \code{\link[base]{list}} to make 1 yd^3 of normal strength concrete.
#'
#'
#'
#'
#' @source
#' r - Convert a character vector of mixed numbers, fractions, and integers to numeric - Stack Overflow answered by G. Grothendieck on May 20 2012 and edited on May 21 2012. See \url{https://stackoverflow.com/questions/10674992/convert-a-character-vector-of-mixed-numbers-fractions-and-integers-to-numeric}.
#'
#'
#'
#' @references
#' Edward G. Nawy, \emph{Reinforced Concrete: A Fundamental Approach}, 5th Edition, Upper Saddle River, New Jersey: Pearson Prentice Hall, 2005, page 23-28.
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#'
#' # "Example 3.1 Mixture Design of Normal-weight Concrete" from Nawy (page 23-28)
#'
#' concr_mix_normal_strength(fc = 4000, max_size_aggr = 3 / 4, FM = 2.6, dry_rod_wt_aggr =
#' 100, mc_coarse = 3, mc_fine = 2, entrainment = "Nonair", construction_type
#' = "Reinforced Foundation walls and footings", slump_value = "Maximum")
#'
#'
#' @import data.table gsubfn fpCompare
#'
#' @export
concr_mix_normal_strength <- function (fc, slump_use = NULL, max_size_aggr, FM, dry_rod_wt_aggr, mc_coarse, mc_fine, entrainment = c("Nonair", "Air"), construction_type = c("Reinforced Foundation walls and footings", "Plain footings and caissons", "Slabs, beams and reinforced walls", "Building Columns", "Pavements and slabs", "Heavy mass construction"), slump_value = c("Maximum", "Maximum + 1", "Minimum")) {


construction_type <- construction_type

slump_value <- slump_value

entrainment <- entrainment

# function to convert from fractions and numeric numbers to numeric (decimal)
# Source 1 begins
to_numeric <- function(n) {
    p <- c(if (length(n) == 2) 0, as.numeric(n), 0:1)
    p[1] + p[2] / p[3]
}
# Source 1 ends


# Tables
# Table 3.1 Recommended Slumps for Various Types of Construction Slump
slump <- data.table(V1 = c("Reinforced Foundation walls and footings", "Plain footings and caissons", "Slabs, beams and reinforced walls", "Building Columns", "Pavements and slabs", "Heavy mass construction"), V2 = c(3, 3, 4, 4, 3, 2), V3 = c(1, 1, 1, 1, 1, 1))
setnames(slump, c("Types of Construction", "Maximum Slump (in)", "Minimum Slump (in)"))


# Table 3.2 Approximate mixing water and air content for different slumps and Nominal maximum sizes of aggregate
# Water (lb/yd^3 of concrete for indicated Nominal Maximum Sizes of Aggregate)

# Nonair-Entrained Concrete
water_nonair <- data.table(V1 = c("1 to 2", "3 to 4", "6 to 7", "Approximate amount of entrapped air in nonair- entrained concrete (%)"), V2 = c(350, 385, 410, 3), V3 = c(335, 365, 385, 2.5), V4 = c(315, 340, 360, 2), V5 = c(300, 325, 340, 1.5), V6 = c(275, 300, 315, 1), V7 = c(260, 285, 300, 0.5), V8 = c(220, 245, 270, 0.3), V9 = c(190, 210, 0, 0.2))
setnames(water_nonair, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in.", "1 in.", "1 1/2 in.", "2 in.", "3 in.", "6 in."))


# Air Entrained Concrete
water_air <- data.table(V1 = c("1 to 2", "3 to 4", "6 to 7"), V2 = c(305, 340, 365), V3 = c(295, 325, 345), V4 = c(280, 305, 325), V5 = c(270, 295, 310), V6 = c(250, 275, 290), V7 = c(240, 265, 290), V8 = c(205, 225, 280), V9 = c(180, 200, 0))
setnames(water_air, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in.", "1 in.", "1 1/2 in.", "2 in.", "3 in.", "6 in."))


# Recommended average total air content (percent for level of exposure)
avg_air_content <- data.table(V1 = c("Mild exposure", "Moderate Exposure", "Extreme exposure"), V2 = c(4.5, 6, 7.5), V3 = c(4, 5.5, 7), V4 = c(3.5, 5, 6), V5 = c(3, 4.5, 6), V6 = c(2.5, 4.5, 5.5), V7 = c(2, 4, 5), V8 = c(1.5, 3.5, 4.5), V9 = c(1, 3, 4))
setnames(avg_air_content, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in.", "1 in.", "1 1/2 in.", "2 in.", "3 in.", "6 in."))


# Maximum size of Aggregate Recommended for various Types of Construction
# Maximum Size Aggregates, in. (mm)
max_size_aggregate <- data.table(V1 = c("2 1/2 - 5", "6 - 11", "12 - 29"), V2 = c("1/2 - 2/3", "3/4 - 1 1/2", "1 1/2 - 3"), V3 = c("3/4", "1 1/2", "3"), V4 = c("3/4 - 1", "1 1/2", "1 1/2 - 3"), V5 = c("3/4 - 1 1/2", "1 1/2 - 3", "3"))
setnames(max_size_aggregate, c("Minimum Dimension of Section, in. (mm)", "Reinforced Walls, Beams and Columns", "Unreinforced Walls", "Heavily Reinforced Slabs", "Lightly Reinforced or Underinforced Slab"))


# Table 3.3: Relationship between Water/Cement Ratio and Compressive strength
# Water/Cement Ratio, by mass (last 2 columns)
wc <- data.table(V1 = c(6000, 5000, 4000, 3000, 2000), V2 = c(0.41, 0.48, 0.57, 0.68, 0.82), V3 = c("", 0.40, 0.48, 0.59, 0.74))
setnames(wc, c("Compressive strength at 28 days (psi)", "Nonair entrained Concrete", "Air-entrained Concrete"))


# Table 3.4 Volume of Coarse aggregate per unit volume of concrete
# Volume of dry-rodded coarse aggregate per unit volume of concrete for different fineness moduli of sand (last 3 columns)
vol_coarse <- data.table(V1 = c(3 / 8, 1 / 2, 3 / 4, 1, 1 + 1 / 2, 2, 3, 6), V2 = c(0.50, 0.59, 0.66, 0.71, 0.75, 0.78, 0.82, 0.87), V3 = c(0.48, 0.57, 0.64, 0.69, 0.73, 0.76, 0.80, 0.85), V4 = c(0.44, 0.53, 0.60, 0.65, 0.69, 0.72, 0.76, 0.81))
setnames(vol_coarse, c("Maximum size of aggregate (in)", "2.40", "2.60", "2.80"))


# Table 3.5: First estimate of weight of Fresh concrete
# First Estimate of Concrete Weight (lb/yd^3)
wt_concrete <- data.table(V1 = c(3 / 8, 1 / 2, 3 / 4, 1, 1 + 1 / 2, 2, 3, 6), V2 = c(3840, 3890, 3960, 4010, 4070, 4120, 4160, 4230), V3 = c(3690, 3760, 3840, 3900, 3960, 4000, 4040, 4120))
setnames(wt_concrete, c("Max. size of Aggregate (in)", "Nonair-entrained", "Air-Entrained"))


# Concrete Mix Design for Normal Strength Concrete
# Reference: Reinforced Concrete: A Fundamental Approach, 5th Edition, Edward G. Nawy, Prentice Hall

# Procedure:

if (missing(slump_use)) {

# 1. Decide on slump from the slump (Table 3.1), if not already known
# determine the slump

if (slump_value == "Maximum") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 2, with = FALSE][[1]] # 1 for column 2 - Maximum slump

} else if (slump_value == "Maximum + 1") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 2, with = FALSE][[1]] + 1 # 2 for column 2 - Maximum slump

} else if (slump_value == "Minimum") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 3, with = FALSE][[1]] + 1 # 3 for column 3 - Minimum slump

}

} else if (!missing(slump_use)) {

slump_use <- slump_use

}


# 2. Decide on the maximum size of aggregate using the following guidelines:
# Maximum size 	not greater than 1/5 narrower dimension between forms
#			Not greater than 1/3 depth of slab
#			Not greater 3/4 of clear spacing between reinforcing bars


# 3. Decide on amount of water and air (Table 3.2)
# determine the water content
# look at the entrainment

if (entrainment == "Nonair") {

water_nonair_col_numeric <- gsub(" in.", "", colnames(water_nonair)[2:ncol(water_nonair)])

water_nonair_col_numeric <- sapply(strapplyc(water_nonair_col_numeric, "\\d+"), to_numeric) # Source 1


# determine the water content
if (between(slump_use, 1, 2)) {

water <- water_nonair[1, which(water_nonair_col_numeric %==% max_size_aggr)+1L, with = FALSE][[1]]

} else if(between(slump_use, 3, 4)) {

water <- water_nonair[2, which(water_nonair_col_numeric %==% max_size_aggr)+1L, with = FALSE][[1]]

} else if(between(slump_use, 6, 7)) {

water <- water_nonair[3, which(water_nonair_col_numeric %==% max_size_aggr)+1L, with = FALSE][[1]]

}

} else if (entrainment == "Air") {

water_air_col_numeric <- gsub(" in.", "", colnames(water_air)[2:ncol(water_air)])

water_air_col_numeric <- sapply(strapplyc(water_air_col_numeric, "\\d+"), to_numeric) # Source 1

# determine the water content
if (between(slump_use, 1, 2)) {

water <- water_air[1, which(water_air_col_numeric %==% max_size_aggr)+1L, with = FALSE][[1]]

} else if(between(slump_use, 3, 4)) {

water <- water_air[2, which(water_air_col_numeric %==% max_size_aggr)+1L, with = FALSE][[1]]

} else if(between(slump_use, 6, 7)) {

water <- water_air[3, which(water_air_col_numeric %==% max_size_aggr)+1L, with = FALSE][[1]]

}
}


wc1 <- wc[, 1, with = FALSE]

# 4. Select water/cement ratio w/c (Table 3.3)

if (entrainment == "Nonair") {

# determine the water/cement ratio

water_cement <- wc[which(fc %==% wc1), 2, with = FALSE][[1]]

} else if (entrainment == "Air") {

# determine the water/cement ratio

water_cement <- wc[which(fc %==% wc1), 3, with = FALSE][[1]]

}


# 5. Calculate cement amount ( = c/w * weight of water)

cement <- water / water_cement


# 6. Choose the amount of coarse aggregate (Table 3.4)

vol_coarse_col_numeric <- as.numeric(colnames(vol_coarse[, 2:ncol(vol_coarse), with = FALSE]))

gravel_yd3 <- vol_coarse[which(max_size_aggr %==% vol_coarse[, 1, with = FALSE]), which(vol_coarse_col_numeric %==% FM)+1L, with = FALSE][[1]]

gravel <- gravel_yd3 * 27 * dry_rod_wt_aggr


# 7. Calculate the amount of fine aggregate using Estimate table the estimated weight of fresh concrete (table 3.5) and the known weights of water, cement, and course aggregates

if (entrainment == "Nonair") {

# determine the water/cement ratio

concrete <- wt_concrete[which(max_size_aggr %==% wt_concrete[, 1, with = FALSE]), 2, with = FALSE][[1]]

} else if (entrainment == "Air") {

# determine the water/cement ratio

concrete <- wt_concrete[which(max_size_aggr %==% wt_concrete[, 1, with = FALSE]), 3, with = FALSE][[1]]

}


sand <- concrete - water - cement - gravel


# 8. Adjust for moisture content in course and fine aggregate.

net_sand <- (1 + mc_fine / 100) * sand

net_gravel <- (1 + mc_coarse / 100) * gravel

net_water <- water - mc_fine / 100 * sand - mc_coarse / 100 * gravel


# 9. Trial mix
# 10. End

# for 1 yd^3 of concrete

return(list(cement = round(cement, digits = -1), sand = round(net_sand, digits = -1), gravel = round(net_gravel, digits = -1), water = round(net_water, digits = -1)))

}
