#' Future value given Present value (Engineering Economics)
#'
#' Compute F given P
#'
#' F is expressed as
#'
#' 	\deqn{F = P\left(1 + i\right)^n}
#'
#' \describe{
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return FgivenP numeric vector that contains the future value(s) rounded to
#'    2 decimal places
#' @return FP data.frame of both n (0 to n) and the resulting future values
#'    rounded to 2 decimal places
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 124, 142, 164-166.
#'
#'
#'
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
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example 4-3 from the Reference text (page 124)
#' FgivenP(8000, 4, 10, frequency = "annual") # the interest rate is 10\%
#'
#' FP(8000, 4, 10, frequency = "annual") # the interest rate is 10\%
#'
#'
#' FgivenP(P = c(1000, 340, 23), n = c(12, 1.3, 3), i = c(10, 2, 0.3),
#' "annual")
#' # is is 10\%, 2\%, and 0.3%
#' # Can't use FP for this example
#'
#'
#' # Example 4-29 from the Reference text (page 165-166)
#' FgivenP(100, 10, 6, "quarter") # the interest rate is 6\% per quarter
#'
#' FP(100, 10, 6, "quarter") # the interest rate is 6\% per quarter
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom data.table data.table setnames setDF
#' @importFrom round round_r3
#'
#' @name FgivenP
NULL

#' @export
#' @rdname FgivenP
FgivenP <- function (P, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {


frequency <- frequency

checks <- c(P, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(frequency, "S==1"), msg = "There is not a frequency or more than 1 frequency is stated. Please specify either 'annual', 'semiannual', 'quarter', 'bimonth', 'month', or 'daily'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("annual", "semiannual", "quarter", "bimonth", "month", "daily") %in% frequency)), msg = "Incorrect frequency. The only options are annual, semiannual, quarter, bimonth, month, daily. Please try again.")
# only process with a specified frequency and provide a stop warning if not


i <- i / 100

fr <- frequency

if (fr == "annual") {
fr <- 1
n <- n * fr

i <- i / fr

FgivenP <- P * ((1 + i) ^ n)

return(round_r3(FgivenP, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

FgivenP <- P * ((1 + i) ^ n)

return(round_r3(FgivenP, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

FgivenP <- P * ((1 + i) ^ n)

return(round_r3(FgivenP, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

FgivenP <- P * ((1 + i) ^ n)

return(round_r3(FgivenP, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

FgivenP <- P * ((1 + i) ^ n)

return(round_r3(FgivenP, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

FgivenP <- P * ((1 + i) ^ n)

return(round_r3(FgivenP, d = 2))

}
}



#' @export
#' @rdname FgivenP
FP <- function (P, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency

checks <- c(P, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(frequency, "S==1"), msg = "There is not a frequency or more than 1 frequency is stated. Please specify either 'annual', 'semiannual', 'quarter', 'bimonth', 'month', or 'daily'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("annual", "semiannual", "quarter", "bimonth", "month", "daily") %in% frequency)), msg = "Incorrect frequency. The only options are annual, semiannual, quarter, bimonth, month, daily. Please try again.")
# only process with a specified frequency and provide a stop warning if not



i <- i / 100

fr <- frequency

if (fr == "annual") {
fr <- 1
n <- n * fr

FP <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FP[[y]] <- P * ((1 + i) ^ seq(n))
}

FP <- data.table(seq(n), unlist(FP))

F0 <- P * ((1 + i) ^ 0)
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FP <- rbind(F0, FP)

setnames(FP, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FP[[col]]))

data.table::set(FP, i = idx, j = col, value = round_r3(FP[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FP, "col.names", setnames(FP, col.names))
setattr(FP, "class", c("data.table", "data.frame"))
FP


} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

FP <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FP[[y]] <- P * ((1 + i) ^ seq(n))
}

FP <- data.table(seq(n), unlist(FP))

F0 <- P * ((1 + i) ^ 0)
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FP <- rbind(F0, FP)

setnames(FP, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FP[[col]]))

data.table::set(FP, i = idx, j = col, value = round_r3(FP[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FP, "col.names", setnames(FP, col.names))
setattr(FP, "class", c("data.table", "data.frame"))
FP


} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

FP <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FP[[y]] <- P * ((1 + i) ^ seq(n))
}

FP <- data.table(seq(n), unlist(FP))

F0 <- P * ((1 + i) ^ 0)
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FP <- rbind(F0, FP)

setnames(FP, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FP[[col]]))

data.table::set(FP, i = idx, j = col, value = round_r3(FP[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FP, "col.names", setnames(FP, col.names))
setattr(FP, "class", c("data.table", "data.frame"))
FP


} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

FP <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FP[[y]] <- P * ((1 + i) ^ seq(n))
}

FP <- data.table(seq(n), unlist(FP))

F0 <- P * ((1 + i) ^ 0)
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FP <- rbind(F0, FP)

setnames(FP, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FP[[col]]))

data.table::set(FP, i = idx, j = col, value = round_r3(FP[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FP, "col.names", setnames(FP, col.names))
setattr(FP, "class", c("data.table", "data.frame"))
FP


} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

FP <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FP[[y]] <- P * ((1 + i) ^ seq(n))
}

FP <- data.table(seq(n), unlist(FP))

F0 <- P * ((1 + i) ^ 0)
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FP <- rbind(F0, FP)

setnames(FP, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FP[[col]]))

data.table::set(FP, i = idx, j = col, value = round_r3(FP[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FP, "col.names", setnames(FP, col.names))
setattr(FP, "class", c("data.table", "data.frame"))
FP


} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

FP <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FP[[y]] <- P * ((1 + i) ^ seq(n))
}

FP <- data.table(seq(n), unlist(FP))

F0 <- P * ((1 + i) ^ 0)
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FP <- rbind(F0, FP)

setnames(FP, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FP[[col]]))

data.table::set(FP, i = idx, j = col, value = round_r3(FP[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FP, "col.names", setnames(FP, col.names))
setattr(FP, "class", c("data.table", "data.frame"))
FP


}
}
