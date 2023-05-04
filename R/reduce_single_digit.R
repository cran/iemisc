#' Reduce an Integer, a Date (Time), or a Number (with or without Decimals) to a Single Integer
#'
#' Takes a character vector coercible to a date using \code{\link[anytime]{anydate}} or a date time
#' using \code{\link[anytime]{anytime}}; a numeric vector; or an integer vector & computes
#' the sum to a single digit using \code{\link{Mod_octave}}
#' 
#' The vectors may include periods, dashes, parentheses, colons, and/or spaces.
#' See the examples.
#'
#'
#' @param string character vector coercible to a date using \code{\link[anytime]{anytime}}
#'     or a date time using \code{\link[anytime]{anytime}}; a numeric vector; or an
#'     integer vector
#'
#' @return a numeric vector with a single digit (integer from 0 - 9)
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Numerology.com, "Number 9 Meaning", \url{https://www.numerology.com/articles/about-numerology/single-digit-number-9-meaning/}.
#'    \item GeeksforGeeks, Last updated on 13 Jun, 2022, "Finding sum of digits of a number until sum becomes single digit", \url{https://www.geeksforgeeks.org/finding-sum-of-digits-of-a-number-until-sum-becomes-single-digit/}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 18 November 2022, "Digital root", \url{https://en.wikipedia.org/wiki/Digital_root}.
#' }
#'
#'
#'
#'
#'
#'
#'
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
#' # Examples
#'
#' library(iemisc)
#'
#' reduce_single_digit(37)
#' 
#' reduce_single_digit(5094322.439344993211394)
#' 
#' reduce_single_digit(-438443.349435493)
#' 
#' reduce_single_digit("-48373744582.47362287482374")
#' 
#' reduce_single_digit("11-09-2022")
#' 
#' reduce_single_digit("24 December 1983 04:37:58.55543333")
#' 
#' reduce_single_digit("4 July 1776")
#' 
#' reduce_single_digit(9)
#' 
#' reduce_single_digit(0)
#' 
#' reduce_single_digit(94321155)
#' 
#' reduce_single_digit("011 (704) 904-0432")
#' 
#' reduce_single_digit("011-894-908-0945")
#' 
#' reduce_single_digit("908-0945")
#' 
#'
#'
#'
#'
#'
#'
#' @importFrom stringi stri_replace_all_regex stri_count_fixed stri_split_fixed
#' @importFrom anytime anytime anydate
#' @importFrom methods is
#' @importFrom mgsub mgsub
#'
#' @export
reduce_single_digit <- function (string) {


if (is(string, "numeric")) {

# to make any negative number positive
string1 <- abs(string)


} else if (is(string, "character")) {

strings <- mgsub(string, pattern = c("-", "/", "(", ")"), replacement = c("", "", "", ""), fixed = TRUE)


if (!is.na(anydate(strings))) {

if (stri_count_fixed(strings, " ") == 2) {

string1 <- anydate(strings)


} else if (stri_count_fixed(strings, " ") == 3) {

string1 <- anytime(strings)

}

} else if (is.na(anytime(strings))) {

string1 <- mgsub(strings, pattern = c(" ", "-"), replacement = c("", ""), fixed = TRUE)

string1 <- as.numeric(string1)


if (stri_detect_fixed(string1, ".")) {

string1 <- unlist(stri_split_fixed(string1, pattern = ".", n = 2))

string1 <- as.numeric(paste0(string1, collapse = ""))

}
}
}



if(is(string1, "Date")) {

string1a <- mgsub(as.character(string1), "-", "")


} else if (is(string1, "POSIXct")) {

string1a <- stri_replace_all_regex(string1, "A-Z", "", case_insensitive = TRUE)

string1a <- mgsub(string1a, pattern = c("-", ":", " "), replacement = c("", "", ""), fixed = TRUE)


} else if (is(string1, "numeric")) {

string1a <- string1

}



if (!is.numeric(string1a)) {

stringer <- as.numeric(mgsub(string1a, " ", "", fixed = TRUE))


} else if (is.numeric(string1a)) {

if (stri_detect_fixed(string1a, ".")) {

string1b <- unlist(stri_split_fixed(string1a, pattern = ".", n = 2))

string1c <- as.numeric(paste0(string1b, collapse = ""))

stringer <- string1c


} else if (!stri_detect_fixed(string1a, ".")) {

stringer <- as.numeric(string1a)

}
}



if(stringer == 0) {

stringer <- 0

return(stringer)


} else if (Mod_octave(stringer, 9) == 0) {

stringer <- 9

return(stringer)


} else if (Mod_octave(stringer, 9) != 0) {

stringer <- Mod_octave(stringer, 9)

return(stringer)

}
}
