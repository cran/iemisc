#' Reduce an Integer Or a Date to a Single Integer
#'
#' Takes a character vector coercible to a date using \code{\link[anytime]{anydate}},
#' a numeric vector, or an integer vector & computes the sum to a single digit.
#' The vectors may include dashes, parentheses, and/or spaces. See the examples.
#'
#'
#' @param string character vector coercible to a date using \code{\link[anytime]{anydate}},
#'     a numeric vector, an integer vector
#'
#'
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
#' library("iemisc")
#'
#' reduce_single_digit(37)
#' 
#' reduce_single_digit("11-09-2022")
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
#' @importFrom anytime anydate
#' @importFrom methods is
#' @importFrom mgsub mgsub
#'
#' @export
reduce_single_digit <- function (string) {


if (is(string, "numeric")) {

string1 <- string


} else if (is(string, "character")) {

strings <- mgsub(string, pattern = c("-", "/", "(", ")"), replacement = c("", "", "", ""), fixed = TRUE)

ifelse(!is.na(anydate(strings)), string1 <- anydate(strings), string1 <- as.numeric(mgsub(strings, " ", "")))

}


if(is(string1, "Date")) {

string1 <- mgsub(as.character(string1), "-", "")

}


ifelse (!is.numeric(string1), stringer <- as.numeric(mgsub(string1, " ", "")), stringer <- string1)


if(stringer == 0) {

stringer <- 0

return(stringer)


} else if (Mod_octave(stringer, 9) == 0) {

stringer <- 9

return(stringer)


} else {

stringer <- Mod_octave(stringer, 9)

return(stringer)

}
}
