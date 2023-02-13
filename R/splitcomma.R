#' Split Comma
#'
#' This function takes a character string in the form of Last Name (Surname),
#' First Name or Second String, First String and transposes that string to
#' First Name Last Name (Surname) or First String Second String while removing
#' the comma.
#'
#'
#' @param string character vector that contains a phrase separated by a comma
#'    or not. If not, there is no change.
#'
#'
#'
#'
#' @return the character \code{\link[base]{vector}} in the form of First Name Last Name
#'
#'
#'
#'
#'
#' @source
#' \enumerate{
#'    \item regex - Split on first comma in string - Stack Overflow answered by flodel on Apr 25 2012. See \url{https://stackoverflow.com/questions/10309122/split-on-first-comma-in-string}.
#'    \item regex - r regexp - replace title and suffix in any part of string with nothing in large file (&gt; 2 million rows) - Stack Overflow answered by Molx on Apr 16 2015. See \url{https://stackoverflow.com/questions/29680131/r-regexp-replace-title-and-suffix-in-any-part-of-string-with-nothing-in-large}.
#' }
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
#' @examples
#'
#' # Example
#' 
#' install.load::load_package("iemisc", "data.table")
#' 
#' dtxx <- data.table(Names = c("Cooler, Wine", "Juice, Fruit", "Hard Water",
#' "Hot Bath", "Set, Data"))
#' 
#' dtxx[, Corrected_Names := splitcomma(dtxx$Names)]
#' 
#' dtxx
#' 
#'
#' 
#' 
#' 
#' @importFrom stringi stri_split_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate testCharacter
#'
#' @export
splitcomma <- function(string) {

# Check string
assert_that(!any(testCharacter(string, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

# Look for a comma in the name column and for those rows create a new column called names that splits the original name in 2 strings and reorder the strings staring with the 2nd string then the 1st string

ifelse (stri_detect_fixed(string, pattern = ", "), sapply(stri_split_fixed(string, pattern = ", ", n = 2), function(st) paste(st[2], st[1], collapse = " "), USE.NAMES = FALSE), string)

}











#' Split Remove
#'
#' This function removes characters from a string based on a character vector
#' named remove. This function can be used to remove prefixes, suffixes,
#' titles, etc. from a given character vector. The function splits the string
#' by empty spaces, dots, commas, and parentheses first & then it removes the
#' items that are in the remove vector.
#'
#'
#'
#' @param string character vector that contains the text to keep and to remove
#' @param remove character vector that contains the characters to remove from
#'    the string
#'
#'
#'
#' @return the revised character \code{\link[base]{vector}} with the contents of
#'    remove removed from the string 
#'
#'
#'
#'
#'
#' @source
#' regex - r regexp - replace title and suffix in any part of string with nothing in large file (&gt; 2 million rows) - Stack Overflow answered by Molx on Apr 16 2015. See \url{https://stackoverflow.com/questions/29680131/r-regexp-replace-title-and-suffix-in-any-part-of-string-with-nothing-in-large}.
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
#' @examples
#'
#' # Example
#' 
#' install.load::load_package("iemisc", "data.table")
#'
#' # create the list of items to remove from the text
#' remove <- c("mister", "sir", "mr", "madam", "mrs", "miss", "ms", "iv",
#' "iii", "ii", "jr", "sr", "md", "phd", "mba", "pe", "mrcp", "and", "&", "prof",
#' "professor", "esquire", "esq", "dr", "doctor")
#' 
#' names <- data.table(Named = c("Alfredy 'Chipp' Kahner IV",
#' "Denis G. Barnekdt III", "JERUEG, RICHARDS Z. MR.", "EDWARDST, HOWARDD K. JR."))
#' 
#' # first use split comma
#' names[, Corrected_Named := splitcomma(names$Named)]
#' 
#' names
#' 
#' names[, Corrected_Named := splitremove(names$Corrected_Named, remove)]
#' 
#' names
#' 
#'
#' 
#' 
#' 
#' 
#' @importFrom stringi stri_detect_regex stri_split_regex
#' @importFrom assertthat assert_that
#' @importFrom checkmate testCharacter
#' @importFrom qdapRegex rm_white
#' @importFrom mgsub mgsub
#'
#' @export
splitremove <- function(string, remove) {

remove <- remove

remover <- paste(remove, collapse = "|")


# Check string
assert_that(!any(testCharacter(string, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values with numbers and provide an error message if the check fails


# Split by empty spaces, dots, commas and parenthesis and then remove the items from remove
stringer <- ifelse (stri_detect_regex(string, remover, case_insensitive = TRUE), sapply(stri_split_regex(string, "\\s|\\.|\\,|\\(|\\)"), function(st) paste(st[!(st %qsin% remove)], collapse = " "), USE.NAMES = FALSE), string)

stringers <- rm_white(stringer)

stringerst <- mgsub(stringers, "( \\w{1}) ", " \\1. ")

stringersts <- rm_white(stringerst)

return(stringersts)
}













#' Quick Search
#'
#' This function performs a quick, case insensitive search of strings
#'
#'
#' @param string character vector that contains the values to match
#' @param vector character vector that has the values to be matched against
#'
#' @details 'Utilizes chin from data.table to quickly complete a case
#'    insensitive search through a character vector to return a logical vector
#'    of string detections. Will always return TRUE or FALSE for each position
#'    of string regardless of NA missing values in either provided vector. NA
#'    in string will never match an NA value in the vector.'
#'
#' @return logical vector the length of the original string
#'
#'
#' @source
#' data.table - R case-insensitive \%in\% - Stack Overflow answered and edited by Dylan Russell on Sep 13, 2020. See \url{https://stackoverflow.com/questions/63874824/r-case-insensitive-in}.
#'
#'
#'
#'
#'
#'
#' @author Dylan Russell (stackoverflow code, examples, and function details, function description), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @examples
#'
#' # Examples
#'
#' x <- c("apple","banana","cherry", NA)
#'
#' "apple" %qsin% x
#'
#' c("APPLE","BANANA", "coconut", NA) %qsin% x
#'
#'
#'
#'
#' @importFrom data.table %chin%
#' @importFrom stats na.omit
#' @importFrom mgsub mgsub
#'
#' @export
`%qsin%` <- function(string, vector) {

# Check string
#assert_that(!any(testCharacter(string, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

# Check vector
#assert_that(!any(testCharacter(vector, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

tolower(string) %chin% na.omit(tolower(vector))

}
