#' Normalized Levenshtein distance
#'
#' Levenshtein distance of two strings is the minimal amount of edits (insertions, deletions and substitutions) to go from string1 to string2. This is normalized by the length of the longer string.
#'
#' @param string1 First string of comparison
#' @param string2 Second string of comparison
#' @param ignoreCharacter Optional, character vector of symbols that will be ignored and removed from the string.
#'
#' @return Returns numeric value of the normalized Levenshtein distance
#' @export
normalizedLevenshtein <- function (string1, string2, ignoreCharacter=NULL, ...) {
  if (!is.null(ignoreCharacter)) {
    for(i in 1:length(ignoreCharacter)) {
      string1<-gsub(ignoreCharacter[i], "", string1)
      string2<-gsub(ignoreCharacter[i], "", string2)
    }
  }
  return(utils::adist(string1, string2)/max(nchar(string1), nchar(string2)))
}

#' Determines if two strings are similar
#'
#' @param string1 first string
#' @param string2 second string
#' @param threshold Threshold for similarity in terms of the normalized Levenshtein distance
#'
#' @return Returns TRUE, if the distance is lower or equal the threshold
#' @export
areSimilar <- function(string1, string2, threshold, ...) {
  if(normalizedLevenshtein(string1, string2, ...)<=threshold) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}