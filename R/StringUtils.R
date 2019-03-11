normalizedLevenshtein <- function (string1, string2) {
  return(utils::adist(string1, string2)/max(nchar(string1), nchar(string2)))
}

areSimilar <- function(string1, string2, threshold) {
  if(normalizedLevenshtein(string1, string2)<=threshold) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}