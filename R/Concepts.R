
#' Get Concept
#'
#' @param id Id of the concept in the abvd-database
#'
#' @return returns a string with the name of the concept
#' @export
getConcept <- function(id) {
  concept <- gsub(pattern = "/", replacement = "_", x = as.character(getWordList()[id,2]))
  concept <- gsub(pattern = " ", replacement = "", x = concept)
  return(concept)
}


#' Get Concept-list
#'
#' @return returns a character vector of all concept-names (not a list!)
#' @export
getConceptList <- function() {
  concepts <- character(210)
  for (i in 1:210) {
    concepts[i] <- getConcept(i)
  }
  return(concepts)
}