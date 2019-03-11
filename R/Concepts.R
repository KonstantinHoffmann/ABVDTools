getConcept <- function(id) {
  concept <- gsub(pattern = "/", replacement = "_", x = as.character(getWordList()[id,2]))
  concept <- gsub(pattern = " ", replacement = "", x = concept)
  return(concept)
}

getConceptList <- function() {
  concepts <- character(210)
  for (i in 1:210) {
    concepts[i] <- getConcept(i)
  }
  return(concepts)
}