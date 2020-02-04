#' Pairwise shared Cognate distance
#' 
#' This calculates a distance based on the amount of shared cognates. Two languages share a cognate for a word
#' if they have at least one cognate code in common for that word (e.g. if language 1 has for the concept cognate "1, 2" and language 2
#' has "1, 3", they share a cognate). The distance is then the proportion of concepts, where these two languages do not share a cognate.
#'
#' @param lang1 row index of the first language in the alignment
#' @param lang2 row index of the second language in the alignment
#' @param alignment alignment matrix as created by createAlignmentMatrix()
#'
#' @return
#'
pairwiseSCDistance <- function(lang1, lang2, alignment) {
  conceptCount <- length(alignment$charsetFrom)
  isShared <- logical(conceptCount)
  isShared <- sapply(1:conceptCount, hasSharedCognates, lang1=lang1, lang2=lang2, alignment=alignment)
  return(1-sum(isShared, na.rm = TRUE)/sum(!is.na(isShared)))
}

hasSharedCognates <- function(word, lang1, lang2, alignment) {
  matrix <- alignment$matrix[c(lang1,lang2), alignment$charsetFrom[word]:alignment$charsetTo[word]]
  if(matrix[1,1]=="?" | matrix[2,1]=="?") return(NA)
  return(sum(matrix[1,]=="1" & matrix[2,]=="1")>=1)
}