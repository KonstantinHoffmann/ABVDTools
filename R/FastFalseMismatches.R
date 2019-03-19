
#' False cognate assignments of all languages
#'
#' Detects all pairs of lexemes that are similar, but have different cognate coding. This may take a while for a lot of languages
#'
#' @param language_ids IDs of all languages in comparison
#' @param threshold Threshold, when lexemes are classified as similar. Normalized levensthein-distance, see \code{\link{normalizedLevensthein}}.
#' @param silent If progress should be pasted
#' @param ... further arguments passed to normalizedLevenshtein
#'
#' @return Returns a data.frame of all lexemes that are suspicious
#' @export
getAllFalseMismatches <- function(language_ids, threshold = 0.34, silent=TRUE, ...) {
  # Initialize
  if(!silent) cat("Start initialization\n")
  languages <- readMultipleLanguages(language_ids)
  concepts <- getConceptList()
  languageLexemes <- list()
  for (i in 1:length(languages)) {
    languageLexemes[[i]] <- list()
    for (j in 1: length(concepts)) {
      languageLexemes[[i]][[j]] <- languages[[i]][languages[[i]]$word_id==j, c(2,4,7)] # columns: word_id, item, cognacy
    }
  }
  
  output <- data.frame(matrix(ncol=8, nrow=0))
  colnames(output) <- c("Language1", "Language2", "word_id", "word", "Lexeme1", "Lexeme2", "Cognagy1", "Cognacy2") 
  rowcount <- 1
  if(!silent) cat("Finished initialization\n")
  # Do the loop
  for (i in 1: (length(languages)-1)) {
    if(!silent) cat("Looking for false mismatches:", i, "/", length(languages)-1, "\n")
    for (j in (i+1) : length(languages)) {
      for (k in 1:210) {
        if (length(languageLexemes[[i]][[k]][,1])==0 | length(languageLexemes[[j]][[k]][,1])== 0) {
          next()
        }
        for (l in 1:min(length(languageLexemes[[i]][[k]][,1]), length(languageLexemes[[j]][[k]][,1]))) {
          if (areSimilar(
            string1=as.character(languageLexemes[[i]][[k]][l,2]),
            string2=as.character(languageLexemes[[j]][[k]][l,2]),
            threshold=threshold, 
            ...
            )) {
              c1 <- gsub(" ", "", as.character(languageLexemes[[i]][[k]][l,3]))
              c2 <- gsub(" ", "", as.character(languageLexemes[[j]][[k]][l,3]))
              if (c1!=c2) {
                output[rowcount,] <- c(language_ids[i], language_ids[j], k, concepts[k], as.character(languageLexemes[[i]][[k]][l,2]), as.character(languageLexemes[[j]][[k]][l,2]), as.character(languageLexemes[[i]][[k]][l,3]), as.character(languageLexemes[[j]][[k]][l,3]))
                rowcount <- rowcount+1
              }
            }
        }
      }
    }
  }
  if(!silent) cat("Finished, found", length(output[,1]), "false mismatches\n")
  return(output)
}