#' Create a comparable word list of two languages
#'
#' @param lang1 First language (ID or data-frame generated from a csv-fiel via \code{\link{read.csv}})
#' @param lang2 Second language
#' @param concepts Optional input. Character-vector of all 210 concepts. Can additionally be provided to speed things up
#'
#' @return Returns a dataframe with all lexemes sorted by meaning-class-ID with their corresponding cognate classes.
twoLanguageWordlist <- function(lang1, lang2, concepts=NULL, ...) {
  if(class(lang1)=="numeric") lang1<-readLanguage(lang1)
  if(class(lang2)=="numeric") lang2<-readLanguage(lang2)
  if(is.null(concepts)) concepts <- getConceptList()
  output <- list()
  for (i in 1:210) {
    lang1.i <- lang1[lang1$word_id==i,]
    lang2.i <- lang2[lang2$word_id==i,]
    for (j in 1:max(length(lang1.i[,1]), length(lang2.i[,1]))) {
      if(max(length(lang1.i[,1]), length(lang2.i[,1]))!=0) {
        output$word_id <- append(output$word_id, i)
        output$concept <- append(output$concept, concepts[i])
        output$item1 <- append(output$item1, as.character(lang1.i$item[j]))
        output$item2 <- append(output$item2, as.character(lang2.i$item[j]))
        output$cognacy1 <- append(output$cognacy1, as.character(lang1.i$cognacy[j]))
        output$cognacy2 <- append(output$cognacy2, as.character(lang2.i$cognacy[j]))
      }
    }
  }
  return(data.frame(output))
}


#' False Mismatches of two languages 
#'
#' Compares lexemes of two languages and returns a list of similar lexemes that have different cognate classes.
#' This is quite slow, for mass-analysis, use \code{\link{getAllFalseMismatches}}.
#'
#' @param lang1 ID of first language
#' @param lang2 ID of second language
#' @param threshold Threshold, when lexemes are classified as similar. Normalized levensthein-distance, see \code{\link{normalizedLevenshtein}}.
#' @param ... further arguments passed to twoLanguageWordList, e.g. a concept-list to speed things up or to normalizedLevenshtein
#'
#' @return Returns a data-frame with all pairs of lexemes included, where a false cognate class assignment is assumed
#' @export
#' @seealso \code{\link{getAllFalseMismatches}}
getFalseMismatches <- function(lang1, lang2, threshold=0.4, ...) {
  if(class(lang1)=="numeric") lang1<-readLanguage(lang1)
  if(class(lang2)=="numeric") lang2<-readLanguage(lang2)  
  wordlist <- twoLanguageWordlist(lang1, lang2, ...)
  falseMismatches <- logical(length(wordlist[,1]))
  for (i in 1:length(wordlist[,1])) {
    if(!(TRUE %in% is.na(wordlist[i, 3:6]))) {
      # remove white-space in cognacy
      cognacy1 <- gsub(" ", "", as.character(wordlist[i,5]))
      cognacy2 <- gsub(" ", "", as.character(wordlist[i,6]))
      if(areSimilar(as.character(wordlist[i, 3]), as.character(wordlist[i,4]), threshold, ...) & cognacy1 != cognacy2) {
        falseMismatches[i] <- TRUE
      }
    }
  }
  return(wordlist[falseMismatches,])
}


#' Amount of false mismatches of given languages (Deprecated)
#'
#' This is too slow, dont use! Use \code{sum\link{getAllFalseMismatches})}
#'
#' @param languages Languages to compare as numerical vector of ids
#' @param threshold Threshold, when lexemes are classified as similar. Normalized levensthein-distance, see \code{\link{normalizedLevenshtein}}. 
#' @export
sumFalseMismatches <- function(languages, threshold=0.4) {
  result <- 0
  if(class(languages)=="numeric") languages <- readMultipleLanguages(languages)
  concepts <- getConceptList()
  for (i in 1:(length(languages)-1)) {
    for (j in (i+1):length(languages)) {
      result <- result + length(getFalseMismatches(languages[[i]], languages[[j]], threshold, concepts=concepts)[,1])
      print(paste(i, ", ", j, " Result: ", result))
    }
  }
  return(result)
}
