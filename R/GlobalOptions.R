getLanguageFolder <- function() {
  if(!is.null(getOption("abvdLanguageFolder"))) {
    folder <- getOption("abvdLanguageFolder")
  } else {
    stop("Need to specify a language folder in the global options! options(abvdLanguageFolder = FOLDER)")
  }
  return(folder)
}

getWordList <- function() {
  if(!is.null(getOption("abvdWordList"))) {
    return(read.csv(getOption("abvdWordList")))
  } else {
    stop("Need to specify a csv-file containing a word list in the global options! options(abvdWordList = WORDLIST.csv)")
  }
}