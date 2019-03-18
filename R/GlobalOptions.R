#' Get Language folder
#'
#' @return Returns the folder, where language files are stores. Should be specified with options(abvdLanguageFolder = "~/path/to/my/languages")
#' @export
getLanguageFolder <- function() {
  if(!is.null(getOption("abvdLanguageFolder"))) {
    folder <- getOption("abvdLanguageFolder")
  } else {
    stop("Need to specify a language folder in the global options! options(abvdLanguageFolder = FOLDER)")
  }
  return(folder)
}



#' Get Word list
#' 
#' word list must be manually downloaded once and then specified with options(abvdWordList="~/path/to/wordlist.csv")
#'
#' @return returns path to wordlist.csv
#' @export
getWordList <- function() {
  if(!is.null(getOption("abvdWordList"))) {
    return(read.csv(getOption("abvdWordList")))
  } else {
    stop("Need to specify a csv-file containing a word list in the global options! options(abvdWordList = WORDLIST.csv)")
  }
}