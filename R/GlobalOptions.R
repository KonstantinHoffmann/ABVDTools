#' Get Language folder
#'
#' @return Returns the folder, where language files are stores. Should be specified with options(abvdLanguageFolder = "~/path/to/my/languages")
#' @export
getLanguageFolder <- function() {
  if(!is.null(getOption("abvdLanguageFolder"))) {
    folder <- getOption("abvdLanguageFolder")
  } else {
    folder <- paste(getwd(), "languages", sep="/")
    if(!dir.exists(folder)) {
      dir.create(folder)
    }
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
    return(read.csv(system.file("words.csv", package = "ABVDTools")))
}