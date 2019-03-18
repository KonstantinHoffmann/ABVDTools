#' Read Language
#'
#' @param id Id of the language
#' @param download if TRUE, automatically download the language files, if they cannot be found
#'
#' @return Returns a data-frame of the wordlist
#' @export
readLanguage <- function(id, download=TRUE) {
  return(read.csv(getWordFile(id, download)))
}

#' Reads multiple languages
#'
#' @param ids numerical vector of language-ids
#' @return Returns a list of language-dataframes.
#' @export
#' @describeIn readLanguage Reads multiple languages and outputs them as a list
readMultipleLanguages <- function(ids, download=TRUE) {
  out <- list()
  for (i in 1:length(ids)) {
    out[[i]] <- readLanguage(ids[i], download)
  }
  return(out)
}

#' Get wordlist file of a language
#'
#' @param id Id of the language
#' @param download if TRUE, automatically download the language files, if they cannot be found
#'
#' @return Returns the path of a wordlist.
#' @export
getWordFile <- function(id, download=TRUE) {
  files <- dir(getLanguageFolder())
  files<-files[(startsWith(files, paste(as.character(id), "-", sep="")) & endsWith(files, "-words.csv"))]
  if(length(files) >=1) {
    return(paste(getLanguageFolder(), "/", files[1], sep=""))
  }
  else if(download==TRUE) {
    download_abvd(id)
    files <- dir(getLanguageFolder())
    files<-files[(startsWith(files, paste(as.character(id), "-", sep="")) & endsWith(files, "-words.csv"))]
    if(length(files) >=1) {
      return(paste(getLanguageFolder(), "/", files[1], sep=""))
    }
  }
  else if(download==FALSE) {
    stop(paste("Couldn't retrieve word file for language", id))
  }
}

#' Get language name
#'
#' @param id id of the language
#' @param ... further arguments passed to getWordFile, e.g. if language should be downloaded
#'
#' @return Returns name of the language. Empty spaces and commas are stripped.
#' @export
getLanguageName <- function(id, ...) {
  name <- basename(getWordFile(id, ...))
  name <- stringr::str_split(string = name, pattern = paste(id,"-", sep=""))[[1]][2]
  name <- stringr::str_split(string = name, pattern = "-words.csv")[[1]][1]
  name <- gsub(",", "", name)
  name <- gsub(" ", "", name)
  return(name)
}