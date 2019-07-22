#' Get language name
#'
#' @param id id of the language
#' @param ... further arguments passed to getWordFile, e.g. if language should be downloaded
#'
#' @return Returns name of the language. Empty spaces and commas are stripped.
#' @export
getLanguageName <- function(id, ...) {
  name <- as.character(read.csv(getLanguageFile(id, ...))$language[1])
  name <- gsub(",", "", name)
  name <- gsub(" ", "", name)
  return(name)
}


#' Get the classification clade of a language
#'
#' @param id id of the language
#' @param ... further arguments passed to getWordFile, e.g. if language should be downloaded
#'
#' @return Returns the classification of the language as string.
#' @export
getClade <- function(id, ...) {
   return(as.character(read.csv(getLanguageFile(id, ...))$classification[1]))
}

#' Get the author of a doculect
#'
#' @param id id of the language
#' @param ... further arguments passed to getWordFile, e.g. if language should be downloaded
#'
#' @return Returns the author as string.
#' @export
getAuthor <- function(id, ...) {
  return(as.character(read.csv(getLanguageFile(id, ...))$author[1]))
}

#' Get the glottocode of a doculect
#'
#' @param id id of the language
#' @param ... further arguments passed to getWordFile, e.g. if language should be downloaded
#'
#' @return Returns the glottocode as string.
#' @export
getGlottocode <- function(id, ...) {
  return(as.character(read.csv(getLanguageFile(id, ...))$glottocode[1]))
}
