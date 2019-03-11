readLanguage <- function(id, download=TRUE) {
  return(read.csv(getWordFile(id, download)))
}

readMultipleLanguages <- function(ids, download=TRUE) {
  out <- list()
  for (i in 1:length(ids)) {
    out[[i]] <- readLanguage(ids[i], download)
  }
  return(out)
}

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

getLanguageName <- function(id, ...) {
  name <- basename(getWordFile(id, ...))
  name <- stringr::str_split(string = name, pattern = paste(id,"-", sep=""))[[1]][2]
  name <- stringr::str_split(string = name, pattern = "-words.csv")[[1]][1]
  name <- gsub(",", "", name)
  name <- gsub(" ", "", name)
  return(name)
}