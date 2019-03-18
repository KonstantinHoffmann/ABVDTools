#' Download from ABVD-Database
#'
#' @param id id of the language that is to be downloaded
#' @param folder destination folder of the download. Optional, if default folder in options(abvdLanguageFolder) is specified.
#'
#' @export
download_abvd <- function(id, folder=NULL) {
  if (is.null(folder)) folder <- getLanguageFolder()
  destfile <- paste(folder, "/", id, ".csv", sep="")
  res <- try(download.file(paste("https://abvd.shh.mpg.de/utils/save/?type=csv&section=austronesian&language=",id, sep=""), destfile=destfile))
  if(class(res)=="try-error") {
    stop("Error downloading file. Stopped execution")
  }
  name<-NULL
  try(name <- gsub(" ", "_", read.csv(destfile)$language[1]))
  if(is.null(name)) {
    file.remove(destfile)
    stop("Could not retrieve language name. Does the language-id exist? Deleted downloaded files")
  }
  file.rename(destfile, paste(folder, "/", id, "-", name, ".csv", sep=""))
  content <- readLines(con = paste(folder, "/", id, "-", name, ".csv", sep=""))
  firstline <- NULL
  for (i in 1:100) {
    if(startsWith(content[i], "id,word_id")) {
      firstline <- i
      break
    }
  }
  if(!is.null(firstline)) {
    writeLines(content[-c(1:firstline-1)], con = paste(folder, "/", id, "-", name, "-words.csv", sep=""))
  } else {
    warning("Could not create words-file: Could not find the first line of the wordlist")
  }
}