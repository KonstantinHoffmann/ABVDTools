
#' Parse cognates out of an item
#'
#' @param language The id of the language 
#' @param wordID The id of the word (1-210)
#' @param removeLoans if TRUE, returns an empty numeric(0), if the lexeme is known to be loaned
#' @param UncertaintyAsUniques if TRUE, every ? in the cognate will be treated as unique column
#' @param removeUncertainties if TRUE, returns an empty numeric(0), if the cognate assignment has a ?
#' @param missingsAsUniques if TRUE, every empty cognate will be treated as unique column. If FALSE, an empty cognate-field will be treated as missing data.
#'
#' @return returns a numeric vector of cognate classes for the given wordID and languageID. Returns -1, if the lexeme requires a unique column. Returns numeric(0), if data is missing.
#' @export 
parseCognates <- function(language, wordID, removeLoans=TRUE, UncertaintyAsUniques=FALSE, removeUncertainties=FALSE, missingsAsUniques=TRUE) {
  if(UncertaintyAsUniques && removeUncertainties) {
    stop("Error: UncertaintyAsUniques and removeUncertainties cannot both set to TRUE")
  }
  languageID <- language
  language <- read.csv(getWordFile(language))
  wordRows <- language[language$word_id==wordID,]
  toRemove <- logical(length(wordRows[,1]))
  if(removeLoans) {
    #toRemove <- grepl("L", wordRows$loan) | grepl("L\\?", wordRows$loan) | grepl("l", wordRows$loan) | grepl("l\\?", wordRows$loan)
    toRemove <- nchar(wordRows$loan)>0
  }
  if(removeUncertainties) {
    toRemove <- grepl("\\?", wordRows$cognacy)
  }
  toRemove <- toRemove | grepl("x", wordRows$cognacy) | grepl("X", wordRows$cognacy) | grepl("P", wordRows$cognacy)
  wordRows <- wordRows[!toRemove,]
  if(length(wordRows$id)==0) { # Word is not recorded
    return(numeric(0))
  }
  cognates <- paste(wordRows$cognacy, sep=",")
  if(TRUE %in% c(grepl("I", cognates), grepl("E", cognates))) { # I or E in cognates => remove word
    return(numeric(0))
  }
  cognates <- gsub(pattern = " ", replacement = "", x=cognates)
  cognates <- gsub(pattern = "a", replacement = "", x=cognates)
  cognates <- gsub(pattern = "b", replacement = "", x=cognates)
  cognates <- gsub(pattern = "\\+", replacement = "", x=cognates)
  cognates <- gsub(pattern = "\\*", replacement = "", x=cognates)
  if (!UncertaintyAsUniques) {
    cognates <- gsub(pattern = "\\?", replacement = "", x=cognates)
  }
  cognates <- unlist(stringr::str_split(cognates, ","))
  if (missingsAsUniques) {
    for (i in 1: length(cognates)) {
      if (cognates[i]=="" | is.na(cognates[i])) {
        cognates[i] <- "-1"
      }
    }
  }
  if (UncertaintyAsUniques) {
    for (i in 1: length(cognates)) {
      if (grepl("\\?", cognates[i])) {
        cognates[i] <- "-1"
      }
    }
  }
  
  tryCatch({
    cognates <- sort(unique(as.numeric(cognates)))
  }, warning=function(e) {
    warning("Language: ", languageID, ", Word: ", wordID)
  }, finally= {
    suppressWarnings({cognates <- sort(unique(as.numeric(cognates)))})
    })
  
  if (length(cognates)==0) {
    return(numeric(0))
  }
  else {
    return(cognates)
  }
}


#' Create an alignment-matrix of a meaning-class
#'
#' @param languages The language-IDs as a numeric vector
#' @param wordID the ID of the meaning-class (must be a single numeric, cannot process a vector of IDs)
#' @param ascertainment if TRUE, an ascertainment column is created at the beginning of the alignment
#' @param ... further arguments passed to parseCognates
#'
#' @return Returns a binary matrix. First column is ascertainment (if ascertainment is set to TRUE), further columns are cognate-columns and the last columns are singletons
#' @export
#' @seealso \code{\link{createAlignmentMatrix}}
createWordAlignment <- function(languages, wordID, ascertainment=TRUE, ...) {
  cognates <- list()
  uniques <- numeric(0)
  for (i in 1:length(languages)) {
    cognates[[i]] <- parseCognates(languages[i], wordID, ...)
    if(-1 %in% cognates[[i]]) {
      uniques[length(uniques)+1] <- i
    }
    cognates[[i]] <- cognates[[i]][cognates[[i]]!=-1]
  }
  allcognates <- sort(unique(unlist(cognates)))
  out <- matrix(nrow = length(languages), ncol=length(allcognates)+ascertainment+length(uniques))
  if (length(out[1,])==ascertainment) {
    return(-1)
  }
  for (i in 1:length(languages)) {
    if(length(cognates[[i]])==0 & !(i %in% uniques)) {
      out[i, ] <- "?"
    } else {
      if(ascertainment) {
        out[i,1] <- "0"
      }
      if(length(allcognates)>0) {
        for (j in (1+ascertainment):(length(allcognates)+ascertainment)) {
          if(allcognates[j-ascertainment] %in% cognates[[i]]) {
            out[i,j] <- "1"
          }
          else {
            out[i,j] <- "0"
          }
        }
      }
      if(length(uniques)>0) {
        for (j in (length(allcognates)+ascertainment+1):length(out[1,])) {
          out[i,j] <- "0"
          if (i %in% uniques) {
            if(uniques[j-ascertainment-length(allcognates)]==i) {
              out[i,j] <- "1"
            }
          }
        }
      }
    }
  }
  return(out)
}

#' Creates an alignment matrix for multiple meaning-classes
#'
#' @param languages numeric vector of language-IDs 
#' @param words numeric vector of meaning-class-ids
#' @param silent output progess in the console
#' @param ... further arguments passed to createWordAlignment
#'
#' @return returns a concatenated matrix of word-alignments
#' @export
#' @seealso \code{\link{createWordAlignment}}
createAlignmentMatrix <- function(languages, words=1:210, silent=FALSE,  ...) {
  wordMatrix <- list()
  charsetFrom <- numeric(length(words))
  charsetTo <- numeric(length(words))
  charsetFrom[1] <- 1
  for (i in 1:length(words)) {
    if(!silent) cat("Creating alignment matrix for word ", i, "\n")    
    wordMatrix[[i]] <- createWordAlignment(languages, words[i], ...)
    if(!is.null(dim(wordMatrix[[i]]))) {
      if(i!=1) {
        charsetFrom[i] <- charsetTo[i-1]+1
      }
      charsetTo[i] <- charsetFrom[i] + length(wordMatrix[[i]][1,]) - 1
    } else {
      charsetFrom[i] <- charsetTo[i-1]
      charsetTo[i] <- charsetFrom[i]
      warning("word ", i, " has no cognates")
    }
  }
  outMatrix <- wordMatrix[[1]]
  for (i in 2:length(words)) {
    if(!is.null(dim(wordMatrix[[i]]))) {
      outMatrix <- cbind(outMatrix, wordMatrix[[i]])
    }
  }
  return(list(matrix=outMatrix, charsetFrom = charsetFrom, charsetTo = charsetTo))
}