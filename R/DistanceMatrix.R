#' Calculate a distance matrix of an alignment
#'
#' Create a matrix of pariwise distances between languages. Takes as input eighter a set of languages or an alignment matrix
#' 
#' @param languages ids of the languages
#' @param alignment alignment matrix consisting of 0, 1, ?. If alignment=NULL, then it will be created with the language IDs.
#' @param normalized should the distance be divided by the length of the sequence
#' @param silent report progress or not
#' @param ... further arguments
#' 
#' @details Optional arguments are: 
#' \itemize{
#' \item removeMissings: (default TRUE): removes all ? in the sequence and only considers entries, where both languages in comparison have data 
#' \item removeZeroes: (default: FALSE): if TRUE, only consider entries, where at least one of the two languages have a 1 
#'}
#'
#' @return a matrix of pairwise distances
#' @export
#'
distanceMatrix <- function(languages=NULL, alignment=NULL, normalized=TRUE, silent=FALSE, ...) {
  if(is.null(alignment)) {
    if(is.null(languages)) stop("Error calculation the distance matrix: Eighter languages or alignment has to be specified!")
    alignment <- createAlignmentMatrix(languages, silent = silent, ...)
  }
  rows <- length(alignment$matrix[,1])
  out <- matrix(nrow = rows, ncol = rows, byrow = TRUE)
  for (i in 1:rows) {
    if(!silent) {
      cat(paste("Calculating distance matrix: ", i, "/", rows, "\n", sep=""))
    }
    for (j in 1:i) {
      if(i==j) {
        out[i,j] <- 0
      } else {
        out[i, j] <- pairwiseDistance(alignment$matrix[i,], alignment$matrix[j,], normalized, ...)
        out[j, i] <- out[i,j]
      }
    }
  }
  return(out)
}


#' Title
#'
#' @param x 
#' @param y 
#' @param normalized 
#' @param removeMissing 
#' @param removeZeroes 
#'
#' @return
#' 
#'
pairwiseDistance <- function(x,y, normalized=TRUE, removeMissing=TRUE, removeZeroes=FALSE) {
  l <- length(x)
  if (l!=length(y)) {
    stop("Error calculating the Hamming distance: Vectors differ in length")
  }
  if(removeMissing) {
    missing <- grepl("\\?", x) | grepl("\\?", y)
    x<-x[!missing]
    y<-y[!missing]
  }
  if(removeZeroes) {
    zeroes <- grepl("0", x) & grepl("0", y)
    x<-x[!zeroes]
    y<-y[!zeroes]
  }
  if (normalized) {
    return (sum(x!=y)/length(x))
  }
  else {
    return (sum(x!=y))
  }
}

#' Distance of two languages
#' 
#' Is not that fast...
#'
#' @param id1 ID of first language
#' @param id2 ID of second language
#' @param normalized should the distance be divided by the length of the sequence
#' @param ... further Arguments
#' 
#' @details Optional arguments are: 
#' \itemize{
#' \item words (default 1:210): which words should be included in the alignment
#' \item normalized: (default TRUE): 
#' \item removeMissings: (default TRUE): removes all ? in the sequence and only considers entries, where both languages in comparison have data 
#' \item removeZeroes: (default: FALSE): if TRUE, only consider entries, where at least one of the two languages have a 1 
#'}
#'
#' @return the distance between language1 and language2
#' @export
#'
getDistance <- function(id1, id2, normalized=TRUE, ...) {
  alignment <- createAlignmentMatrix(c(id1, id2), silent=TRUE, ...)
  return(distanceMatrix(alignment=alignment, silent=TRUE, ...)[1,2])
}