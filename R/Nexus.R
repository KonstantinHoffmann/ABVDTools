#' Create Nexus file for phylogenetic analyses
#'
#' @param languages Language ids that should be included
#' @param file output-file
#' @param words word-id that should be included as numeric vector (default is 1:210, which is all words)
#' @param sortBins if TRUE, charsets are sorted by their size
#' @param ... further arguments passed to createAlignmentMatrix
#' @export
createNexusFile <- function(languages, file, words=1:210, sortBins=FALSE, ...) {
  if(file.exists(file)) {
    file.remove(file)
  }
  file.create(file)
  data <- createAlignmentMatrix(languages, words, ...)
  cat("#NEXUS\n", file = file, append=TRUE)
  cat("\n", file = file, append=TRUE)
  cat("BEGIN DATA;\n", file = file, append=TRUE)
  cat("\t DIMENSIONS NTAX=",length(languages), " NCHAR=", length(data$matrix[1,]), ";\n", file = file, append=TRUE, sep="")
  cat("\t FORMAT DATATYPE=STANDARD MISSING=? GAP=-  SYMBOLS=\"01\";\n", file = file, append=TRUE, sep="")  
  cat("MATRIX\n", file = file, append=TRUE)
  for (i in 1:length(languages)) {
    cat(languages[i], "-", getLanguageName(languages[i]), file = file, append=TRUE, sep="")
    cat("\t\t\t\t\t", file = file, append=TRUE)
    cat(paste(data$matrix[i,], collapse=""), file = file, append=TRUE)
    cat("\n", file = file, append=TRUE)
  }
  cat(";\n", file = file, append=TRUE)
  cat("END;\n", file = file, append=TRUE)
  cat("begin assumptions;\n", file = file, append=TRUE)
  if(sortBins) {
    sorting <- order(data$charsetTo-data$charsetFrom)
    words <- words[sorting]
    data$charsetTo <- data$charsetTo[sorting]
    data$charsetFrom <- data$charsetFrom[sorting]
  }
  for (i in 1:length(words)) {
    cat("\t charset ", getConcept(words[i]), " = ", data$charsetFrom[i], "-", data$charsetTo[i], ";\n", file = file, append=TRUE, sep="")
  }
  cat("end;\n", file = file, append=TRUE)
}