% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DistanceMatrix.R
\name{distanceMatrix}
\alias{distanceMatrix}
\title{Calculate a distance matrix of an alignment}
\usage{
distanceMatrix(languages = NULL, alignment = NULL, type = "Hamming",
  silent = FALSE, ...)
}
\arguments{
\item{languages}{ids of the languages}

\item{alignment}{alignment matrix consisting of 0, 1, ?. If alignment=NULL, then it will be created with the language IDs.}

\item{type}{one of "Hamming" or "Jaccard"}

\item{silent}{report progress or not}

\item{...}{further arguments}
}
\value{
a matrix of pairwise distances
}
\description{
Create a matrix of pariwise distances between languages. Takes as input eighter a set of languages or an alignment matrix
}
\details{
Optional arguments are: 
\itemize{
\item removeMissings: (default TRUE): removes all ? in the sequence and only considers entries, where both languages in comparison have data 
\item removeZeroes: (default: FALSE): if TRUE, only consider entries, where at least one of the two languages have a 1 
}
}
