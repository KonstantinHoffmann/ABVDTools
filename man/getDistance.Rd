% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DistanceMatrix.R
\name{getDistance}
\alias{getDistance}
\title{Distance of two languages}
\usage{
getDistance(id1, id2, normalized = TRUE, ...)
}
\arguments{
\item{id1}{ID of first language}

\item{id2}{ID of second language}

\item{normalized}{should the distance be divided by the length of the sequence}

\item{...}{further Arguments}
}
\value{
the distance between language1 and language2
}
\description{
Is not that fast...
}
\details{
Optional arguments are: 
\itemize{
\item words (default 1:210): which words should be included in the alignment
\item normalized: (default TRUE): 
\item removeMissings: (default TRUE): removes all ? in the sequence and only considers entries, where both languages in comparison have data 
\item removeZeroes: (default: FALSE): if TRUE, only consider entries, where at least one of the two languages have a 1 
}
}
