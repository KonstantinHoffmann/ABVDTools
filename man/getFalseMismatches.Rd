% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CompareLanguages.R
\name{getFalseMismatches}
\alias{getFalseMismatches}
\title{False Mismatches of two languages}
\usage{
getFalseMismatches(lang1, lang2, threshold = 0.4, ...)
}
\arguments{
\item{lang1}{ID of first language}

\item{lang2}{ID of second language}

\item{threshold}{Threshold, when lexemes are classified as similar. Normalized levensthein-distance, see \code{\link{normalizedLevenshtein}}.}

\item{...}{further arguments passed to twoLanguageWordList, e.g. a concept-list to speed things up or to normalizedLevenshtein}
}
\value{
Returns a data-frame with all pairs of lexemes included, where a false cognate class assignment is assumed
}
\description{
Compares lexemes of two languages and returns a list of similar lexemes that have different cognate classes.
This is quite slow, for mass-analysis, use \code{\link{getAllFalseMismatches}}.
}
\seealso{
\code{\link{getAllFalseMismatches}}
}
