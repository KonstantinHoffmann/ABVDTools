% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Alignment.R
\name{parseCognates}
\alias{parseCognates}
\title{Parse cognates out of an item}
\usage{
parseCognates(language, wordID, removeLoans = TRUE,
  UncertaintyAsUniques = FALSE, removeUncertainties = FALSE,
  missingsAsUniques = TRUE)
}
\arguments{
\item{language}{The id of the language}

\item{wordID}{The id of the word (1-210)}

\item{removeLoans}{if TRUE, returns an empty numeric(0), if the lexeme is known to be loaned}

\item{UncertaintyAsUniques}{if TRUE, every ? in the cognate will be treated as unique column}

\item{removeUncertainties}{if TRUE, returns an empty numeric(0), if the cognate assignment has a ?}

\item{missingsAsUniques}{if TRUE, every empty cognate will be treated as unique column. If FALSE, an empty cognate-field will be treated as missing data.}
}
\value{
returns a numeric vector of cognate classes for the given wordID and languageID. Returns -1, if the lexeme requires a unique column. Returns numeric(0), if data is missing.
}
\description{
Parse cognates out of an item
}
