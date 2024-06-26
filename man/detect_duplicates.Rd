% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_functions.R
\name{detect_duplicates}
\alias{detect_duplicates}
\title{Detect Duplicate Records}
\usage{
detect_duplicates(.base_table, .base_key, .threshold, .weights = NULL)
}
\arguments{
\item{.base_table}{A data table containing the base records, with tokens and columns.}

\item{.base_key}{A character string specifying the key column in the base table.}

\item{.threshold}{A numeric value specifying the minimum identification potential required to consider a match.}

\item{.weights}{A named numeric vector specifying the weights for each column. If NULL, equal weights are used.}
}
\value{
A data.table containing the groups of duplicate records with their identification potential.
}
\description{
This function detects duplicate records within a base table using token-based heuristic linkage.
It calculates the identification potential for each pair of records based on the rarity of shared tokens and specified weights.
}
\examples{
likely_duplicates <- detect_duplicates(
  .base_table = search_table_base,
  .base_key = "key_base",
  .threshold = 0.8
)
}
