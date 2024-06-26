% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_preparers.R
\name{word_tokens}
\alias{word_tokens}
\title{Return a list of word tokens for the .text separated by spaces.}
\usage{
word_tokens(.text, .min_length = 0)
}
\arguments{
\item{.text}{A character string from which words will be extracted.}
}
\value{
Returns a vector of words extracted from the input text.
}
\description{
This function splits the input text into words based on spaces. It returns a vector of the words
found in the text. This function is useful for natural language processing tasks where word-level
manipulation of text is required.
}
\examples{
word_tokens("This is an example.")
word_tokens("Another, test; string.")
}
