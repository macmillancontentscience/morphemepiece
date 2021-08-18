% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{.mp_tokenize_single_string}
\alias{.mp_tokenize_single_string}
\title{Tokenize an Input Word-by-word}
\usage{
.mp_tokenize_single_string(words, vocab, lookup)
}
\arguments{
\item{words}{Character; a vector of words (generated by space-tokenizing a
single input).}

\item{vocab}{Named integer vector containing vocabulary words. Should have
"vocab_split" attribute, with components named "prefixes", "words",
"suffixes".}

\item{lookup}{A morphemepiece lookup table.}
}
\value{
A named integer vector of tokenized words.
}
\description{
Tokenize an Input Word-by-word
}
\keyword{internal}