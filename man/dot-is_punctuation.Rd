% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.is_punctuation}
\alias{.is_punctuation}
\title{Check whether \code{char} is a punctuation character.}
\usage{
.is_punctuation(char)
}
\arguments{
\item{char}{A character scalar, comprising a single unicode character.}
}
\value{
TRUE if \code{char} is a punctuation character.
}
\description{
We treat all non-letter/number ASCII as punctuation.
Characters such as "^", "$", and "`" are not in the Unicode
Punctuation class but we treat them as punctuation anyway, for
consistency.
}
\keyword{internal}
