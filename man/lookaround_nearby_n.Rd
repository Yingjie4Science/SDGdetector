% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_SDG_search_terms.R
\name{lookaround_nearby_n}
\alias{lookaround_nearby_n}
\title{Look Around}
\usage{
lookaround_nearby_n(word_ls1, word_ls2, n, exclude = "", third_AND_string = "")
}
\arguments{
\item{word_ls1}{is a string, which includes a list of words connected by "|" that indicates 'OR'}

\item{word_ls2}{is a string, which includes a list of words connected by "|" that indicates 'OR'}

\item{n}{is a number, indicates the number of words to look around}

\item{exclude}{is a vector, including a list of words to be excluded from match}

\item{third_AND_string}{similar to word_ls1 or word_ls2, it is a string that includes
a list of words connected by "|" that indicates 'OR'}
}
\value{
A regex string
}
\description{
Look around to match pattern in a sentence
}
\examples{
con1 <- c('apple', 'bean', 'food')
con2 <- c('big', 'delicious')
lookaround_nearby_n(word_ls1 = con1, word_ls2 = con2, n = 2, exclude = "", third_AND_string = "")
}
