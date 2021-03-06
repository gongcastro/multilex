% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_wordcloud.R
\name{ml_wordcloud}
\alias{ml_wordcloud}
\title{Retrieve and update local and/or remote data from formr}
\usage{
ml_wordcloud(responses = NULL, participant)
}
\arguments{
\item{responses}{Responses data frame, as generated by \code{ml_responses}. If NULL (default), \code{ml_responses} is run.}

\item{participant}{Character string indicating the ID of the participant to generate a wordcloud for.}
}
\value{
A plot with the wordcloud of one participant across ages
}
\description{
Retrieve and update local and/or remote data from formr
}
