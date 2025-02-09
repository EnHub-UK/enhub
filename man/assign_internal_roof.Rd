% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructions.R
\name{assign_internal_roof}
\alias{assign_internal_roof}
\title{Assigns internal roof (ceiling) properties}
\usage{
assign_internal_roof(dfa, df_hlc)
}
\arguments{
\item{dfa}{A data frame containing internal ceiling construction information.}

\item{df_hlc}{A data frame containing construction and material data (assumed to exist).}
}
\value{
A list containing:
\itemize{
\item idf: A character vector representing the IDF blocks for the internal roof.
\item summary: A data frame summarizing the internal roof properties.
}
}
\description{
Creates IDF blocks for an internal roof (ceiling) based on provided data and construction library.
}
