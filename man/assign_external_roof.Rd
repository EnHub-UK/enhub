% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructions.R
\name{assign_external_roof}
\alias{assign_external_roof}
\title{Assigns external roof properties}
\usage{
assign_external_roof(dfa, dfb, k_att, df_hlc)
}
\arguments{
\item{dfa}{A data frame containing roof construction information.}

\item{dfb}{A data frame containing dwelling age information.}

\item{k_att}{A logical value indicating a pitched roof (TRUE) or flat roof (FALSE).}

\item{df_hlc}{A data frame containing construction and material data (assumed to exist).}
}
\value{
A list containing:
\itemize{
\item idf: A character vector representing the IDF blocks for the external roof.
\item summary: A data frame summarizing the external roof properties.
}
}
\description{
Creates IDF blocks and summarizes properties for an external roof based on provided data and construction library.
}
