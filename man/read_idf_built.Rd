% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{read_idf_built}
\alias{read_idf_built}
\title{Reads an IDF file from a stored batch}
\usage{
read_idf_built(idf, path_ref = the$.enhub.paths$eval$A)
}
\arguments{
\item{idf}{Character string specifying the IDF file name.}

\item{path_ref}{Character string specifying the path to the stored batch.
Defaults to \code{the$.enhub.paths$eval$A}.}
}
\value{
A character vector containing the lines of the IDF file.
}
\description{
This function reads an IDF file from a specified path within a stored batch.
The function cleans the read data by removing tabs and certain characters.
}
