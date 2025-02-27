% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queue.R
\name{generate_table_of_queue_archetypes}
\alias{generate_table_of_queue_archetypes}
\title{Generates a table of queued archetypes}
\usage{
generate_table_of_queue_archetypes(p_core = the$.enhub.paths)
}
\arguments{
\item{p_core}{A list containing core directory paths (default: the$.enhub.paths).}
}
\value{
A list containing various data frames and file paths:
\itemize{
\item \code{index}: A data frame containing the evaluation index.
\item \code{queue}: A data frame containing queued archetypes with assigned UUIDs.
\item \code{new_pairs}: A data frame containing newly created pairs for evaluation.
\item \code{cloud_pairs}: A data frame containing evaluated pairs (downloaded).
\item \code{file_queue}: The file path to the queued archetypes data file.
\item \code{file_pairs}: The file path to the evaluated pairs data file.
\item \code{file_index}: The file path to the evaluation index data file.
}
}
\description{
Reads and processes data on queued archetypes, evaluated pairs, and the evaluation index.
}
\details{
This function processes data related to building archetypes for evaluation.
It reads local and remote data sources, performs cleaning and filtering,
and generates tables containing queued archetypes, newly created pairs for
evaluation, and downloaded data on previously evaluated pairs.
}
