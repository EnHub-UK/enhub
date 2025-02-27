% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{parse_body}
\alias{parse_body}
\title{Parse Body from EP+ Summary File}
\usage{
parse_body(db, t_b, t_h)
}
\arguments{
\item{db}{A character vector representing the lines of an EP+ summary file.}

\item{t_b}{A character string indicating the beginning of the body section.}

\item{t_h}{A character vector representing the header for the body data.}
}
\value{
A data frame containing the parsed body data.
}
\description{
Extracts and parses the body data from an EP+ summary file.

This function processes the body section of an EP+ summary file, extracting
individual rows, parsing them into columns based on the provided header, and
combining them into a data frame.
}
