% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialiser.R
\name{define_global_parameters}
\alias{define_global_parameters}
\title{Save Dictionary as JSON File}
\usage{
define_global_parameters(fileout, dirbase = getwd(), redefine = FALSE)
}
\arguments{
\item{fileout}{A string representing the output file path.}

\item{dirbase}{A string representing the base directory path.}

\item{redefine}{A boolean indicating whether to redefine and save the dictionary.}
}
\value{
None
}
\description{
This function saves a given dictionary (list) as a JSON file. It creates the
necessary directories if they do not exist, located in \verb{<root>/config/global.json}.
}
