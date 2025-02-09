% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructions.R
\name{assign_underfloor_heat}
\alias{assign_underfloor_heat}
\title{Assigns underfloor heating parameters}
\usage{
assign_underfloor_heat(idf_block, l_sup = l_idf_blocks)
}
\arguments{
\item{idf_block}{A character vector representing the existing IDF content.}
}
\value{
A character vector representing the updated IDF content with underfloor heating parameters (if applicable).
}
\description{
Inserts IDF blocks for underfloor heating parameters if specified.
}
