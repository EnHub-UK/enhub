% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy-system.R
\name{make_energy_block_plant_sizing}
\alias{make_energy_block_plant_sizing}
\title{Create IDF block for Sizing Plant based on loop data}
\usage{
make_energy_block_plant_sizing(l_iter, l_sup = l_idf_blocks)
}
\arguments{
\item{l_iter:}{List containing loop properties (e.g., name, temperature).}

\item{l_sup:}{List with supplementary data (defaults to \code{l_idf_blocks}).}
}
\value{
A character vector representing the complete IDF block content.
}
\description{
This function generates an IDF block representing a sized
plant loop using an iterator (\code{l_iter}) defining loop properties.
It also uses supplementary data (\code{l_sup}) for comments.
}
