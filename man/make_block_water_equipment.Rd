% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy-system.R
\name{make_block_water_equipment}
\alias{make_block_water_equipment}
\title{Creates IDF blocks for water equipment.}
\usage{
make_block_water_equipment(d_w, l_sup = l_idf_blocks)
}
\arguments{
\item{d_w}{A data frame containing water service information.}

\item{l_sup}{A list containing IDF block data (optional).}
}
\value{
A character vector representing the IDF block content.
Helper function to create water equipment block.
}
\description{
This function generates IDF blocks for water equipment based on water service data.
}
