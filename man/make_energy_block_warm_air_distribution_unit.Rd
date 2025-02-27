% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy-system.R
\name{make_energy_block_warm_air_distribution_unit}
\alias{make_energy_block_warm_air_distribution_unit}
\title{Create IDF block for ZoneHVAC:AirDistributionUnit
objects based on loop data}
\usage{
make_energy_block_warm_air_distribution_unit(
  l_iter,
  d_loop,
  d_item,
  d_zone,
  l_sup = l_idf_blocks
)
}
\arguments{
\item{l_iter:}{List containing loop properties (e.g., name, temperature).}

\item{d_loop:}{Data frame containing detailed loop information.}

\item{d_item:}{Data frame containing system loop information.}

\item{d_zone:}{Data frame containing detailed zone information.}

\item{l_sup:}{List with supplementary data (defaults to \code{l_idf_blocks}).}
}
\value{
A character vector representing the complete IDF block content.
}
\description{
This function generates an IDF block representing a zone
equipment distribution using loop data (\code{d_loop}), system info (\code{d_item}),
zone data (\code{d_zone}) and an iterator (\code{l_iter}) defining loop properties.
It also uses supplementary data (\code{l_sup}) for comments.
}
