% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy-system.R
\name{make_energy_block_heat_pump_coil_w_auxiliary}
\alias{make_energy_block_heat_pump_coil_w_auxiliary}
\title{Create IDF block for Coil:WaterHeating:AirToWaterHeatPump:Pumped
object based on loop data}
\usage{
make_energy_block_heat_pump_coil_w_auxiliary(
  l_iter,
  d_loop,
  d_item,
  l_sup = l_idf_blocks
)
}
\arguments{
\item{l_iter:}{List containing loop properties (e.g., name, temperature).}

\item{d_loop:}{Data frame containing detailed loop information.}

\item{d_item:}{Data frame containing system loop information.}

\item{l_sup:}{List with supplementary data (defaults to \code{l_idf_blocks}).}
}
\value{
A character vector representing the complete IDF block content.
}
\description{
This function generates an IDF block representing a heat pump
coil using system info (\code{d_item}), and an iterator (\code{l_iter}) defining
loop properties. It also uses supplementary data (\code{l_sup}) for comments.
}
