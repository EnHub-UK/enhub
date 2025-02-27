% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{dig_simulation_outcomes}
\alias{dig_simulation_outcomes}
\title{Digs simulation outcomes}
\usage{
dig_simulation_outcomes(
  idm,
  path_ref = the$.enhub.paths$eval$B,
  outSet = "period",
  l_ref = s_ehs_2011_ext
)
}
\arguments{
\item{idm}{An identifier for the simulation model.}

\item{path_ref}{The path to the simulation results directory (default: the$.enhub.paths$eval$B).}

\item{outSet}{A character string specifying the output set ("period" or "ddy") (default: "period").}

\item{l_ref}{A reference list for data processing (default: s_ehs_2011_ext).}
}
\value{
A data frame containing the processed simulation outputs.
}
\description{
Reads EP tables including outputs and metrics.
}
\details{
This function reads simulation output data, processes the data, and returns
a cleaned and formatted data frame.
}
