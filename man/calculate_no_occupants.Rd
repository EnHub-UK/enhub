% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occupancy.R
\name{calculate_no_occupants}
\alias{calculate_no_occupants}
\title{Calculates the number of occupants for a dwelling based on EUSILC assumptions.}
\usage{
calculate_no_occupants(
  idm,
  samp = "sto",
  l_ehs = s_ehs_2011_ext,
  thrshld_ad = 0.5,
  thrshld_ch = 0.3
)
}
\arguments{
\item{idm}{Character string representing the dwelling code.}

\item{samp}{Character string specifying the sample type (default "sto").}

\item{l_ehs}{A data frame containing EHS data (e.g., s_ehs_2011_ext).}

\item{thrshld_ad}{Numeric value representing the adult threshold (default 0.5).}

\item{thrshld_ch}{Numeric value representing the child threshold (default 0.3).}
}
\value{
A list containing two data frames:
\itemize{
\item \code{full}: Data frame with detailed occupant information.
\item \code{simple}: Data frame with simplified occupant counts (adult, young, child, generic)
}
}
\description{
This function uses the European Union Statistics on Income and Living Conditions
(EUSILC) assumptions to estimate the number of occupants in a dwelling

\eqn{eHS = 1 + cA (HA − 1) + cCHC}

It considers reported occupants and a BREDEM-based estimate.
}
