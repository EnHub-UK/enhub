% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idf-sections.R
\name{populate_idf_section_G}
\alias{populate_idf_section_G}
\title{Populates the IDF section G with energy system and water service data.}
\usage{
populate_idf_section_G(
  hid,
  l_dim,
  l_scd,
  l_lds,
  d_ids = the$core_index,
  l_ehs = s_ehs_2011_ext
)
}
\arguments{
\item{hid}{Character string representing the housing code.}

\item{l_dim}{List containing building dimensions data.}

\item{l_scd}{List containing schedule data.}

\item{l_lds}{List containing load data.}

\item{d_ids}{Dataframe containing housing code information (default: the$core_index).}

\item{l_ehs}{List containing EHS data (default: s_ehs_2011_ext).}
}
\value{
A list containing a summary and IDF sections.
}
\description{
This function processes housing data, calculates energy system components,
generates water service data, and creates IDF sections for energy systems,
schedules, and curves.
}
