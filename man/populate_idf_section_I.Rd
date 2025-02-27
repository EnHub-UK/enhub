% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idf-sections.R
\name{populate_idf_section_I}
\alias{populate_idf_section_I}
\title{Populates the IDF section I with low-zero carbon (LZC) technology data.}
\usage{
populate_idf_section_I(
  hid,
  l_weather,
  l_context,
  l_dim,
  d_ids = the$core_index,
  l_ehs = s_ehs_2011_ext
)
}
\arguments{
\item{hid}{Character string representing the housing code.}

\item{l_weather}{List containing weather data.}

\item{l_context}{List containing contextual data.}

\item{l_dim}{List containing building dimensions.}

\item{d_ids}{Dataframe containing housing code information (default: the$core_index).}

\item{l_ehs}{List containing EHS data (default: s_ehs_2011_ext).}
}
\value{
A list containing a summary and IDF sections.
}
\description{
This function processes housing data, determines LZC technologies, and generates
IDF sections for solar, water heating, wind, and biomass systems.
}
