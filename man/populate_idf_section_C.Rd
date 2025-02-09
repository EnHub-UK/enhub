% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idf-sections.R
\name{populate_idf_section_C}
\alias{populate_idf_section_C}
\title{Populates IDF section C with usage schedules and profiles}
\usage{
populate_idf_section_C(
  hid,
  l_dim,
  d_ids = the$core_index,
  l_ehs = s_ehs_2011_ext,
  d_tus = s_tus,
  d_rom = d_rooms
)
}
\arguments{
\item{hid}{A character string representing the housing code.}

\item{l_dim}{A list containing building dimension data.}

\item{d_ids}{A data frame containing housing code information (default: the$core_index).}

\item{l_ehs}{A list containing EHS data (default: s_ehs_2011_ext).}

\item{d_tus}{A list containing TUS data (default: s_tus).}

\item{d_rom}{A data frame containing room data (default: d_rooms).}
}
\value{
A list containing:
\itemize{
\item sum: A summary of the generated parameters, including schedules, occupants, and zone appliance profiles.
\item idf: A character vector representing the IDF content for section C.
}
}
\description{
Creates IDF content for section C based on provided housing code, building dimensions,
and various other data sources.
}
