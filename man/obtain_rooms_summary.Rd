% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layouts.R
\name{obtain_rooms_summary}
\alias{obtain_rooms_summary}
\title{Obtain summary of house disaggregated per rooms}
\usage{
obtain_rooms_summary(l_rdim, l_rzns, l_rlys, l_rblc, l_rglz)
}
\arguments{
\item{l_rdim}{general dimensions of house}

\item{l_rzns}{list of thermal zones}

\item{l_rlys}{list of detailed room dimensions}

\item{l_rblc}{list of idf-blocks}

\item{l_rglz}{list of glazing dimensions}
}
\value{
dictionary of key characteristics of house
}
\description{
Obtain summary of house disaggregated per rooms
}
\examples{
\dontrun{
obtain_rooms_summary(l_room_dimensions, l_zones, l_room_layouts, l_blocks, l_glazing)
}
}
