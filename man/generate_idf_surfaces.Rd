% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layouts.R
\name{generate_idf_surfaces}
\alias{generate_idf_surfaces}
\title{Generate idf blocks for all opaque surfaces}
\usage{
generate_idf_surfaces(l_b, b_core)
}
\arguments{
\item{l_b}{list of active block/unit}

\item{b_core}{selected unit}
}
\value{
list of blocks in idf format
}
\description{
Generate idf blocks for all opaque surfaces
}
\examples{
\dontrun{
generate_idf_surfaces(l_block$`1F`$`(1F-1-1)`, TRUE)
}
}
