% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r-environment.R
\name{get_eplus_version}
\alias{get_eplus_version}
\title{Extracts EnergyPlus Version from Path}
\usage{
get_eplus_version(path)
}
\arguments{
\item{path}{The path string to extract the version from.}
}
\value{
The extracted EnergyPlus version (currently only supports 9.3).
}
\description{
Extracts the EnergyPlus version from a given path string.
}
\details{
This function checks if the provided path string contains a
version number matching '9-3-X' or '9.3.X' format. If a match is
found, it returns 9.3; otherwise, it stops with an error message
indicating that EnergyPlus version 9.3 is required.
}
