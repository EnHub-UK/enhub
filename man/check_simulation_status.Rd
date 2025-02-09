% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/status.R
\name{check_simulation_status}
\alias{check_simulation_status}
\title{Checks the status of simulations.}
\usage{
check_simulation_status(f_sel = NULL)
}
\arguments{
\item{f_sel}{Optional simulation ID to check.
If NULL, checks all simulations.}
}
\value{
A list containing simulation status information.
}
\description{
This function checks the status of simulations based on the presence of
specific files in predefined directories.
}
