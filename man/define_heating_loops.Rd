% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy-system.R
\name{define_heating_loops}
\alias{define_heating_loops}
\title{Define heating loops based on main system type}
\usage{
define_heating_loops(k_msh, l_loop)
}
\arguments{
\item{k_msh:}{Integer representing the master system type code.}
}
\value{
A character vector containing the possible water loop types.
}
\description{
This function takes a main system type code (\code{k_msh}) and
determines the possible water loop types associated with that system.
}
\examples{
define_heating_loops(1, 3)
define_heating_loops(15, 8)
}
