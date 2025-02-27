% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy-system.R
\name{assign_cylinder_insulation_transfer}
\alias{assign_cylinder_insulation_transfer}
\title{Assigns thermal conductance based on insulation data.}
\usage{
assign_cylinder_insulation_transfer(insulation_data)
}
\arguments{
\item{insulation_data}{A data frame with columns 'type'
(insulation type) and 'thickness' (insulation in millimeters).}
}
\value{
Thermal conductance in W/mK.
}
\description{
This function takes a data frame containing insulation information
and calculates the thermal conductance based on type and thickness.
}
