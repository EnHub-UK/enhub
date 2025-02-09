% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/context.R
\name{estimate_ground_temperature}
\alias{estimate_ground_temperature}
\title{Estimate Ground Temperature}
\usage{
estimate_ground_temperature(
  depth_meters,
  weather_data,
  thermal_conductivity = 1.21,
  soil_density = 1960,
  specific_heat_capacity = 840
)
}
\arguments{
\item{depth_meters}{Depth below the ground surface (m).}

\item{weather_data}{A list containing weather data, including an EPW file.}

\item{thermal_conductivity}{Soil thermal conductivity (W/m·K), default is 1.21.}

\item{soil_density}{Soil density (kg/m3), default is 1960.}

\item{specific_heat_capacity}{Soil specific heat capacity (J/kg·K), default is 840.}
}
\value{
A data frame with monthly average ground and air temperatures.
}
\description{
Calculates the ground temperature at a given depth based on air temperature
data, and soil properties.

It uses:
conductivity [W \/ m\·K]
soil_density [kg \/ m3]
specific heat capacity [J \/ kg\⋅K]
}
\references{
Labs, K. "Regional analysis of ground and above-ground climate
conclusion", Underground Space Vol.7 pp037-65, 1982.
}
