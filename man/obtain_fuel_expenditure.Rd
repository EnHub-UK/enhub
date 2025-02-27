% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{obtain_fuel_expenditure}
\alias{obtain_fuel_expenditure}
\title{Calculates Fuel Expenditure}
\usage{
obtain_fuel_expenditure(df, df_ref = s_sap$CostAndCoefficientsSummary)
}
\arguments{
\item{df}{A data frame containing energy use data with columns for energy__kWh and fuel type.}

\item{df_ref}{(optional) A data frame containing reference coefficients for fuel prices, standing charges, etc. Defaults to \code{s_sap$CostAndCoefficientsSummary}.}
}
\value{
A data frame with additional columns for fuel cost (fuel.cost) and standing charge (standing.charge).
}
\description{
Calculates fuel costs and standing charges based on energy use data and reference coefficients.
}
