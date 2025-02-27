% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-sap.R
\docType{data}
\name{s_sap}
\alias{s_sap}
\title{Data from SAP}
\format{
A list containing various datasets related to the Standard Assessment Procedure (SAP) for energy rating of dwellings
\itemize{
\item AnnualRatio: A tibble containing monthly ratios for different orientations and tilts.
\itemize{
\item month: Month of the year.
\item horizontal: Ratio for horizontal orientation.
\item x30: Ratio for 30-degree tilt.
\item x45: Ratio for 45-degree tilt.
\item x60: Ratio for 60-degree tilt.
\item vertical: Ratio for vertical orientation.
}
\item CollectorFactor: A tibble containing collector factors for different tilts and orientations.
\itemize{
\item tilt: Tilt angle.
\item south: Factor for south orientation.
\item se_sw: Factor for southeast and southwest orientation.
\item e_w: Factor for east and west orientation.
\item ne_nw: Factor for northeast and northwest orientation.
\item north: Factor for north orientation.
}
\item CostAndCoefficients: A tibble containing cost and coefficient data for various fuels.
\itemize{
\item fuel: Type of fuel.
\item name: Name of the fuel.
\item standing_charge___: Standing charge in pence per kWh.
\item unit_price__p_kwh: Unit price in pence per kWh.
\item emissions__kgco2_kwh: Emissions in kg CO2 per kWh.
\item primary_energy_factor: Primary energy factor.
\item fuel_code: Fuel code.
}
\item RegionLatitudes: A tibble containing latitudes for different regions.
\itemize{
\item id: Region ID.
\item region: Name of the region.
\item lat: Latitude of the region.
}
\item SolarDeclination: A tibble containing solar declination data for each month.
\itemize{
\item month: Month of the year.
\item declination: Solar declination in degrees.
}
\item CollectorFactor_Adjusted: A tibble containing adjusted collector factors for different tilts and orientations.
\itemize{
\item tilt: Tilt angle.
\item south: Adjusted factor for south orientation.
\item se_sw: Adjusted factor for southeast and southwest orientation.
\item e_w: Adjusted factor for east and west orientation.
\item ne_nw: Adjusted factor for northeast and northwest orientation.
\item north: Adjusted factor for north orientation.
}
\item CostAndCoefficientsSummary: A tibble containing a summary of cost and coefficient data for various fuels.
\itemize{
\item fuel: Type of fuel.
\item standing_charge___: Standing charge in pence per kWh.
\item unit_price__p_kwh: Unit price in pence per kWh.
\item emissions__kgco2_kwh: Emissions in kg CO2 per kWh.
\item primary_energy_factor: Primary energy factor.
}
}
}
\usage{
s_sap
}
\description{
This object contains the data retrieved from the Standard Assessment Procedure (SAP) for the Energy Rating of Dwellings.
}
\examples{
\dontrun{
# Displaying the whole list
s_sap
}
}
\seealso{
Other core data sources: 
\code{\link{s_bredem}},
\code{\link{s_chm}},
\code{\link{s_chm_reference}},
\code{\link{s_ehs_2011_ext}},
\code{\link{s_ehs_2011_hhd}},
\code{\link{s_ehs_2011_rom}},
\code{\link{s_tus}}
}
\concept{core data sources}
\keyword{datasets}
