% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data--dictionaries.R
\docType{data}
\name{l_idf_outputs}
\alias{l_idf_outputs}
\title{Dictionary with pre-selected output variables}
\format{
Lists containing various categories of output meters and output variables used in energy modelling.
\itemize{
\item Output_Meter: A list containing categories of output meters.
\itemize{
\item Renewables_PV: A character vector representing photovoltaic-related output meters.
\itemize{
\item "Photovoltaic:ElectricityProduced"
\item "ElectricitySurplusSold:Facility"
\item "ElectricityProduced:Facility"
\item "ElectricityNet:Facility"
\item ...
}
\item Renewables_Wind: A character vector representing wind turbine-related output meters.
\itemize{
\item "WindTurbine:ElectricityProduced"
\item "ElectricitySurplusSold:Facility"
\item "ElectricityProduced:Facility"
\item "ElectricityNet:Facility"
\item ...
}
\item Systems: A character vector representing various system-related output meters.
\itemize{
\item "Water:Facility"
\item "CarbonEquivalentEmissions:Carbon Equivalent"
\item "Electricity:Facility"
\item "NaturalGas:Facility"
\item ...
}
}
\item Output_Variable: A list containing categories of output variables.
\itemize{
\item Comfort: A character vector representing comfort-related output variables.
\itemize{
\item "Zone Thermal Comfort Fanger Model PMV"
}
\item Performance: A character vector representing performance-related output variables.
\itemize{
\item "Surface Window Heat Loss Rate"
\item "Surface Window Heat Gain Rate"
\item "Surface Window Net Heat Transfer Rate"
\item "Surface Inside Face Conduction Heat Transfer Rate"
\item ...
}
\item Renewables_PV: A character vector representing photovoltaic-related output variables.
\itemize{
\item "Inverter AC Output Electric Energy"
\item "Inverter AC Output Electric Power"
\item "Generator PV Array Efficiency"
\item "Generator PV Cell Temperature"
\item ...
}
\item Renewables_Wind: A character vector representing wind turbine-related output variables.
\itemize{
\item "Inverter AC Output Electric Energy"
\item "Inverter AC Output Electric Power"
\item "Generator Produced Electric Power"
\item "Generator Produced Electric Energy"
\item ...
}
\item Zonal: A character vector representing zonal-related output variables.
\itemize{
\item "Zone Air Temperature"
\item "Zone Air Humidity Ratio"
\item "Zone Air Relative Humidity"
\item "Zone Mean Air Temperature"
\item ...
}
\item Heating_District: A character vector representing district heating-related output variables.
\itemize{
\item "District Heating Hot Water Energy"
\item "District Heating Hot Water Rate"
\item "Water Heater DistrictHeating Energy"
\item "Water Heater DistrictHeating Rate"
\item ...
}
\item Heating_HP: A character vector representing heat pump-related output variables.
\itemize{
\item "Baseboard Air Inlet Temperature"
\item "Baseboard Air Mass Flow Rate"
\item "Baseboard Air Outlet Temperature"
\item "Fan Air Mass Flow Rate"
\item ...
}
\item Heating_Warm: A character vector representing warm heating-related output variables.
\itemize{
\item "Cooling Coil Water Heating Electric Energy"
\item "Cooling Coil Water Heating Electric Power"
\item "Heating Coil Electric Energy"
\item "Heating Coil Electric Power"
\item ...
}
\item Heating_General: A character vector representing general heating-related output variables.
\itemize{
\item "Baseboard Convective Heating Rate"
\item "Baseboard Hot Water Mass Flow Rate"
\item "Baseboard Radiant Heating Rate"
\item "Baseboard Total Heating Rate"
\item ...
}
\item Site: A character vector representing site-related output variables.
\itemize{
\item "Site Outdoor Air Drybulb Temperature"
\item "Site Outdoor Air Dewpoint Temperature"
\item "Site Outdoor Air Wetbulb Temperature"
\item "Site Outdoor Air Relative Humidity"
\item ...
}
\item Excluded: A character vector representing excluded output variables.
\itemize{
\item "Zone Ventilation Total Heat Gain Energy"
\item "Zone Ventilation Total Heat Loss Energy"
\item "Zone Ventilation Air Change Rate"
\item "Zone Ventilation Sensible Heat Loss Energy"
\item ...
}
}
}
}
\usage{
l_idf_outputs
}
\description{
This object contains a dictionary with pre-selected output variables
}
\examples{
\dontrun{
# Displaying the whole list
l_idf_outputs
}
}
\seealso{
Other supporting dictionaries: 
\code{\link{l_activities}},
\code{\link{l_activity_groups}},
\code{\link{l_appliance_rating}},
\code{\link{l_comfort_states}},
\code{\link{l_energy_systems}},
\code{\link{l_epw_header_units}},
\code{\link{l_housing_groups}},
\code{\link{l_htg_curves}},
\code{\link{l_idf_algorithm}},
\code{\link{l_idf_blocks}},
\code{\link{l_idf_lzc}},
\code{\link{l_sched_compact}},
\code{\link{l_schedules_airflow}},
\code{\link{l_schedules_base}},
\code{\link{l_schedules_limit}},
\code{\link{l_schedules_people}}
}
\concept{supporting dictionaries}
\keyword{datasets}
