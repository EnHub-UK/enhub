% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data--dictionaries.R
\docType{data}
\name{l_idf_lzc}
\alias{l_idf_lzc}
\title{Low and zero carbon technology (LZC) descriptions}
\format{
A list of 2 character vectors:
\describe{
\item{biomass}{A character vector with 6 elements describing the biomass energy system.}
\item{solar_collector}{A character vector with 26 elements describing the solar collector system for hot water.}
}
}
\usage{
l_idf_lzc
}
\description{
A list containing descriptions of different LZC system implementation into \emph{EnergyPlus IDFs}, including biomass and solar collectors.
}
\examples{
\dontrun{
# Access the list
l_idf_lzc

# Access the biomass description
l_idf_lzc$biomass

# Access the solar collector description
l_idf_lzc$solar_collector
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
\code{\link{l_idf_outputs}},
\code{\link{l_sched_compact}},
\code{\link{l_schedules_airflow}},
\code{\link{l_schedules_base}},
\code{\link{l_schedules_limit}},
\code{\link{l_schedules_people}}
}
\concept{supporting dictionaries}
\keyword{datasets}
