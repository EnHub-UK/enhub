% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data--dictionaries.R
\docType{data}
\name{l_energy_systems}
\alias{l_energy_systems}
\title{Dictionary with energy systems information}
\format{
A list containing categories of heating systems.
\itemize{
\item space_heating: A character vector representing different types of space heating systems.\tabular{ll}{
    \tab  \cr
   1 \tab Gas standard \cr
   2 \tab Gas ‐ combi \cr
   3 \tab Gas back boiler \cr
   4 \tab Oil standard \cr
   5 \tab Solid boiler (house coal/anthracite) \cr
   6 \tab Electric boiler \cr
   7 \tab Electric storage \cr
   8 \tab Electric room heater \cr
   9 \tab Warm air ‐ gas fired \cr
   10 \tab Warm air ‐ electric \cr
   11 \tab Community heating without CHP \cr
   12 \tab Community heating with CHP \cr
   13 \tab Biomass boiler \cr
   14 \tab Ground source heat pump (GSHP) \cr
   15 \tab Air source heat pump (ASHP) \cr
}

\item hot_water: A character vector representing different types of hot water systems.\tabular{ll}{
    \tab  \cr
   1 \tab Gas standard \cr
   2 \tab Gas - combi (storage) \cr
   3 \tab Gas - combi (instantaneous) \cr
   4 \tab Gas back boiler \cr
   5 \tab Oil standard \cr
   6 \tab Solid boiler (house coal/anthracite) \cr
   7 \tab Biomass boiler \cr
   8 \tab Electric boiler \cr
   9 \tab Other electric \cr
   10 \tab Community heating without CHP \cr
   11 \tab Community heating with CHP \cr
}

}
}
\usage{
l_energy_systems
}
\description{
This object contains a dictionary with energy systems information
}
\examples{
\dontrun{
# Displaying the whole list
l_energy_systems
}
}
\references{
Hughes, M., Armitage, P., Palmer, J., & Stone, A. (2012).
Converting English Housing Survey Data for Use in Energy Models.
}
\seealso{
Other supporting dictionaries: 
\code{\link{l_activities}},
\code{\link{l_activity_groups}},
\code{\link{l_appliance_rating}},
\code{\link{l_comfort_states}},
\code{\link{l_epw_header_units}},
\code{\link{l_housing_groups}},
\code{\link{l_htg_curves}},
\code{\link{l_idf_algorithm}},
\code{\link{l_idf_blocks}},
\code{\link{l_idf_lzc}},
\code{\link{l_idf_outputs}},
\code{\link{l_sched_compact}},
\code{\link{l_schedules_airflow}},
\code{\link{l_schedules_base}},
\code{\link{l_schedules_limit}},
\code{\link{l_schedules_people}}
}
\concept{supporting dictionaries}
\keyword{datasets}
