% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data--dictionaries.R
\docType{data}
\name{l_schedules_people}
\alias{l_schedules_people}
\title{People Schedules Data}
\format{
A list of 5 elements, each containing the following components:
\describe{
\item{blocks}{A list of blocks, where each block is a list of varying length.}
\item{limits}{Character. The type of limits applied to the schedule (e.g., "Any Number").}
\item{name}{Character. The descriptive name of the schedule.}
}
}
\usage{
l_schedules_people
}
\description{
This dataset contains various schedules related to people, including activity, work efficiency, clothing, air velocity, and base activity schedules.
Each schedule includes blocks of data, limits, and a descriptive name.
}
\examples{
\dontrun{
# Access the name of the first schedule
l_schedules_people[[1]]$name

# Access the limits of the second schedule
l_schedules_people[[2]]$limits

# Access the blocks of the third schedule
l_schedules_people[[3]]$blocks

# Access the name and limits of the fourth schedule
l_schedules_people[[4]]$name
l_schedules_people[[4]]$limits

# Access the blocks of the fifth schedule
l_schedules_people[[5]]$blocks
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
\code{\link{l_idf_outputs}},
\code{\link{l_sched_compact}},
\code{\link{l_schedules_airflow}},
\code{\link{l_schedules_base}},
\code{\link{l_schedules_limit}}
}
\concept{supporting dictionaries}
\keyword{datasets}
