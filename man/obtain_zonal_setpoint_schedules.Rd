% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedules.R
\name{obtain_zonal_setpoint_schedules}
\alias{obtain_zonal_setpoint_schedules}
\title{Extract and format zonal setpoint schedules}
\usage{
obtain_zonal_setpoint_schedules(df)
}
\arguments{
\item{df:}{A data frame containing columns for heating setpoints (htg_sp),
cooling setpoints (clg_sp), heating schedules (htg_schedules), and
cooling schedules (clg_schedules).}
}
\value{
: A list of lists containing formatted zonal setpoint schedules.
}
\description{
This function extracts and formats zonal setpoint
schedules from a data frame.
}
