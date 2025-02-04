% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{obtain_airflow_stats}
\alias{obtain_airflow_stats}
\title{Extracts Airflow and Setpoint Statistics}
\usage{
obtain_airflow_stats(df)
}
\arguments{
\item{df}{A data frame containing airflow and setpoint data.}
}
\value{
A list containing three data frames:
\itemize{
\item \code{ventilation}: Ventilation airflow statistics.
\item \code{infiltration}: Infiltration airflow statistics.
\item \code{setpoints}: Zone setpoint information.
}
}
\description{
Extracts ventilation, infiltration, and setpoint data from a given data frame.
}
