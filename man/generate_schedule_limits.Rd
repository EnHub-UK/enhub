% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedules.R
\name{generate_schedule_limits}
\alias{generate_schedule_limits}
\title{Create IDF block for ScheduleTypeLimits objects based on dictionaries}
\usage{
generate_schedule_limits(l_sets, l_sup = l_idf_blocks)
}
\arguments{
\item{l_sets:}{Dictionary of schedule type limits}

\item{l_sup:}{List with supplementary data (defaults to \code{l_idf_blocks}).}
}
\value{
A character vector representing the complete IDF block content.
}
\description{
This function generates an IDF block representing a
schedule type limits object using a dictionary of limits (\code{l_sets}).
It also uses supplementary data (\code{l_sup}) for comments.
}
