% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{obtain_coldest_state}
\alias{obtain_coldest_state}
\title{Obtains the Coldest State from Data}
\usage{
obtain_coldest_state(d)
}
\arguments{
\item{d}{A data frame containing weather data.}
}
\value{
A data frame containing the coldest state with cleaned column names.
}
\description{
This function identifies the row with the lowest Outdoor.Air.Drybulb.Temperature..C..Hourly. value
and returns it as a data frame with cleaned column names.
}
