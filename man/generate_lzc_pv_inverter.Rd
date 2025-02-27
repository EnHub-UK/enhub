% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renewables.R
\name{generate_lzc_pv_inverter}
\alias{generate_lzc_pv_inverter}
\title{Create IDF block for ElectricLoadCenter:Inverter:Simple}
\usage{
generate_lzc_pv_inverter(l_sup = l_idf_blocks)
}
\value{
A character vector representing the complete IDF block content.
}
\description{
This function generates an IDF block representing a
photovoltaic inverter object, It also uses supplementary
data (\code{l_sup}) for comments.
}
