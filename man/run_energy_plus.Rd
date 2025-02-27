% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eplus-wrapper.R
\name{run_energy_plus}
\alias{run_energy_plus}
\title{Runs an EnergyPlus simulation}
\usage{
run_energy_plus(i, path_epw)
}
\arguments{
\item{i}{Index or ID for the simulation.}

\item{path_epw}{Path to the EnergyPlus Weather File}
}
\value{
Path to the simulation output directory.
}
\description{
A wrapper to execute an EnergyPlus simulation based on the
provided the index/location of the \if{html}{\out{<span style="color:pink">}}EnergyPlus
Input Files (IDF)\if{html}{\out{</span>}} and \if{html}{\out{<span style="color:pink">}}EnergyPlus Weather
File (EPW)\if{html}{\out{</span>}}. It handles file set-up, simulation execution, output
processing, and clean-up.
}
\seealso{
Other EnergyPlus evaluation: 
\code{\link{convert_idf_to_json}()},
\code{\link{extract_simulation_outputs}()},
\code{\link{generate_idf}()},
\code{\link{generate_idf_multiple}()},
\code{\link{read_idf_outcomes}()},
\code{\link{read_input_file}()},
\code{\link{read_input_summary}()},
\code{\link{simulate_available_idfs}()}
}
\concept{EnergyPlus evaluation}
