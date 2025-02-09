% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcomes.R
\name{extract_simulation_outputs}
\alias{extract_simulation_outputs}
\title{Extracts simulation outputs}
\usage{
extract_simulation_outputs(i)
}
\arguments{
\item{i}{\link{integer} The index of the evaluated IDF file.}
}
\value{
A list containing extracted simulation data, including energy
demand, performance metrics, output cards, raw results, and schedules.
}
\description{
This function extracts and processes simulation outputs
(ie. \code{results_eplus.csv.gz} and \code{results_summary.csv.gz}) stored in
the folder \if{html}{\out{<local>}}/outcomes/simulation/b_outputs/\if{html}{\out{<id>}}
}
\seealso{
Other EnergyPlus evaluation: 
\code{\link{convert_idf_to_json}()},
\code{\link{generate_idf}()},
\code{\link{generate_idf_multiple}()},
\code{\link{read_idf_outcomes}()},
\code{\link{read_input_file}()},
\code{\link{read_input_summary}()},
\code{\link{run_energy_plus}()},
\code{\link{simulate_available_idfs}()}
}
\concept{EnergyPlus evaluation}
