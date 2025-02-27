% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occupancy.R
\name{estimate_occupancy}
\alias{estimate_occupancy}
\title{Estimates Occupancy}
\usage{
estimate_occupancy(tfa)
}
\arguments{
\item{tfa}{Total forest area.}
}
\value{
Estimated occupancy.
}
\description{
Estimates occupancy based on the given total forest area (TFA).
}
\details{
The function calculates occupancy using a combination of base occupancy,
exponential, and linear terms. The base occupancy is 1 for TFA less than 13.9,
otherwise it's also 1. The exponential term is calculated based on the TFA,
and the linear term is also calculated based on the TFA. The final occupancy
is the sum of the base occupancy and the occupancy adjustment, which is
the product of the exponential term and a constant plus the linear term.
}
\examples{
estimate_occupancy(10)
estimate_occupancy(20)
}
