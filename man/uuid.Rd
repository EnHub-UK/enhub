% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r-environment.R
\name{uuid}
\alias{uuid}
\title{Generates a UUID}
\usage{
uuid(uppercase = FALSE)
}
\arguments{
\item{uppercase}{Logical indicating whether to generate an uppercase
UUID (default: FALSE).}
}
\value{
A character string representing the generated UUID.
}
\description{
Generates a universally unique identifier (UUID) in either
lowercase or uppercase format.
}
\details{
The function generates a UUID by randomly sampling hexadecimal digits
and constructing the UUID according to the UUID format. The UUID format
consists of 32 hexadecimal digits separated by hyphens, with specific
constraints on certain digits.
}
\examples{
uuid() # Generates a lowercase UUID
uuid(TRUE) # Generates an uppercase UUID
}
