% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idf-format.R
\name{make_idf_block}
\alias{make_idf_block}
\title{Make IDF block}
\arguments{
\item{l_in:}{A list of input data.}

\item{i_act:}{The active block.}

\item{i_rgm:}{The active regime.}

\item{i_rad:}{The active radiator.}

\item{d_ref:}{A data frame containing reference data.}

\item{l_zon:}{A list of zone data.}

\item{l_com:}{A list of IDF-comments}
}
\description{
This function generates an IDF block based on provided data.
It is used to create a block of text that can be inserted into an EnergyPlus.
The function is used as a lamda function in other functions, and reads the
EnergyPlus dictionary of IDF-comments to create a block of text.
}
\seealso{
Other idf-formatting: 
\code{\link{add_idf_comment}()},
\code{\link{add_idf_comment_custom}()},
\code{\link{determine_end_of_idf_block}()},
\code{\link{iterate_on_list}()},
\code{\link{iterate_on_output_list}()},
\code{\link{make_idf_block_header}()},
\code{\link{make_idf_block_title}()}
}
\concept{idf-formatting}
