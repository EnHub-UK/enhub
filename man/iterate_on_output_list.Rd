% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idf-format.R
\name{iterate_on_output_list}
\alias{iterate_on_output_list}
\title{Iterate Over Output List}
\usage{
iterate_on_output_list(ip, lp, sup)
}
\arguments{
\item{ip}{Input parameter.}

\item{lp}{List of parameters.}

\item{sup}{Supplementary information.}
}
\value{
A character vector of IDF comments.
}
\description{
This function iterates over an output list and creates IDF comments.

Here is a usage example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{iterate_on_output_list(
 "Site:Location", 
 list("Site:Location" = c("a location", 51.15, -0.18, 0.0, 62.0)), 
 l_idf_blocks)
}\if{html}{\out{</div>}}

And the output will be:

\if{html}{\out{<div class="sourceCode fortran">}}\preformatted{
 !-   =========== ALL OBJECTS IN CLASS: SITE:LOCATION ==========="
 
 Site:Location,  !- "
     a location, !- Name"
     51.15,      !- Latitude"
     -0.18,      !- Longitude"
     0,          !- Time Zone"
     62;         !- Elevation"

}\if{html}{\out{</div>}}
}
\examples{
# Example 1: Iterate over an output list
iterate_on_output_list("class1", 
 list("class1" = c("comment2", "comment2")), 
 list("class1" = c("comment2", "comment2")))

# Example 2: Add IDF comments to a list of values and corresponding comments
iterate_on_output_list("Site:Location", 
 list("Site:Location" = c("a location", 51.15, -0.18, 0.0, 62.0)), 
 l_idf_blocks)
}
\seealso{
Other idf-formatting: 
\code{\link{add_idf_comment}()},
\code{\link{add_idf_comment_custom}()},
\code{\link{determine_end_of_idf_block}()},
\code{\link{iterate_on_list}()},
\code{\link{make_idf_block}},
\code{\link{make_idf_block_header}()},
\code{\link{make_idf_block_title}()}
}
\concept{idf-formatting}
