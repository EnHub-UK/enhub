#' @title Data from BEIS
#'
#' @description This object contains the data retrieved from the BEIS sources
#'
#' @format A list with 4  elements: 
#' - gases.total: A tibble containing data from GHGtable1, which likely includes total greenhouse gas emissions data.
#' - sources.domestic: A tibble containing data from GHGtable2, which likely includes data on domestic sources of greenhouse gas emissions.
#' - enduses.domestic: A tibble containing data from GHGtable3, which likely includes data on domestic end uses of greenhouse gas emissions.
#' - fuels.total: A tibble containing data from GHGtable4, which likely includes total data on fuels contributing to greenhouse gas emissions.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_beis
#' }
"s_beis"
