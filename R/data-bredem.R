#' @title Data from BREDEM
#'
#' @description This object contains the data retrieved from the BREDEM sources
#'
#' @format A list with 3  elements: 
#' - FluxConstants (data.frame): A data frame containing *'Table 15`: Constants for calculation of solar flux on vertical and inclined surfaces'*
#' - GlobalIrradiance (data.frame): A data frame containing *'Table A1`: Mean global solar irradiance (W/m2) on a horizontal plane, and latitude (° North)'*
#' - HorizontalRadiation (data.frame): A data frame containing *'Table D13`: Monthly Average Horizontal Solar Radiation (W/m2)'*
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_bredem
#' }
#' @family core data sources
"s_bredem"
