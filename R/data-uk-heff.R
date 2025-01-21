#' @title Data from UK-HEFF
#'
#' @description This object contains the data retrieved from the UK-Housing Energy Fact File sources
#'
#' @format A list containing various datasets related to household energy efficiency in the UK.
#' - population.raw: A tibble containing raw data on population.
#' - households_x10e6.raw: A tibble containing raw data on households in millions.
#' - households_x10e6.eng: A tibble containing data on households in millions for England.
#' - tenure_x10e6.raw: A tibble containing raw data on tenure in millions.
#' - type_x10e6.raw: A tibble containing raw data on dwelling types in millions.
#' - fuelPrices.raw: A tibble containing raw data on fuel prices.
#' - use24hrs: A tibble containing data on energy use over 24 hours.
#' - seasonal: A tibble containing data on seasonal energy use.
#' - power: A tibble containing data on power schedules.
#' - schedules: A tibble containing data on various schedules.
#' - typical: Data on typical energy use patterns.
#' - inventory: Data on the inventory of energy-related items.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_uk_heff
#' }
"s_uk_heff"
