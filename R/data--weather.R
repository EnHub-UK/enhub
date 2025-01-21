#' @title Data with weather station information
#'
#' @description This object contains the summary table of weather station information.
#'
#' @format A data frame containing weather station information.
#' - station: An integer representing the station ID.
#' - place: A character string representing the name of the place where the station is located.
#' - admin: A character string representing the administrative region code (e.g., "ENG" for England).
#' - country: A character string representing the country code (e.g., "GBR" for Great Britain).
#' - lat: A numeric value representing the latitude of the station.
#' - lon: A numeric value representing the longitude of the station.
#' - tzn: An integer representing the time zone offset from UTC.
#' - elv: An integer representing the elevation of the station in meters.
#' - hist: A logical value indicating whether historical data is available for the station.
#' - filepack: A character string representing the name of the file package containing the data.
#' - uuid: A character string representing the unique identifier for the station.
#' - regional_default: A character string representing the regional default settings (if any).
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' d_stations
#' }
"d_stations"
