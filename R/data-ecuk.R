#' @title Data from ECUK 2012
#'
#' @description This object contains the data retrieved from the ECUK sources
#'
#' @format  list containing various datasets related to energy consumption and usage in the UK for the year 2012.
#' - population_x1000.der: A tibble containing data on population in thousands.
#' - cold_x1000.der: A tibble containing data on cold-related metrics in thousands.
#' - centralheat_x1000.der: A tibble containing data on central heating in thousands.
#' - boiler_x1000.der: A tibble containing data on boilers in thousands.
#' - EnergyUse.der: A tibble containing data on energy use.
#' - intensity.der: A tibble containing data on energy intensity.
#' - intensityNormal.melt: A tibble containing normalized data on energy intensity.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_ecuk_2012
#' }
"s_ecuk_2012"


#' @title Data from ECUK 2014
#'
#' @description This object contains the data retrieved from the ECUK sources
#'
#' @format A list containing various datasets related to energy consumption and usage in the UK for the year 2014.
#' - fuels_mtoe__derived: A tibble containing data on fuels in million tonnes of oil equivalent (Mtoe), derived.
#' - fuels_ktoe.der: A tibble containing data on fuels in thousand tonnes of oil equivalent (ktoe), derived.
#' - systems_mtoe__derived: A tibble containing data on energy systems in million tonnes of oil equivalent (Mtoe), derived.
#' - temperature_c.der: A tibble containing data on temperature in degrees Celsius, derived.
#' - electricityGas.der: A tibble containing data on electricity and gas usage, derived.
#' - appliances_ktoe.der: A tibble containing data on appliances in thousand tonnes of oil equivalent (ktoe), derived.
#' - hhd.appl.groups: A tibble containing data on household appliance groups.
#' - averageDay.der: A tibble containing data on average daily energy usage, derived.
#' - sectorContribution.der: A tibble containing data on sector contributions to energy usage, derived.
#' - domesticFuel.der: A tibble containing data on domestic fuel usage, derived.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_ecuk_2014
#' }
"s_ecuk_2014"
