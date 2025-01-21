#' @title Data from World Bank
#'
#' @description This object contains the data retrieved from the World Bank sources
#'
#' @format A list containing various datasets related to World Bank indicators.
#' - d_carbon_intensity: A tibble containing data on carbon intensity with 59 rows and 11 columns.
#'   - year: Year of the data.
#'   - en.atm.ghgt.kt.ce: Total greenhouse gas emissions (kt of CO2 equivalent).
#'   - eg.use.pcap.kg.oe: Energy use (kg of oil equivalent per capita).
#'   - en.atm.co2e.kt: CO2 emissions (kt).
#'   - sp.pop.totl.ma.in: Population, male.
#'   - sp.pop.totl.fe.in: Population, female.
#'   - si.pov.gini: Gini index (measure of income inequality).
#'   - ny.gdp.mktp.kd: GDP (constant 2010 US$).
#'   - eg.use.elec.kh.pc: Electric power consumption (kWh per capita).
#'   - en.co2.bldg.zs: CO2 emissions from buildings (% of total fuel combustion).
#'   - en.atm.co2e.eg.zs: CO2 emissions from electricity and heat production (% of total fuel combustion).
#' - lbl_carbon_intensity: A data frame containing metadata for the carbon intensity indicators with 10 rows and 3 columns.
#'   - Code: Indicator code.
#'   - Indicator.Name: Name of the indicator.
#'   - License.Type: License type for the data.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_world_bank
#' }
"s_world_bank"
