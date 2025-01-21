#' @title Data from SAP
#'
#' @description This object contains the data retrieved from the Standard Assessment Procedure (SAP) for the Energy Rating of Dwellings.
#'
#' @format A list containing various datasets related to the Standard Assessment Procedure (SAP) for energy rating of dwellings
#' - AnnualRatio: A tibble containing monthly ratios for different orientations and tilts.
#'   - month: Month of the year.
#'   - horizontal: Ratio for horizontal orientation.
#'   - x30: Ratio for 30-degree tilt.
#'   - x45: Ratio for 45-degree tilt.
#'   - x60: Ratio for 60-degree tilt.
#'   - vertical: Ratio for vertical orientation.
#' - CollectorFactor: A tibble containing collector factors for different tilts and orientations.
#'   - tilt: Tilt angle.
#'   - south: Factor for south orientation.
#'   - se_sw: Factor for southeast and southwest orientation.
#'   - e_w: Factor for east and west orientation.
#'   - ne_nw: Factor for northeast and northwest orientation.
#'   - north: Factor for north orientation.
#' - CostAndCoefficients: A tibble containing cost and coefficient data for various fuels.
#'   - fuel: Type of fuel.
#'   - name: Name of the fuel.
#'   - standing_charge___: Standing charge in pence per kWh.
#'   - unit_price__p_kwh: Unit price in pence per kWh.
#'   - emissions__kgco2_kwh: Emissions in kg CO2 per kWh.
#'   - primary_energy_factor: Primary energy factor.
#'   - fuel_code: Fuel code.
#' - RegionLatitudes: A tibble containing latitudes for different regions.
#'   - id: Region ID.
#'   - region: Name of the region.
#'   - lat: Latitude of the region.
#' - SolarDeclination: A tibble containing solar declination data for each month.
#'   - month: Month of the year.
#'   - declination: Solar declination in degrees.
#' - CollectorFactor_Adjusted: A tibble containing adjusted collector factors for different tilts and orientations.
#'   - tilt: Tilt angle.
#'   - south: Adjusted factor for south orientation.
#'   - se_sw: Adjusted factor for southeast and southwest orientation.
#'   - e_w: Adjusted factor for east and west orientation.
#'   - ne_nw: Adjusted factor for northeast and northwest orientation.
#'   - north: Adjusted factor for north orientation.
#' - CostAndCoefficientsSummary: A tibble containing a summary of cost and coefficient data for various fuels.
#'   - fuel: Type of fuel.
#'   - standing_charge___: Standing charge in pence per kWh.
#'   - unit_price__p_kwh: Unit price in pence per kWh.
#'   - emissions__kgco2_kwh: Emissions in kg CO2 per kWh.
#'   - primary_energy_factor: Primary energy factor.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_sap
#' }
#' @family core data sources
"s_sap"
