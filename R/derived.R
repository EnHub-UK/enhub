
get_aacode <- function(codes) {
  #' @title Retrieve IDF Codes
  #'
  #' @description
  #' Extracts IDF codes from a reference data frame based on the
  #' specified mode and input codes.
  #'
  #' @param codes The input codes for retrieving IDF codes.
  #' @examples
  #' get_aacode(2)
  #' @return A vector of IDF codes.

  # Extract IDF codes based on other modes
  idf_codes <- the$core_index %>% 
    dplyr::filter(index == codes) %>% 
    dplyr::pull(aacode)

  return(idf_codes)
}

assign_comfort_ranges <- function(oscillation_value) {
  #' @title Assign Comfort Ranges
  #'
  #' @description
  #' Determines the comfort range based on the given oscillation value.
  #'
  #' @param oscillation_value The oscillation value.
  #' @return The corresponding comfort range.

  # Define the comfort ranges and their corresponding oscillation thresholds
  ranges <- c(2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7.5)
  thresholds <- c(13, 15.5, 18.5, 23.5, 27.5, 32.5, 37.5, 44.5, Inf)

  # Find the first threshold that is greater than or equal to the oscillation value
  index <- which(oscillation_value <= thresholds)[1]

  # Return the corresponding comfort range
  return(ranges[index])
}


decode_energy_system <- function(energy_system_id) {
  #' @title Decode Energy System Information
  #'
  #' @description
  #' Extracts the MSH and DHW components from an energy system ID code.
  #'
  #' @param energy_system_id The energy system ID code.
  #' @return A list containing:
  #'   * `msh`: The MSH component of the ID code.
  #'   * `dhw`: The DHW component of the ID code.

  # Extract main heating system (MSH) component
  msh_component <- gsub("^MSH(\\d+)(.*)$", "\\1", energy_system_id) %>% as.integer()

  # Extract domestic hot water (DHW) component
  dhw_component <- gsub("(.*DHW)(\\d*)$", "\\2", energy_system_id) %>% as.integer()

  # Return the components as a list
  return(list(msh = msh_component, dhw = dhw_component))
}


parse_space_heating_group <- function(energy_system_code) {
  #' @title Parse Space Heating Group
  #'
  #' @description
  #' Determines the space heating group based on the provided energy system code.
  #'
  #' @param energy_system_code The energy system code.
  #' @return The space heating group.

  # Decode the energy system code
  decoded_code <- decode_energy_system(energy_system_code)

  # Extract the main heating system component
  main_heating_system <- decoded_code$msh

  # Assign the heating system group based on the main heating system component
  if (main_heating_system %in% c(1:6, 11:13)) {
    heating_group <- "central heating"
  } else if (main_heating_system %in% 7:8) {
    heating_group <- "electric heaters"
  } else if (main_heating_system %in% 9:10) {
    heating_group <- "warm air"
  } else if (main_heating_system %in% 14:15) {
    heating_group <- "heat pump"
  } else {
    heating_group <- "undefined system"
  }

  return(heating_group)
}
