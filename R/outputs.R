
obtain_idf_output_energy <- function(key, output_definitions = l_idf_outputs) {
  #' @title Obtain IDF Output for Energy
  #' @description Determines the appropriate IDF output based on the provided key.
  #' @param key The input key.
  #' @param output_definitions The output definitions. Defaults to `l_idf_outputs`.
  #' @return A character vector of IDF output variables.

  # Determine output based on key pattern
  if (grepl("^MSH9D|^MSH10D", key, perl = TRUE)) {
    # If the key starts with MSH9D or MSH10D, use Heating_Warm output variable
    idf_out <- output_definitions$Output_Variable$Heating_Warm
  } else if (grepl("MSH1[4-5]D", key, perl = TRUE)) {
    # If the key matches MSH14D or MSH15D, use Heating_General and Heating_HP output variables
    idf_out <- c(
      output_definitions$Output_Variable$Heating_General,
      output_definitions$Output_Variable$Heating_HP
    )
  } else if (grepl("MSH1[1-2]D", key, perl = TRUE)) {
    # If the key matches MSH11D or MSH12D, use Heating_General and Heating_District output variables
    idf_out <- c(
      output_definitions$Output_Variable$Heating_General,
      output_definitions$Output_Variable$Heating_District
    )
  } else {
    # For all other keys, use Heating_General output variable
    idf_out <- output_definitions$Output_Variable$Heating_General
  }

  return(idf_out)
}


obtain_idf_output_lzc <- function(load_zone_characteristics, zone_information, output_definitions_lzc) {
  #' @title Obtain IDF Output for Load Zones
  #' @description Extracts output variables and meters for load zones based on the presence of solar PV and wind turbines.
  #' @param load_zone_characteristics Load zone characteristics.
  #' @param zone_information Zone information.
  #' @param output_definitions_lzc Output definitions for load zones.
  #' @return A list containing output variables and meters.

  # Extract effective zones
  effective_zones <- zone_information$zone

  # Initialize output variables and meters
  res_var <- res_mtr <- NULL

  # Collect solar PV outputs if present
  if (TRUE %in% load_zone_characteristics["z.solarpv"]) {
    res_var <- c(res_var, output_definitions_lzc$Output_Variable$Renewables_PV)
    res_mtr <- c(res_mtr, output_definitions_lzc$Output_Meter$Renewables_PV)
  }

  # Collect wind turbine outputs if present
  if (TRUE %in% load_zone_characteristics["x.turbine"]) {
    res_var <- c(res_var, output_definitions_lzc$Output_Variable$Renewables_Wind)
    res_mtr <- c(res_mtr, output_definitions_lzc$Output_Meter$Renewables_Wind)
  }

  # Remove duplicates
  res_var <- unique(res_var)
  res_mtr <- unique(res_mtr)

  # Return results as a list
  result <- list(variables = res_var, metres = res_mtr)
  return(result)
}


expand_output_resolution <- function(resolution) {
  #' @title Expands Output Resolution
  #' @description Converts various input resolution strings to 
  #' their corresponding standard formats. If the input resolution 
  #' is not found, the original string is returned. 
  #' @param resolution The input resolution string.
  #' @return The expanded resolution string.
  #' @examples
  #' \dontrun{
  #' expand_output_resolution("hourly")
  #' }

  # Create a lookup table for resolution mappings
  resolution_mappings <- c(
    "hourly" = "hourly", "hrr" = "hourly", "h" = "hourly",
    "daily" = "daily", "day" = "daily", "d" = "daily",
    "monthly" = "monthly", "mnt" = "monthly", "m" = "monthly",
    "timestep" = "timestep", "tsp" = "timestep", "t" = "timestep"
  )

  # Return the mapped resolution or the original if not found
  return(resolution_mappings[resolution] %||% resolution)
}


make_block_output_general <- function(input_parameters = l_idf_blocks) {
  #' @title Creates a General Block Output
  #' @description Generates a list of output parameters based on the provided `input_parameters` list.
  #' @param input_parameters A list of input parameters. Defaults to `l_idf_blocks`.
  #' @return A list of output parameters.

  # Define output parameters
  output_parameters <- list(
    "CurrencyType" = list(
      value = "GBP"
    ),
    "Output:VariableDictionary" = list(
      value = "IDF", sort = "Name"
    ),
    "Output:Surfaces:List" = list(
      type = "Details", spec = "IDF"
    ),
    "Output:Surfaces:Drawing" = list(
      type = "VRML", spec = "Triangulate3DFace"
    ),
    "Output:Constructions" = list(
      type_a = "Constructions", type_b = "Materials"
    ),
    "Output:Table:SummaryReports" = list(
      rep_a = "AllSummary"
    ),
    "OutputControl:Table:Style" = list(
      sepa = "CommaAndHTML", unit = "JtoKWH"
    ),
    "OutputControl:ReportingTolerances" = list(
      t_htg = "0.2", t_clg = "0.2"
    ),
    "Output:Diagnostics" = list(
      key = "DisplayAdvancedReportVariables"
    )
  )

  # Apply `iterate_on_output_list` to each output parameter
  idf_res <-
    lapply(
      names(output_parameters),
      iterate_on_output_list, output_parameters, input_parameters
    ) %>% unlist()

  return(idf_res)
}


make_block_output_variables <- function(label_list, load_zone_calculations, system_key, result_key, support_list = l_idf_blocks) {
  #' @title Creates Output Variables for an IDF Block
  #' @description This function generates a list of output variables based on provided parameters.
  #' @param label_list A label list.
  #' @param load_zone_calculations A list of load zone calculations.
  #' @param system_key A system key.
  #' @param result_key A result key.
  #' @param support_list A support list. Defaults to `l_idf_blocks`.
  #' @return A character vector representing the IDF block structure.

  # Create a list of output variables
  output_variables <- list(
    site = label_list$Output_Variable$Site,
    performance = label_list$Output_Variable$Performance,
    comfort = label_list$Output_Variable$Comfort,
    lzc = load_zone_calculations$variables,
    heating = obtain_idf_output_energy(system_key)
  )

  # Filter and format output variables
  output_variables <- output_variables[lengths(output_variables) > 0L] %>%
    unlist() %>%
    as.character()

  # Process output variables with iterate_on_list
  active_output <- "Output:Variable"
  common_parameters <- support_list[[active_output]]
  parameter_outputs <- lapply(output_variables, function(x) {
    iterate_on_list(list(active_output, "*", x, result_key), common_parameters)
  }) %>%
    unlist() %>%
    as.character()

  # Create IDF block structure
  idf_result <- c(make_idf_block_title(active_output), "", parameter_outputs)

  return(idf_result)
}


make_block_output_zonal <- function(label_list, zones, result_key, support_list = l_idf_blocks) {
  #' @title Creates Output Variables for Zonal Data in an IDF Block
  #' @description This function generates a list of output variables for zonal data based on provided parameters.
  #' @param label_list A label list.
  #' @param zones A list of zones.
  #' @param result_key A result key.
  #' @param support_list A support list. Defaults to `l_idf_blocks`.
  #' @return A character vector representing the IDF block structure.

  # Extract zonal output variables
  output_variables <- list(zonal = label_list$Output_Variable$Zonal)
  output_variables <- output_variables[lengths(output_variables) > 0L] %>%
    unlist() %>%
    as.character()

  # Escape parentheses -- EP bug
  zones <- zones %>%
    gsub("\\(", "\\\\(", .) %>%
    gsub("\\)", "\\\\)", .)

  # Create output variables for each zone
  active_output <- "Output:Variable"
  common_parameters <- support_list[[active_output]]
  parameter_outputs <- lapply(zones, function(y) {
    lapply(output_variables, function(x) {
      iterate_on_list(
        list(active_output, y, x, result_key), common_parameters
      )
    })
  }) %>%
    unlist() %>%
    as.character()

  # Create IDF block structure
  idf_result <- c(make_idf_block_title(active_output), "", parameter_outputs)

  return(idf_result)
}


make_block_output_metres <- function(label_list, heating_data, load_zone_calculations, resolution_key, support_list = l_idf_blocks) {
  #' @title Creates Output Meters for an IDF Block
  #' @description This function generates a list of output meters based on provided parameters.
  #' @param label_list A list of available outcome variables.
  #' @param heating_data A list of heating system data.
  #' @param load_zone_calculations A list of load zone calculations.
  #' @param resolution_key A string indicating the resolution of the output.
  #' @param support_list A support list. Defaults to `l_idf_blocks`.
  #' @return A character vector representing the IDF block structure.

  # Filter meters based on fuel types
  meters <- label_list$Output_Meter$Systems
  fuels <- c(heating_data$systems$main_fuel, heating_data$systems$dhw_fuel) %>% unique()
  facility_fuels <- c(fuels, "Electricity", "MainsWater", "Water")
  facility_fuels <- gsub("Natural", "", facility_fuels)
  base_fuels <- c("Electricity", "Facility", "Equivalent", "Plant")
  fuels <- c(base_fuels, fuels)
  meters <- meters[grepl(paste(fuels, collapse = "|"), meters)]
  facility_meters <- meters[grepl("Facility$", meters)]
  non_facility_meters <- meters[!grepl("Facility$", meters)]
  facility_meters <- facility_meters[grepl(paste(facility_fuels, collapse = "|"), facility_meters)]
  meters <- c(facility_meters, non_facility_meters)

  # Prepare list of output variables
  output_variables <- list(heating = meters, lzc = load_zone_calculations$metres)
  output_variables <- output_variables[lengths(output_variables) > 0L] %>%
    unlist() %>%
    as.character()

  # Create output meter parameters
  active_output <- "Output:Meter"
  common_parameters <- support_list[[active_output]]
  parameter_outputs <- lapply(output_variables, function(x) {
    iterate_on_list(list(active_output, x, resolution_key), common_parameters)
  }) %>%
    unlist() %>%
    as.character()

  # Create IDF block structure
  idf_result <- c(make_idf_block_title(active_output), "", parameter_outputs)

  return(idf_result)
}
