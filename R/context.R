
#.. building context ----------------------------------------------------------

assign_house_orientation <- function(building_properties, reference_df) {
  #' @title Assign House Orientation
  #'
  #' @description
  #' Extracts the house orientation from a list of building properties or a reference data frame.
  #' If the "house_front_orientation" key is not found in the list, attempts to retrieve
  #' the orientation from the "V585_CuboidOrientation" variable in the reference data frame.
  #' Missing values in the reference data frame are assumed to be North (0).
  #'
  #' @param building_properties A list containing building property information.
  #' @param reference_df A reference data frame (optional) containing house orientation data.
  #' @return The house orientation as a numeric value.

  # Check if house_front_orientation exists in building_properties
  if ("house_front_orientation" %in% names(building_properties)) {
    v_ori <- building_properties$house_front_orientation
  } else {
    # Use reference data frame if provided
    v_ori <- reference_df$V585_CuboidOrientation
    # If orientation is unknown, then assume it is North (0)
    v_ori <- ifelse(is.na(v_ori), 0, v_ori)
  }

  return(v_ori)
}

obtain_ddy <- function(idf_file, rain_value, design_day_family = "heating") {
  #' @title Extract Design Day Objects from IDF File
  #'
  #' @description
  #' This function parses an IDF file (`idf_file`) and extracts information
  #' about Design Day objects based on the specified family (`design_day_family`).
  #' It modifies the IDF file to include a rain value (`rain_value`) for those objects.
  #'
  #' @param idf_file A character vector containing the lines of the IDF file.
  #' @param rain_value The rain value to be included in Design Day objects.
  #' @param design_day_family The type of Design Day objects to extract ('heating', 'cooling', or 'other'). Defaults to 'heating'.
  #' @return A list containing:
  #'   * `idf`: A character vector with the modified IDF file content.
  #'   * `sum`: A data frame summarizing the extracted Design Day objects (label and group).

  # Use pre-defined indentation
  indent <- the$.indent

  # 1. Prepare IDF header and read lines
  idf_class <- make_idf_block_header("DESIGN DAYS: EPW FILES")
  idf_block <- readLines(idf_file)

  # 2. Modify Site:Location line
  ln. <- grep("Site:Location,", idf_block)
  idf_block <- c("", idf_class, idf_block[(ln. + 6):length(idf_block)], "")

  # 3. Update rain value in .rain lines
  ln. <- grep(".rain,", idf_block)
  idf_block[ln.] <- paste0(indent, rain_value, ".csv,") # EP-bug: see weather.R-117
  idf_block[ln. - 1] <- paste0(indent, "Any Number, !- Schedule Type Limits Name")

  # 4. Extract information about existing Design Day objects
  ln. <- grep("SizingPeriod:DesignDay,", idf_block)
  ln_labels <- ln. + 1
  d_ddys <- data.frame(ini = ln., label = idf_block[ln_labels]) %>%
    tibble::tibble() %>%
    dplyr::mutate(label = gsub("^\\s+", "", label)) %>%
    dplyr::mutate(label = gsub("\\,\\s+\\!.*$", "", label))

  # 5. Determine location and group for each Design Day object
  d_ddys <- lapply(d_ddys$ini, determine_end_of_idf_block, idf_block) %>%
    plyr::ldply(data.frame) %>%
    tibble::tibble() %>%
    dplyr::mutate(group = ifelse(grepl("\\sHtg\\s|Hum\\_n\\s9",
      label,
      ignore.case = TRUE
    ), "heating",
    ifelse(grepl("\\sClg\\s",
      label,
      ignore.case = TRUE
    ), "cooling", "other")
    )) %>%
    dplyr::mutate(group = factor(group)) %>%
    dplyr::arrange(ini)

  # 6. Filter Design Day objects based on family
  d_ddys_fam <- d_ddys %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(min = min(ini), max = max(end))

  # 7. Extract relevant IDF sections based on family
  idf_ini <- idf_block[1:(min(d_ddys_fam$min) - 2)]
  idf_end <- idf_block[(max(d_ddys_fam$max) + 1):length(idf_block)]

  if (design_day_family == "heating") {
    idf_mid <- idf_block[(d_ddys_fam$min[d_ddys_fam$group == "heating"] - 1):(
      d_ddys_fam$max[d_ddys_fam$group == "heating"])]
  } else if (design_day_family == "cooling") {
    idf_mid <- idf_block[(d_ddys_fam$min[d_ddys_fam$group == "cooling"] - 1):(
      d_ddys_fam$max[d_ddys_fam$group == "cooling"])]
  } else {
    idf_mid <- idf_block[(min(d_ddys_fam$min) - 1):max(d_ddys_fam$max)]
  }

  # Combine idf objects
  idf_block <- c(idf_ini, "", idf_mid, idf_end)

  return(list(idf = idf_block, sum = d_ddys %>% dplyr::select(label, group)))
}

assign_terrain <- function(urban_classification, sparse_classification) {
  #' @title Assign Terrain Type
  #'
  #' @description
  #' This function determines the terrain type based on the provided urban and sparse classifications.
  #' It maps urban classifications to general terrain types and then overrides the result to 'Ocean'
  #' if the sparse classification is 'sparse'.
  #'
  #' @param urban_classification A character string representing the urban classification.
  #' @param sparse_classification A character string representing the sparse classification.
  #' @return A character string representing the assigned terrain type.


  # Map urban classification to terrain type
  res <- switch(as.character(urban_classification),
    "City centre" = "City",
    "Urban" = "Urban",
    "Suburban residential" = "Suburbs",
    "Rural residential" = "Country",
    "Village centre" = "Country",
    "Rural" = "Country",
    "Question Not Applicable" = "Suburbs",
    "Unknown" = "Suburbs"
  )

  # Override terrain type to 'Ocean' if sparse_classification is 'sparse'
  res <- ifelse(as.character(sparse_classification) == "sparse", "Ocean", res)

  return(res)
}

get_effective_zones <- function(housing_code, data_list) {
  #' @title Retrieve Effective Zones for Housing Code
  #'
  #' @description
  #' This function calculates effective zones based on the provided housing code and data.
  #' It retrieves necessary information from the `data_list`, including complementary data,
  #' archetype parameters, and zonal definitions.
  #'
  #' @param housing_code The housing code.
  #' @param data_list A list containing data for processing.
  #' @return A list with two elements:
  #'   * `info`: A data frame containing archetype parameters.
  #'   * `zones`: A list of effective zones.


  # Get complementary data
  df_cmp <- subset(data_list[["complementary"]], V001_HousingCode == housing_code)

  # Get archetype parameters
  df_id <- obtain_archetype_parameters(housing_code, data_list)
  floors <- df_id$vFlr
  attic <- as.character(df_id$vRff)
  base <- as.character(df_id$vBsm)
  attachment <- as.character(df_id$vAtt)
  flat <- as.character(df_id$vFlt)

  # Determine zonal levels
  l_zonal <- obtain_archetype_level(floors, base, attic, df_cmp)

  # Select effective zones based on flat type
  if (flat == "flat") {
    l_eff <- l_zonal$main
  } else {
    l_eff <- l_zonal$effective
  }

  # Return results
  return(list(info = df_id, zones = l_eff))
}

estimate_ground_temperature <- function(depth_meters, weather_data, thermal_conductivity = 1.21, soil_density = 1960, specific_heat_capacity = 840) {
  #' @title Estimate Ground Temperature
  #'
  #' @description
  #' Calculates the ground temperature at a given depth based on air temperature 
  #' data, and soil properties.
  #' 
  #' It uses:
  #'        conductivity \[W \/ m\·K\]
  #'        soil_density \[kg \/ m3\]
  #'        specific heat capacity \[J \/ kg\⋅K\]
  #' 
  #' @references Labs, K. "Regional analysis of ground and above-ground climate 
  #' conclusion", Underground Space Vol.7 pp037-65, 1982.
  #' 
  #' @param depth_meters Depth below the ground surface (m).
  #' @param weather_data A list containing weather data, including an EPW file.
  #' @param thermal_conductivity Soil thermal conductivity (W/m·K), default is 1.21.
  #' @param soil_density Soil density (kg/m3), default is 1960.
  #' @param specific_heat_capacity Soil specific heat capacity (J/kg·K), default is 840.
  #'
  #' @return A data frame with monthly average ground and air temperatures.


  # thermal diffusivity of soil [m^2 / day]
  soil_diff <- 8.64 * 10^4 * thermal_conductivity / (soil_density * specific_heat_capacity)

  # decrement (exponential rate) [-]
  decrement <- exp(-depth_meters * (pi / (365 * soil_diff))^0.5)

  # time lag [day/m]
  lag_t <- 0.5 * (365 / (pi * soil_diff))^0.5

  # Load climate data
  f_data <- weather_data$epw
  h_epw <- l_epw_header_units

  d_epw <- read.csv(f_data, skip = 8, header = F) %>% tibble::tibble()
  colnames(d_epw) <- h_epw$name
  d_epw <- d_epw %>%
    dplyr::select(-Datasource) %>%
    dplyr::mutate(Date = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>%
    dplyr::mutate(cumday = lubridate::yday(Date))

  # Calculate daily mean, max, and min temperatures
  d_daily <- d_epw %>%
    dplyr::group_by(cumday) %>%
    dplyr::summarise(
      daily_mean = mean(DryBulb, na.rm = TRUE),
      daily_max = max(DryBulb, na.rm = TRUE),
      daily_min = min(DryBulb, na.rm = TRUE)
    )

  # Average earth temperature [C]
  t_mean <- mean(d_epw$DryBulb)

  # Phase [day]
  t_o <- d_daily %>%
    dplyr::slice(which.min(daily_mean)) %>%
    dplyr::select(cumday) %>%
    unlist() %>%
    as.integer()

  # Amplitude [C]
  As <- 0.5 * (max(d_daily$daily_mean) - min(d_daily$daily_min))

  # Calculate ground temperature for each day
  d_Ts <- d_daily %>%
    dplyr::select(cumday) %>%
    dplyr::mutate(T_ground = t_mean - As * decrement *
      cos(2 * pi * (cumday - t_o - depth_meters * lag_t) / 365)) %>%
    plyr::join(d_epw, by = "cumday") %>%
    dplyr::select(Month, cumday, T_ground, DryBulb)

  # Calculate monthly averages
  d_monthly <- d_Ts %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(
      DryBulb = mean(DryBulb, na.rm = TRUE),
      T_ground = mean(T_ground, na.rm = TRUE)
    )

  return(d_monthly)
}

append_ground_temperatures <- function(temperature_data) {
  #' @title Append Ground-Related Temperatures
  #'
  #' @description
  #' Calculates and adds ground-related parameters
  #' (FCfactor, Shallow, Deep, Reflectance) to the input data frame.
  #'
  #' @param temperature_data A data frame containing ground temperature data.
  #' 
  #' @return The input data frame with added columns for
  #' FCfactor, Shallow, Deep, and Reflectance.


  # Define FCfactor values
  fcm <- c(4.16, 5.31, 7.51, 9.61, 13.58, 15.67, 16.24, 15.18, 12.74, 9.69, 6.69, 4.72)

  # Append ground-related parameters
  temperature_data <- temperature_data %>%
    dplyr::mutate(FCfactor = fcm, Shallow = 14, Deep = 14, Reflectance = 0.2)

  return(temperature_data)
}
