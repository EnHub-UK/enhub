
summarise_indexed_simulation <- function(i){
  #' @title Summarizes a simulation indexed by `i`
  #'
  #' @description This function processes a simulation summary file, extracts relevant data,
  #' and returns it in a structured format.
  #'
  #' @param i Index of the simulation to process.
  #'
  #' @return A list containing:
  #'   * `table`: A data frame with extracted simulation data.
  #'   * `index`: A data frame with the index and UUID.
  #'

  # File path to the simulation summary card
  path_card <- file.path(the$.enhub.paths$eval$A, i, 'summary_card.json')

  # Load the JSON data
  l_card <- jsonlite::fromJSON(path_card)

  # Extract dwelling information
  i_dwell <- l_card$house$cuboid %>% gsub('^MOD-','',.)
  i_sto <- as.integer(gsub('(\\d)(.*$)','\\1',i_dwell)) +
    grepl('WA',i_dwell) + grepl('WB',i_dwell)

  # Extract weather information
  i_epw <- l_card$weather$stat %>% gsub('/data.stat','',.) %>%
    basename() %>% paste0(., '.zip')

  # Create a list of simulation data
  l_dwell <- list(index = i,
                  uuid = l_card$uuid,
                  ehs_heat = l_card$house$energy_system$code,
                  ehs_code = l_card$ehs_base,
                  ehs_type = l_card$house$typology$code,
                  ehs_region = tolower(l_card$weather$region_in_survey),
                  cuboid = l_card$house$cuboid,
                  epoch = l_card$house$epoch$code,
                  no_storeys = i_sto,
                  hot_water = '-',
                  radiator_type = '-',
                  glazing_coverage = '-',
                  thermostat_temperature = '-',
                  wall_type = '-',
                  wall_u_value = '-',
                  weather = i_epw,
                  uuid_pair = '-',
                  heat_group = '-')

  # Convert list to data frame and split into table and index
  d_dwell <- data.frame(l_dwell)
  d_index <- d_dwell %>% dplyr::select(index, uuid)
  d_dwell <- d_dwell %>% dplyr::select(-index)

  return(list(table=d_dwell, index=d_index))
}

rename_indexed_simulation <- function(i, df){
  #' @title Renames a simulation based on index and UUID
  #'
  #' @description This function renames a simulation in a data frame based on its index and UUID.
  #'
  #' @param i Index of the simulation to rename.
  #' @param df Data frame containing simulation data.
  #'
  #' @return The modified data frame.
  
  d_sub <- df %>% dplyr::filter(index == i)
  rename_model_with_uuid(d_sub$index)
}

obtain_most_frequent <- function(d){
  #' @title Obtains the most frequent row in a data frame
  #'
  #' @description This function selects the row with the highest value in the `V002_DwellingWeight` column
  #' and removes that column from the result.
  #'
  #' @param d Data frame to process.
  #'
  #' @return The most frequent row without the `V002_DwellingWeight` column.
  
  d_out <- d %>%
    dplyr::arrange(dplyr::desc(V002_DwellingWeight)) %>%
    dplyr::slice(1) %>%
    dplyr::select(-V002_DwellingWeight)
  
  return(d_out)
}
