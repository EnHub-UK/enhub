
# schedules --------------------------------------------------------------------

obtain_zonal_setpoint_schedules <- function(df){
  #' @title Extract and format zonal setpoint schedules
  #'
  #' @description This function extracts and formats zonal setpoint
  #' schedules from a data frame.
  #'
  #' @param df: A data frame containing columns for heating setpoints (htg_sp),
  #' cooling setpoints (clg_sp), heating schedules (htg_schedules), and
  #' cooling schedules (clg_schedules).
  #'
  #' @return: A list of lists containing formatted zonal setpoint schedules.
  #'

  # Helper function to format individual setpoint schedule
  format_setpoint_schedule <- function(l_sp){

    # Convert setpoint data to vector
    l_sp <- l_sp %>% as.vector()

    l_res <- list(
      name=l_sp$schedules,                         # Assign schedule name
      limits='Temperature',                        # Set limit type (constant)
      blocks = list(
        list(
          range = 'Through: 12/31',                # Default range
          applicable = 'For: AllDays',             # Default applicability
          values = paste('Until: 24:00,', l_sp$sp) # Define values
        )
      )
    )

    # Return dictionary
    return(l_res)
  }

  # Extract heating setpoint data with distinct values
  df_sp_htg <- df %>%
    dplyr::select(htg_sp, htg_schedules) %>%
    dplyr::distinct() %>%
    dplyr::rename(sp = htg_sp, schedules = htg_schedules)

  # Extract cooling setpoint data with distinct values
  df_sp_clg <- df %>%
    dplyr::select(clg_sp, clg_schedules) %>%
    dplyr::distinct() %>%
    dplyr::rename(sp = clg_sp, schedules = clg_schedules)

  # Combine heating and cooling data
  df_sp <- df_sp_htg %>% dplyr::bind_rows(df_sp_clg)

  # Split data frame into a list by row
  l_sp <- split(df_sp, seq(1, nrow(df_sp)))
  names(l_sp) <- NULL

  # Apply formatting function to each setpoint schedule
  l_res <- lapply(l_sp, format_setpoint_schedule)

  # Return list of parsed data frame
  return(l_res)
}

define_selected_system_schedule <- function(i_code, l_loop, l_sys, l_scd){
  #' @title Define selected system schedule
  #'
  #' @description This function defines a selected system schedule based on
  #' the provided energy system code and loop data.
  #'
  #' @param i_code The energy system code.
  #' @param l_loop A list containing loop parameters.
  #' @param l_sys A list of loop parameters
  #' @param l_scd A list of generated schedules
  #'
  #' @return A list containing the defined schedule.
  #'

  # a) parse inputs

  # Decode energy system codes and assign to loop
  l_loop$heating_codes <- decode_energy_system(i_code)

  # Define loop parameters
  l_loops <- define_loop_parameters(l_loop$loop)


  # b) define parameters

  l_res <- list()
  k_item <- 0

  # Define hot-water temperature
  k_main_loop_sp <- the$GLOBAL_PAR$heating_water_temperature
  k_dhw <- the$GLOBAL_PAR$domestic_hot_water_temperature
  k_dhw_tank <- l_sys$dhw_loop$temperature_exit

  # Define set-point temperatures
  if(l_loop$heating_codes$msh %in% c(14:15)){
    k_hp_tech <- the$GLOBAL_PAR$heating_hp_variation
    k_mwl_sp <- ifelse(k_hp_tech == 'high-temp', 68.445645, 48.445645)
    k_vessel <- ifelse(k_hp_tech == 'high-temp', 55.283451, 53.283451)
    k_main_loop_sp <- ifelse(k_hp_tech == 'high-temp', 69.445645, 54.045645)
    k_dhw <- ifelse(k_hp_tech == 'high-temp', k_dhw, 57.445645)
  }


  # c) define schedules

  # Create schedule for main water loop set-point temperature
  l_base <- list(
    name='schedule:main-water-loop--setpoint-temperature',
    limits='Any Number',
    blocks=list(list(
      a_range='Through: 12/31',
      b_applicable='For: AllDays',
      c_values=paste('Until: 24:00, ', k_main_loop_sp))))

  # Update list of schedules
  l_res[[k_item <- k_item + 1]] <- l_base


  # Create schedule for hot-water loop set-point temperature
  l_dhw <- list(
    name='schedule:hot-water-loop--setpoint-temperature',
    limits='Any Number',
    blocks=list(list(
      a_range='Through: 12/31',
      b_applicable='For: AllDays',
      c_values=paste('Until: 24:00, ', k_dhw))))

  # Update list of schedules
  l_res[[k_item <- k_item + 1]] <- l_dhw


  # Create schedule for hot-water loop set-point temperature
  l_dhw_tank <- list(
    name='schedule:dhw-water-heater--setpoint-temperature',
    limits='Any Number',
    blocks=list(list(
      a_range='Through: 12/31',
      b_applicable='For: AllDays',
      c_values=paste('Until: 24:00, ', k_dhw_tank))))

  # Update list of schedules
  l_res[[k_item <- k_item + 1]] <- l_dhw_tank


  # Create schedule for heat-pump setpoint temperature
  if(l_loop$heating_codes$msh %in% c(14)){
    l_append <- list(
      name='schedule:hw-heat-pump--setpoint-temperature',
      limits='Any Number',
      blocks=list(list(
        a_range='Through: 12/31',
        b_applicable='For: AllDays',
        c_values=paste('Until: 24:00, ', k_mwl_sp))))

    # Update list of schedules (within condition)
    l_res[[k_item <- k_item + 1]] <- l_append
  }


  # Create schedule for heat-pump vessel setpoint temperature
  if(l_loop$heating_codes$msh %in% c(15)){
    l_append <- list(
      name='schedule:hw-heat-pump-vessel--setpoint-temperature',
      limits='Any Number',
      blocks=list(list(
        a_range='Through: 12/31',
        b_applicable='For: AllDays',
        c_values=paste('Until: 24:00, ', k_vessel))))

    # Update list of schedules (within condition)
    l_res[[k_item <- k_item + 1]] <- l_append
  }


  # Create schedule for heat-pump vessel setpoint temperature
  if(any(grepl('hp-water-loop--setpoint-temperature',l_scd))){
    l_append <- list(
      name='schedule:hp-water-loop--setpoint-temperature',
      limits='Any Number',
      blocks=list(list(
        a_range='Through: 12/31',
        b_applicable='For: AllDays',
        c_values=paste('Until: 24:00, ', k_main_loop_sp))))

    # Update list of schedules (within condition)
    l_res[[k_item <- k_item + 1]] <- l_append
  }


  # Create schedule for heat-pump vessel setpoint temperature
  if(any(grepl('day-radiant-heat',l_scd))){
    l_append <- list(
      name='schedule:day-radiant-heat',
      limits='Any Number',
      blocks=list(list(
        a_range='Through: 12/31',
        b_applicable='For: AllDays',
        c_values=paste('Until: 24:00, ', 17.5))))

    # Update list of schedules (within condition)
    l_res[[k_item <- k_item + 1]] <- l_append
  }


  # Create schedule for main water loop setpoint temperature
  if(l_loop$heating_codes$msh %in% c(3)){
    l_append <- list(
      name='schedule:hw-back-boiler--setpoint-temperature',
      limits='Any Number',
      blocks=list(list(
        a_range='Through: 12/31',
        b_applicable='For: AllDays',
        c_values=paste('Until: 24:00, ', 70.4552645))))

    # Update list of schedules (within condition)
    l_res[[k_item <- k_item + 1]] <- l_append
  }


  # Return the defined schedule
  return(l_res)
}

generate_schedule_limits <- function(l_sets, l_sup = l_idf_blocks){
  #' @title Create IDF block for ScheduleTypeLimits objects based on dictionaries
  #'
  #' @description This function generates an IDF block representing a
  #' schedule type limits object using a dictionary of limits (`l_sets`).
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sets: Dictionary of schedule type limits
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_val, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    k_fields <- l_val %>% unlist() %>% as.character()

    # Prepend object type to parameter list
    l_block_set <- c(i_act, k_fields)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- 'ScheduleTypeLimits'

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_sets, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_schedule_compact <- function(l_sets, l_sup = l_idf_blocks){
  #' @title Create IDF block for Schedule:Compact objects based on dictionaries
  #'
  #' @description This function generates an IDF block representing a compact
  #' style schedule using a dictionary of schedules (`l_sets`). It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sets: Dictionary of schedules
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_val, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    k_fields <- l_val$blocks %>% unlist() %>% as.character()
    l_comment <- c(l_comment[1:2], paste('Field', seq(1:length(k_fields))))
    l_values <- c(l_val$name, l_val$limits, k_fields)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- 'Schedule:Compact'

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_sets, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

extract_schedule_information <- function(idf){
  #' @title Extract schedule information from an IDF file
  #'
  #' @description This function extracts schedule names from an IDF file.
  #'
  #' @param idf: A character vector representing the lines of an IDF file.
  #'
  #' @return: A character vector containing unique and sorted schedule names.
  #'

  # Identify lines containing "schedule:" definitions
  txt <- grep("^\\s+schedule\\:", idf, value = TRUE)

  # Clean schedule names by removing leading whitespace and trailing delimiters
  txt <- txt %>%
    gsub('^\\s+','',.) %>%                # Remove leading whitespace
    gsub('(.*)[\\,\\;](.*$)','\\1',.) %>% # Remove trailing delimiters (",";)
    unique() %>%                          # Remove duplicates
    sort()                                # Sort alphabetically

  # Return the list of extracted schedule names
  return(txt)
}
