
randomise_profiles <- function(k="det"){
  #' @title Randomises profiles used for scheduling
  #'
  #' @description Sets the random seed for profile randomisation.
  #'
  #' @param k Character string indicating the type of randomisation.
  #'   Default is "det" for deterministic behaviour.
  #'   If "sto", sets the seed to NULL for stochastic behavior.

  if(k=="sto"){
    set.seed(NULL)
  }else{
    set.seed(1234)
  }
}

generate_schedule__typical <- function(sel, var_int=1, days=365){
  #' @title Generate Schedule with Typical Profile Variation
  #'
  #' @description This function creates a schedule with a typical profile, incorporating
  #' variation based on provided parameters.
  #'
  #' @param sel A vector representing the typical profile.
  #' @param var_int Numeric value controlling the amount of variation.
  #'   Higher values lead to larger deviations. (default: 1)
  #' @param days Integer specifying the number of days in the schedule.
  #'   (default: 365)
  #'
  #' @return A numeric vector representing the generated schedule.

  # Convert selection vector to numeric
  df_fraction <- as.vector(as.numeric(unlist(sel)))

  # Inner function to generate a single day's profile
  generate_profile <- function(day, df_frac, k){

    # Calculate variation statistics
    var.med <- median(df_frac)
    var.std <- sd(df_frac)
    var.low <- min(df_frac) * 1.00001
    var.top <- max(df_frac) * (1 - 0.00001)

    # Calculate variation factor
    var.factor <- var.std / (var.top - var.low) * k

    # Generate profile with jitter
    df_gen <- abs(jitter(df_frac, var.factor))

    # Handle edge cases (median equal to min or max)
    if(var.med==var.low | var.med==var.top){
      df_bool <- as.logical((df_frac - min(df_frac)) / (max(df_frac) - min(df_frac)))
      df_gen <- df_gen * df_bool
      if(var.med==var.low){
        df_gen[!df_bool] <- var.low
      }else{
        df_gen[!df_bool] <- var.top
      }
    }

    # Round and return generated profile
    return(round(df_gen * 100, 2) / 100)
  }

  # Generate profiles for all days and combine into data frame
  df_profile <- lapply(1:days, generate_profile, df_fraction, var_int)
  df_profile <- plyr::ldply(df_profile, data.frame)
  df_profile <- as.numeric(unlist(df_profile))

  # Handle missing values
  df_profile[is.na(df_profile)] <- 0

  return(df_profile)
}

generate_usage_schedules <- function(ids, i_int, i_sched, df_dem,
                              df_base=s_tus$essentials,
                              df_typical=s_uk_heff$typical,
                              df_period=the$l_days$period){
  #' @title Generate Usage Schedules
  #'
  #' @description This function generates usage schedules for different scenarios.
  #'
  #' @param ids Character vector specifying identifiers for each schedule.
  #' @param i_int Character containing energy use intensity parameter.
  #' @param i_sched Character indicating the scheduling type ("det" for deterministic, "sto" for stochastic).
  #' @param df_dem Data frame containing demand data.
  #' @param df_base Data frame containing base appliance data (default: s_tus$essentials).
  #' @param df_typical Data frame containing typical appliance profiles (default: s_uk_heff$typical).
  #' @param df_period Data frame containing time periods (default: the$l_days$period).
  #'
  #' @return A list named `l_profiles` containing two data frames:
  #'   - schedules: Daily usage profiles.
  #'   - appliances: Base appliance data with power calculations.

  # Create output directories
  path_to <- file.path(the$.enhub.paths$eval$A, ids)
  r_rm(path_to); r_mkdir(path_to)
  path_schd <- file.path(path_to, "default_schedules.csv")
  path_apps <- file.path(path_to, "typical_appliances.csv")

  # Determine energy mode and intensity
  i_mode <- switch(i_int, 'normal' = 'md', 'high' = 'hg', 'low' = 'lo')
  i_intensity <- switch(i_int, 'normal' = 1.0, 'high' = 6, 'low' = 1/6)

  # Identify electric and gas appliances
  df_gas <- as.numeric(
    subset(df_dem, variable=='cooking_gas', select = yearly))
  df_not_gas <- sum(df_dem$yearly) - df_gas
  i_cook <- grep("^(hob|oven)",df_base$name)
  i_rest <- grep("^(hob|oven)",df_base$name, invert = T)

  # Scale base appliance usage based on gas presence
  df_base$mainf <- 'ele'
  if(df_gas>0){
    df_base$mainf[i_cook] <- 'gas'
    df_base$usage__kwh___yr[i_cook] <-
      df_base$usage__kwh___yr[i_cook] /
      sum(df_base$usage__kwh___yr[i_cook]) * sum(df_gas)
    df_base$usage__kwh___yr[i_rest] <-
      df_base$usage__kwh___yr[i_rest] /
      sum(df_base$usage__kwh___yr[i_rest]) * sum(df_not_gas)
  }else{
    df_base$usage__kwh___yr <- df_base$usage__kwh___yr /
      sum(df_base$usage__kwh___yr) * sum(df_not_gas)
  }

  # Calculate power and usage per area and person
  df_base$power__W <- df_base$usage__kwh___yr * 1000 / 365
  df_base$power__W__m2 <- df_base$power__W / 92
  df_base$power__W__ppl <- df_base$power__W / 2.3

  # Prepare profiles for merging
  df_typical <- data.frame(tid = lubridate::hour(df_typical$name), df_typical)
  df_sched_template <- data.frame(date=df_period$Hour, tid=0:23)

  # Generate profiles based on scheduling type
  if(i_sched == 'det'){
    df_events <- plyr::join(df_sched_template, df_typical, by=c('tid'))
    df_events$tid <- df_events$name <- NULL

  }else if(i_sched == 'sto'){
    df_events <- df_typical
    df_events <- df_events[, grep("^x",colnames(df_events), ignore.case = T)]

    df_events <- cbind(
      data.frame(date = df_period$Hour),
      apply(df_events, 2, generate_schedule__typical, i_intensity)
    )

  }else{

    stop("Oops... something's wrong with this type of schedule. Please check!")

  }

  # Prepare and export profile
  colnames(df_events) <- gsub("date","Hour",colnames(df_events))

  # Parse, re-format and export profiles
  df_events[is.na(df_events)] = 0
  (df_events <- tibble::tibble(df_events))

  # Parse, re-format and export reference table
  (df_base <- tibble::tibble(df_base))

  # Return tables for further processing
  l_profiles <- list(df_events, df_base)
  names(l_profiles) <- c('schedules','appliances')

  invisible(l_profiles)
}

assign_appliances_profile <- function(l_sched, df_zones){
  #' @title Assigns Appliances to Zones
  #'
  #' @description Assigns appliances to specific zones based on their type.
  #'
  #' @param l_sched A list containing appliance data.
  #' @param df_zones A data frame defining zones and their characteristics.
  #'
  #' @return A data frame with appliances and assigned zones.

  # Regular expression for filtering columns
  var.grep <- '(date|hour|day|min|month|peo*|adul*|youn*|kids*)'

  df <- l_sched$appliances

  # Assign appliances to cooking zone
  df_cook <- df %>%
    dplyr::filter(type %in% c('Cooling','Cooking','Washing appliance')) %>%
    dplyr::rowwise() %>% dplyr::mutate(zone = sample(df_zones$cooking$zone, 1)) %>%
    dplyr::ungroup() %>% dplyr::relocate(id,zone)

  # Assign appliances to hygiene zone
  df_wash <- df %>%
    dplyr::filter(type %in% c('Washing / showers')) %>%
    dplyr::rowwise() %>% dplyr::mutate(zone = sample(df_zones$hygiene$zone, 1)) %>%
    dplyr::ungroup() %>% dplyr::relocate(id,zone)

  # Assign appliances to other zones
  k_rest_zones <-
    c(df_zones$gather$zone, df_zones$rest$zone, df_zones$other$zone)
  df_other <- df %>%
    dplyr::filter(!type %in% c('Cooling','Cooking',
                        'Washing appliance', 'Washing / showers')) %>%
    dplyr::rowwise() %>% dplyr::mutate(zone = sample(k_rest_zones, 1)) %>%
    dplyr::ungroup() %>% dplyr::relocate(id,zone)

  # Combine assigned appliances
  df_res <- df_cook %>% dplyr::bind_rows(df_wash) %>%
    dplyr::bind_rows(df_other) %>% dplyr::arrange(id)

  return(df_res)
}

assign_presence_by_people <- function(i, d_in, d_ref) {
  #' @title Assign presence to activity zones for a specific person
  #' @description This function assigns presence probabilities to activity zones
  #' based on a reference data frame and a person identifier.
  #'
  #' @param i The person identifier.
  #' @param d_in A data frame containing presence probabilities for the person.
  #' @param d_ref A data frame containing reference data on activity zones.
  #' @return A data frame with presence probabilities for each zone across time.

  # Select presence data for the person
  d_out <- d_in %>% dplyr::select(Hour, dplyr::matches(i))

  # Combine zone information from reference data
  k_zone <- c(
    d_ref$gather$zone,
    d_ref$rest$zone,
    d_ref$cooking$zone,
    d_ref$hygiene$zone,
    d_ref$other$zone
  )

  k_prob <- c(
    rep(9, length(d_ref$gather$zone)),
    rep(6, length(d_ref$rest$zone)),
    rep(3, length(d_ref$cooking$zone)),
    rep(1, length(d_ref$hygiene$zone)),
    rep(2, length(d_ref$other$zone))
  )

  k_prob <- k_prob / sum(k_prob)

  # Sample zones based on probabilities
  k_zone <- c(sample(k_zone, dim(d_out)[1], replace = TRUE, prob = k_prob))

  # Add zone information to presence data
  d_out$zones <- k_zone

  # Reshape data for presence probabilities by zone
  colnames(d_out) <- c("hour", "value", "zone")
  d_out <- d_out %>%
    tidyr::pivot_wider(names_from = zone, values_from = value)
  d_cols <- grep("(hour|time)", colnames(d_out), ignore.case = TRUE, invert = TRUE)

  # Handle missing values and format output
  d_out[, d_cols][is.na(d_out[, d_cols])] <- 0
  colnames(d_out) <- paste0(i, "__", colnames(d_out))
  colnames(d_out) <- gsub("^-_", "generic_", colnames(d_out))
  d_out <- tibble::tibble(d_out)
  colnames(d_out)[grep("hour", colnames(d_out))] <- "Hour"

  return(d_out)
}

assign_presence_by_zone <- function(zoneId, zoneData) {
  #' @title Assigns presence based on zone data
  #'
  #' @description Calculates presence based on the sum of zone-specific presence
  #'   values, rescales the values, and converts them to binary
  #'   presence/absence.
  #'
  #' @param zoneId The zone identifier.
  #' @param zoneData The data frame containing zone-specific presence data.
  #' @return A numeric vector indicating presence (1) or absence (0) for each
  #'   time step.

  # Extract zone-specific data
  d_out <- zoneData %>% dplyr::select(ends_with(paste0("__", zoneId)))

  # Calculate total presence, rescale and binarize
  d_out <- d_out %>%
    dplyr::mutate(presence = rowSums(d_out)) %>%
    dplyr::mutate(presence = (presence - min(presence, na.rm = TRUE)) / (
      max(presence, na.rm = TRUE) - min(presence, na.rm = TRUE))) %>%
    dplyr::mutate(presence = as.integer(as.logical(presence))) %>%
    dplyr::pull(presence)

  return(d_out)
}

assign_occupants_profile <- function(df_presence, df_zones){
  #' @title Assigns Occupancy Profiles to Zones
  #'
  #' @description Assigns occupancy profiles to each zone based on overall presence data.
  #'
  #' @param df_presence A data frame containing presence data across hours.
  #' @param df_zones A data frame defining zones and their characteristics.
  #'
  #' @return A data frame with presence status for each zone and hour.

  # Identify non-hour columns for processing
  df_res <- colnames(df_presence)[grep('hour', colnames(df_presence),
                                        ignore.case = T,invert = T)]

  # Assign presence by people for each non-hour column
  df_res <- lapply(df_res, assign_presence_by_people, df_presence, df_zones)

  # Combine presence profiles across columns
  df_res <- plyr::join_all(df_res, by='Hour') %>% tibble::tibble()

  # Get unique zone labels
  lbl_zones <- unique(df_zones$all_zones$zone)

  # Assign presence by zone for each unique zone
  df_status <- lapply(lbl_zones, assign_presence_by_zone, df_res)

  # Combine zone presence data into a data frame
  df_status <- as.data.frame(do.call(cbind, df_status))
  colnames(df_status) <- paste0('status_', lbl_zones)

  # Combine presence by people and zone information
  df_out <- cbind(df_res, df_status)

  # Handle missing values in non-hour columns
  k_col <- grep("(hour|time)", colnames(df_out), ignore.case = T, invert = T)
  df_out[, k_col][is.na(df_out[, k_col])] <- 0

  # Convert to tibble for improved structure
  df_out <- df_out %>% tibble::tibble()

  return(df_out)
}

make_block_schedule <- function(l_zon, d_ref, l_sup = l_idf_blocks){
  #' @title Creates an IDF Block Schedule
  #'
  #' @description Constructs an IDF block schedule based on provided zones, reference data, and supplementary information.
  #'
  #' @param l_zon A list of zones.
  #' @param d_ref Reference data for block creation.
  #' @param l_sup Supplementary information (default: l_idf_blocks).
  #'
  #' @return A list containing the constructed IDF block schedule.

  # Helper to construct an individual IDF block
  make_block <- function(iz, block, ipar, com) {
    #' @title Creates an IDF Block
    #'
    #' @description Constructs an individual IDF block with specified parameters.
    #'
    #' @param iz Zone identifier.
    #' @param block Block type.
    #' @param ipar Input parameters.
    #' @param com Comments for the block.
    #'
    #' @return A list representing the IDF block.

    # Define block parameters
    lbl_block <- block
    lbl_obj <- iz
    lbl_frc <- 'Fraction'
    lbl_file <- 'default_schedules.csv'
    lbl_col <- grep(iz, colnames(ipar), fixed = TRUE) + 1
    lbl_skip <- 1
    lbl_hour <- dim(ipar)[1]
    lbl_seps <- 'Comma'
    lbl_intp <- 'No'
    lbl_mini <- 60
    lbl_aday <- 'Yes'

    # Create block object
    obj <- c(lbl_block, lbl_obj, lbl_frc, lbl_file, lbl_col,
             lbl_skip, lbl_hour, lbl_seps, lbl_intp, lbl_mini, lbl_aday)

    # Add comments to the block
    res <- add_idf_comment(obj, c("", com))

    return(res)
  }

  # Set active block type and create section header
  i_active <- "Schedule:File"
  l_section <- make_idf_block_title(i_active)
  l_com <- l_sup[[i_active]]

  # Create blocks for each zone
  l_obj <- lapply(l_zon, make_block, i_active, d_ref, l_com)

  # Combine blocks into a list
  l_res <- c("", l_section, "", unlist(l_obj), "")

  return(l_res)
}

generate_schedule_idf <- function(ids, df_sched, df_apps){
  #' @title Generates IDF Schedule
  #'
  #' @description Creates an IDF schedule based on provided schedules and appliances data.
  #'
  #' @param df_sched Data frame containing schedule data.
  #' @param df_apps Data frame containing appliance data.
  #' @param path_idf Character string specifying the output path for IDF files.
  #'
  #' @return A list containing the generated IDF schedule.


  # Define file paths
  path_idf <- file.path(the$.enhub.paths$eval$A, ids)
  path_sched <- file.path(path_idf, "default_schedules.csv") %>%
    normalizePath(winslash = "/", mustWork = F)
  path_apps <- file.path(path_idf, "typical_appliances.csv") %>%
    normalizePath(winslash = "/", mustWork = F)


  # Extract schedule labels
  lbl_sched <- names(df_sched)[grep("^Hour", names(df_sched), invert = T)]

  # Create IDF schedule blocks
  idf_out <- make_block_schedule(lbl_sched, df_sched)


  # Update schedule and appliance files
  write.csv(df_sched, file = path_sched)
  r_zip(path_sched)
  write.csv(df_apps %>% dplyr::select(id, zone, name, type, supervised, mainf),
            file = path_apps)

  return(idf_out)
}
