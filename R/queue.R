
generate_table_of_queue_archetypes <- function(p_core=the$.enhub.paths){
  #' @title Generates a table of queued archetypes
  #'
  #' @description Reads and processes data on queued archetypes, evaluated pairs, and the evaluation index.
  #'
  #' @param p_core A list containing core directory paths (default: the$.enhub.paths).
  #' @return A list containing various data frames and file paths:
  #'   * `index`: A data frame containing the evaluation index.
  #'   * `queue`: A data frame containing queued archetypes with assigned UUIDs.
  #'   * `new_pairs`: A data frame containing newly created pairs for evaluation.
  #'   * `cloud_pairs`: A data frame containing evaluated pairs (downloaded).
  #'   * `file_queue`: The file path to the queued archetypes data file.
  #'   * `file_pairs`: The file path to the evaluated pairs data file.
  #'   * `file_index`: The file path to the evaluation index data file.
  #'
  #' @details
  #' This function processes data related to building archetypes for evaluation.
  #' It reads local and remote data sources, performs cleaning and filtering,
  #' and generates tables containing queued archetypes, newly created pairs for
  #' evaluation, and downloaded data on previously evaluated pairs.

  # Define local file paths
  f_queue <- file.path(p_core$enhub, 'myData/_sessions/_queue.csv')
  f_pairs <- file.path(p_core$enhub, 'myData/_sessions/_pairs.csv')
  f_index <- file.path(p_core$enhub, 'myData/_sessions/_index.csv')

  # Define remote data URLs
  url_gcp <- 'https://storage.googleapis.com/enhub-data/'
  f_gcp <- 'Evaluation/pairs.csv'
  f_url <- paste0(url_gcp, f_gcp)
  f_gci <- 'Evaluation/index.csv'
  f_uri <- paste0(url_gcp, f_gci)

  # Download and read remote evaluated pairs data
  f_loc <- tempfile()
  download.file(f_url, f_loc, quiet = F, cacheOK = T, method = 'wget')
  d_evaluated <- read.csv(f_loc, row.names = 1) %>% tibble::tibble()

  # Download and read remote evaluation index data
  f_loc <- tempfile()
  download.file(f_uri, f_loc, quiet = F, cacheOK = T, method = 'wget')
  d_index <- read.csv(f_loc, row.names = 1) %>% tibble::tibble()

  # Read and clean local queued archetypes data
  d_origin <- d_queue <-
    read.csv(f_queue, row.names = 1, colClasses = "character") %>%
    as.data.frame() %>% tibble::tibble() %>%
    dplyr::mutate(no_storeys = as.integer(no_storeys)) %>%
    dplyr::filter(ehs_code != 'X0000001')

  # Identify and remove duplicate archetypes
  d_origin_a <- d_index %>% dplyr::select(-uuid, -available)
  d_origin_b <- d_origin %>% dplyr::select(-uuid)
  d_origin_a$exists <-
    do.call(paste0, d_origin_a) %in% do.call(paste0, d_origin_b)
  d_origin_c <- d_origin_a %>% dplyr::filter(exists == TRUE) %>% dplyr::select(ehs_code)
  d_origin <- d_queue <- d_origin %>%
    dplyr::filter(!ehs_code %in% d_origin_c$ehs_code) %>%
    dplyr::mutate(uuid_pair = uuid())

  # read table of queued pairs
  d_pair <- d_queue %>%
    dplyr::mutate(tmp = uuid) %>%
    dplyr::mutate(uuid = uuid_pair) %>%
    dplyr::mutate(uuid_pair = tmp) %>%
    dplyr::select(-tmp) %>%
    dplyr::mutate(ehs_heat = 'MSH15DHW8')

  # Create new pairs for evaluation
  d_pairs <- d_queue %>% dplyr::select(uuid, ehs_code, uuid_pair)

  d_queue <- d_queue %>% dplyr::bind_rows(d_pair) %>% dplyr::distinct() %>% dplyr::arrange(ehs_code)

  d_same <- d_queue %>% dplyr::select(ehs_code, ehs_heat) %>%
    dplyr::group_by(ehs_code) %>% summarize(distinct_points = n_dplyr::distinct(ehs_heat))

  d_same <- d_queue %>% plyr::join(d_same) %>%
    dplyr::filter(distinct_points<2) %>% dplyr::select(uuid, ehs_code, ehs_heat)

  k_redundant <- d_same$uuid[!d_same$uuid %in% d_origin$uuid]

  d_queue <- d_queue %>% dplyr::filter(!uuid %in% k_redundant) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(heat_group = parse_space_heating_group(ehs_heat)) %>%
    dplyr::ungroup()

  # Return list of summaries
  return(list(index=d_index, queue=d_queue,
              new_pairs=d_pairs, cloud_pairs=d_evaluated,
              file_queue=f_queue, file_pairs=f_pairs, file_index=f_index))
}

reassign_glazing <- function(li, dhl=s_ehs_2011_ext$other.heat.loss.elements){
  #' @title Reassigns glazing types
  #'
  #' @description Reassigns glazing types based on the provided level of glazing.
  #'
  #' @param li A character string indicating the level of glazing: "ALL", "MORE_THAN_HALF", "LESS_THAN_HALF", or any other value.
  #' @param dhl A data frame containing heat loss elements (default: s_ehs_2011_ext$other.heat.loss.elements).
  #'
  #' @return The modified data frame with reassigned glazing types.

  if(li=='ALL'){
    dhl$D068_Windows1Type <- "double glazing"
    dhl$D070_Windows2Type <- "double glazing"
    dhl$D071_Windows1FrameType <- "double-glazed- UPVC"
    dhl$D072_Windows2FrameType <- "double-glazed- UPVC"
  }else if(li=='MORE_THAN_HALF'){
    dhl$D068_Windows1Type <- "double glazing"
    dhl$D070_Windows2Type <- "double glazing"
    dhl$D071_Windows1FrameType <- "mixed types"
    dhl$D072_Windows2FrameType <- "single-glazed- wood casement"
  }else if(li=='LESS_THAN_HALF'){
    dhl$D068_Windows1Type <- "single glazing"
    dhl$D070_Windows2Type <- "double glazing"
    dhl$D071_Windows1FrameType <- "single-glazed- wood casement"
    dhl$D072_Windows2FrameType <- "single-glazed- wood casement"
  }else{
    dhl$D068_Windows1Type <- "single glazing"
    dhl$D070_Windows2Type <- "single glazing"
    dhl$D071_Windows1FrameType <- "single-glazed- wood casement"
    dhl$D072_Windows2FrameType <- "single-glazed- wood casement"
  }

  return(dhl)
}

reassign_wall_type <- function(li, dhl=s_ehs_2011_ext$geometry){
  #' @title Reassigns wall type based on input value
  #'
  #' @description This function reassigns the `D077_ExternalWallConstruction` field in the provided
  #' `dhl` data frame based on the input value `li`. The mapping between `li` and
  #' wall types is defined within the function.
  #'
  #' @param li Character string indicating the wall type.
  #' @param dhl Spatial data frame containing the `D077_ExternalWallConstruction` field.
  #'   Defaults to `s_ehs_2011_ext$geometry`.
  #' @return The modified `dhl` data frame with updated wall types.
  #'
  #' @references s_chm$CHMtbl_EHS_CHM_wallType[1:16,]
  #'

  wt <- dhl$D077_ExternalWallConstruction

  if(li=="CAVITY_WITH_INSULATION"){
    wt <- 10
  }else if(li=="CAVITY_WITHOUT_INSULATION"){
    wt <- 9
  }else if(li=="SOLID_WITH_INSULATION"){
    wt <- 4
  }else if(li=="SOLID_WITHOUT_INSULATION"){
    wt <- 3
  }

  dhl$D077_ExternalWallConstruction <- wt

  return(dhl)
}

reassign_thermostat_temperature <- function(li, l_par = the$GLOBAL_PAR){
  #' @title Reassigns thermostat set points based on input category
  #'
  #' @description This function reassigns the `set_point_heating` and `set_point_cooling`
  #' parameters within the `l_par` list based on the provided input category `li`.
  #'
  #' @param li Character string indicating the temperature category.
  #' @param l_par List of parameters, defaulting to `the$GLOBAL_PAR`.
  #' @return The modified `l_par` list with updated set points.

  # Determine set points based on temperature category
  sp <- switch(li,
               "BELOW_15" = c(14.4083, 28.2102),
               "FROM_15_TO_17" = c(15.7083, 27.1102),
               "FROM_18_TO_21" = c(19.5083, 27.0102),
               "FROM_22_TO_24" = c(22.8083, 27.0002),
               "MORE_THAN_25" = c(25.6083, 26.0802)
  )

  # Update set points in l_par
  l_par$set_point_heating <- sp[1]
  l_par$set_point_cooling <- sp[2]

  return(l_par)
}

assign_custom_inputs <- function(l_i, l_defs, s_main){
  #' @title Assigns custom inputs to model parameters
  #'
  #' @description This function assigns custom input values to model parameters within the
  #' provided `l_defs` and `s_main` objects based on the input list `l_i`.
  #'
  #' @param l_i List of custom input values.
  #' @param l_defs List of model default parameters.
  #' @param s_main Model data structure.
  #' @return A list containing the modified `s_main`, `l_defs`, and original `l_i` inputs.


  # Assign weather file path
  if(!is.null(l_i$weather)){
    l_defs$use_weather_file <- gsub('\\.zip','',l_i$weather)
  }

  # Assign UUID
  if(!is.null(l_i$uuid)){
    l_defs$uuid <- l_i$uuid
  }

  # Assign heating system code
  if(!is.null(l_i$energycode)){
    l_defs$heating_system_code <- l_i$energycode
  }

  # Assign wall U-value
  if(!is.null(l_i$wall_u_value)){
    l_defs$wall_transmittance <- as.numeric(l_i$wall_u_value)
  }

  # Assign thermostat set points
  if(!is.null(l_i$thermostat_temperature)){
    l_temp <- reassign_thermostat_temperature(l_i$thermostat_temperature)
    l_defs$set_point_cooling <- l_temp$set_point_cooling
    l_defs$set_point_heating <- l_temp$set_point_heating
  }

  # Assign energy code to summarized data
  if(!is.null(l_i$energycode)){
    s_main$summarised$.heatid <- l_i$energycode
  }

  # Assign typology to summarized data
  if(!is.null(l_i$typology)){
    s_main$summarised$.hubtyp <- l_i$typology
  }

  # Assign epoch to summarized data
  if(!is.null(l_i$epoch)){
    s_main$summarised$.hubage <- l_i$epoch
  }

  # Assign cuboid to summarized data
  if(!is.null(l_i$cuboid)){
    s_main$summarised$.cubdid <- l_i$cuboid
  }

  # Assign glazing coverage
  if(!is.null(l_i$glazing_coverage)){
    s_main$other.heat.loss.elements <- reassign_glazing(l_i$glazing_coverage)
  }

  # Assign wall type
  if(!is.null(l_i$wall_type)){
    s_main$geometry$D077_ExternalWallConstruction <-
      reassign_wall_type(l_i$wall_type)$D077_ExternalWallConstruction
  }

  # Return modified objects
  l_custom <- list(main=s_main, defs=l_defs, custom=l_i)

  return(l_custom)
}

evaluate_custom_archetype <- function(uid, dqu, sim=TRUE){
  #' @title Evaluates a custom archetype
  #'
  #' @description This function evaluates a custom archetype based on the provided UUID. It
  #' performs various steps including data subsetting, input assignment, IDF generation,
  #' EnergyPlus simulation (if specified), and result analysis.
  #'
  #' @param uid Character string representing the UUID of the archetype to evaluate.
  #' @param dqu Data frame containing archetype data.
  #' @param sim Logical indicating whether to perform the EnergyPlus simulation.
  #'   Defaults to TRUE.
  #' @return The ID of the evaluated model.
  
  # Helper function to retrieve index from aacode
  get_index_from_aacode <- function(input_code) {
    # @title Retrieve ID Code
    #
    # @description
    # Extracts the corresponding ID code from the provided data frame
    # based on the input code and a reduction flag.
    #
    # @param input_code The input code.
    # @return The extracted ID code as an integer.

    # Extract the full ID code
    id_code <- the$core_index %>%
      dplyr::filter(aacode %in% input_code) %>%
      dplyr::pull(index) %>%
      as.integer()

    return(id_code)
  }

  # Subset data for the specified archetype
  d_sel <- dqu %>% dplyr::filter(uuid == uid)

  # Create input list for EnHub matching
  l_inp <- list(
    uuid = d_sel$uuid,
    weather= d_sel$weather,
    aacode = d_sel$ehs_code,
    energycode = d_sel$ehs_heat,
    typology = d_sel$ehs_type,
    epoch = d_sel$epoch,
    cuboid = d_sel$cuboid,
    glazing_coverage = d_sel$glazing_coverage,
    wall_type = d_sel$wall_type,
    wall_u_value = d_sel$wall_u_value,
    hot_water = d_sel$hot_water,
    radiator_type = d_sel$radiator_type,
    thermostat_temperature = d_sel$thermostat_temperature
  )

  # Backup global objects and assign custom inputs
  the$GLOBAL_PAR_base <- the$GLOBAL_PAR
  s_ehs_2011_ext_base <- s_ehs_2011_ext
  l_custom <- assign_custom_inputs(l_inp, the$GLOBAL_PAR, s_ehs_2011_ext)
  the$GLOBAL_PAR <- l_custom$defs
  s_ehs_2011_ext <- l_custom$main

  # Obtain model ID based on aacode
  id_model <- get_index_from_aacode(l_inp$aacode)

  # Generate IDF
  idf_model <-
    generate_idf(id_model, fixed_schedule = FALSE, resolution = 'h')

  if(sim==TRUE){

    # Display IDF header
    idf_model$idf[c(59:79,117:177)]

    # Display assigned Energy Plus Weather (EPW) file
    idf_model$epw$epw; file.exists(idf_model$epw$epw); idf_model$epw$info

    # Perform Energy Plus simulation
    d_simulation <- run_energy_plus(id_model, idf_model$epw$epw)

    # Analyse simulation results
    check_simulation_status(id_model)
    l_results <- suppressMessages(extract_simulation_outputs(id_model))

  }

  # Rename inputs and outputs
  rename_model_with_uuid(id_model)

  # Restore global objects
  the$GLOBAL_PAR <- the$GLOBAL_PAR_base
  s_ehs_2011_ext <- s_ehs_2011_ext_base

  return(id_model)
}
