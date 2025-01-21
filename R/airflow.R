
estimate_infiltration_rates <- function(d_ref, d_age, l_dim, i_ven=TRUE){
  #' @title Estimates infiltration rates
  #'
  #' @description Calculates infiltration rates based on building age, room dimensions, and ventilation flag.
  #'
  #' @param d_ref Reference data.
  #' @param d_age Building age data.
  #' @param l_dim Building dimensions data.
  #' @param i_ven Logical flag indicating whether to include ventilation.
  #' @return A data frame containing estimated infiltration rates.

  # Convert ventilation flag to numeric
  i_ven <- ifelse(i_ven == TRUE, 1, 0)

  # Extract building age
  i_age <- d_age$.hubage %>% unlist() %>% as.character()

  # Prepare room data
  d_rom <- d_rooms %>% 
    dplyr::select(-id, -colour, -description, -ehs.function) %>%
    dplyr::rename(room_function = name)

  # Adjust room data based on building age
  if(i_age == 'post-2002'){
    d_rom <- d_rom %>% dplyr::select(-cat.A, -cat.B)
  }else if(i_age == 'modern'){
    d_rom <- d_rom %>% dplyr::select(-cat.A, -cat.C)
  }else{
    d_rom <- d_rom %>% dplyr::select(-cat.B, -cat.C)
  }

  # Rename columns in room data
  colnames(d_rom) <- c('code', 'room_function', 'htg_sp', 'ach')

  # Calculate exposed surface area and join data
  d_res <- l_dim$zones$thermal_zones %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(exposed_mm2 = volume__m3 * 1000 * 1/10 * 0.95) %>% dplyr::ungroup() %>%
    plyr::join(d_rom, by='room_function') %>%
    dplyr::mutate(vent = ach * 0.82 * i_ven) %>%
    dplyr::mutate(schedule='Infiltration {intermittent}',
           leakage='Leaking {erratic}',
           nat_vent='Natural Ventilation Schedule') %>%
    dplyr::select(-code, -storey) %>% tibble::tibble() %>%
    dplyr::distinct()

  return(d_res)
}

make_block_infiltration_rate <- function(d_rate, l_sup = l_idf_blocks) {
  #' @title Creates IDF blocks for infiltration rates
  #'
  #' @description Generates IDF blocks for infiltration rates based on provided data.
  #'
  #' @param d_rate Dataframe containing infiltration rates.
  #' @param l_sup List of supplementary information, typically IDF block templates.
  #' @return A character vector containing IDF lines for infiltration rates.

  # Helper function to create a single infiltration rate block
  make_block <- function(iz, d_ref, i_block, com) {

    lbl_block <- paste(iz, "Infiltration", sep = ":")
    lbl_method <- "AirChanges/Hour"

    d_sub <- d_ref %>% dplyr::filter(zone %in% iz)
    is <- d_sub %>% dplyr::select(schedule) %>% unlist() %>% as.character()
    ir <- d_sub %>% dplyr::select(ach) %>% unlist() %>% as.numeric()

    obj <- c(i_block,
      lbl_block, iz, is, lbl_method, 0.0, 0.0, 0.0,
      ir, 0.6061881811, 0.036300148, 0.1737, 0
    )

    res <- add_idf_comment(obj, c("", com))

    return(res)
  }

  # Create IDF block section header
  i_active <- "ZoneInfiltration:DesignFlowRate"
  l_section <- make_idf_block_title(i_active)
  l_com <- l_sup[[i_active]]

  # Create infiltration rate blocks for each zone
  l_obj <- lapply(d_rate$zone, make_block, d_rate, i_active, l_com)

  # Combine section header, blocks, and empty lines
  l_res <- c(l_section, "", unlist(l_obj), "")

  return(l_res)
}

make_block_infiltration_leak <- function(d_rate, l_sup = l_idf_blocks) {
  #' @title Creates IDF blocks for infiltration leak rates
  #'
  #' @description Generates IDF blocks for infiltration leak rates based on provided data.
  #'
  #' @param d_rate Dataframe containing ventilation rates.
  #' @param l_sup List of supplementary information, typically IDF block templates.
  #' @return A character vector containing IDF lines for ventilation rates.

  # Helper function to create a single ventilation rate block
  make_block <- function(iz, d_ref, i_block, com) {

    lbl_block <- paste(iz, "ExtraLeak", sep = ":")

    d_sub <- d_ref %>% dplyr::filter(zone %in% iz)
    is <- d_sub %>% dplyr::select(leakage) %>% unlist() %>% as.character()
    ir <- d_sub %>% dplyr::select(exposed_mm2) %>% unlist() %>% as.numeric()

    obj <- c(i_block,
             lbl_block, iz, is, ir/1000, 0.00015, 0.0001
    )

    res <- add_idf_comment(obj, c("", com))

    return(res)
  }

  # Create IDF block section header
  i_active <- "ZoneInfiltration:EffectiveLeakageArea"
  l_section <- make_idf_block_title(i_active)
  l_com <- l_sup[[i_active]]

  # Create ventilation rate blocks for each zone
  l_obj <- lapply(d_rate$zone, make_block, d_rate, i_active, l_com)

  # Combine section header, blocks, and empty lines
  l_res <- c(l_section, "", unlist(l_obj), "")

  return(l_res)
}

make_block_ventilation_rate <- function(d_rate, l_sup = l_idf_blocks){
  #' @title Creates IDF blocks for ventilation rates
  #'
  #' @description Generates IDF blocks for ventilation rates based on provided data.
  #'
  #' @param d_rate Dataframe containing ventilation rates.
  #' @param l_sup List of supplementary information, typically IDF block templates.
  #' @return A character vector containing IDF lines for ventilation rates.

  # Helper function to create a single ventilation rate block
  make_block <- function(iz, d_ref, i_block, com) {

    lbl_block <- paste(iz, "Ventilation", sep = ":")
    lbl_method <- "AirChanges/Hour"

    d_sub <- d_ref %>% dplyr::filter(zone %in% iz)
    is <- d_sub %>% dplyr::select(nat_vent) %>% unlist() %>% as.character()
    ir <- d_sub %>% dplyr::select(ach) %>% unlist() %>% as.numeric()

    obj <- c(i_block,
             lbl_block, iz, is, lbl_method, 0.0, 0.0, 0.0,
             ir, 'Natural', 0, 1, 1, 0, 0, 0, 24, '', 100,
             '', -50, '', -100, '', 100, '', 40
    )

    res <- add_idf_comment(obj, c("", com))

    return(res)
  }

  # Create IDF block section header
  i_active <- "ZoneVentilation:DesignFlowRate"
  l_section <- make_idf_block_title(i_active)
  l_com <- l_sup[[i_active]]

  # Create ventilation rate blocks for each zone
  l_obj <- lapply(d_rate$zone, make_block, d_rate, i_active, l_com)

  # Combine section header, blocks, and empty lines
  l_res <- c(l_section, "", unlist(l_obj), "")

  return(l_res)
}

make_block_ventilation_stack <- function(d_rate, l_sup = l_idf_blocks){
  #' @title Creates IDF blocks for wind and stack ventilation
  #'
  #' @description Generates IDF blocks for wind and stack ventilation 
  #' based on provided data.
  #' 
  #' `r lifecycle::badge("experimental")`
  #'
  #' @param d_rate Dataframe containing ventilation rates.
  #' @param l_sup List of supplementary information, typically IDF block templates.
  #' @return A character vector containing IDF lines for wind and stack ventilation.

  # Helper function to create a single ventilation stack block
  make_block <- function(iz, d_ref, i_block, com) {

    lbl_block <- paste(iz, "WindVentilation", sep = ":")

    d_sub <- d_ref %>% dplyr::filter(zone %in% iz)
    is <- d_sub %>% dplyr::select(leakage) %>% unlist() %>% as.character()
    ir <- d_sub %>% dplyr::select(exposed_mm2) %>% unlist() %>% as.numeric()

    obj <- c(i_block,
             lbl_block, iz, 0.2, 'Window opening {fraction}',
             'autocalculate', '', '', 0.5, 12.0, '', 24.0, '', 1.0
    )

    res <- add_idf_comment(obj, c("", com))

    return(res)
  }

  # Create IDF block section header
  i_active <- "ZoneVentilation:WindandStackOpenArea"
  l_section <- make_idf_block_title(i_active)
  l_com <- l_sup[[i_active]]

  # Create ventilation stack blocks for each zone
  l_obj <- lapply(d_rate$zone, make_block, d_rate, i_active, l_com)

  # Combine section header, blocks, and empty lines
  l_res <- c(l_section, "", unlist(l_obj), "")

  return(l_res)
}

make_block_infiltration <- function(d_ven, d_dwl, l_dms, i_vnt=TRUE){
  #' @title Creates infiltration and ventilation data for a block
  #'
  #' @description Calculates infiltration and ventilation rates, generates IDF-compatible data,
  #' and returns both the IDF data and summary information.
  #'
  #' @param d_ven Ventilation data.
  #' @param d_dwl Dwelling data.
  #' @param l_dms Design meteorological data.
  #' @param i_vnt Logical flag indicating whether to include ventilation.
  #'
  #' @return A list containing:
  #'   * `idf`: A character vector of IDF lines.
  #'   * `sum`: Summary data related to infiltration and ventilation rates.

  # Calculate infiltration rates
  d_rate <- estimate_infiltration_rates(d_ven, d_dwl, l_dms, i_vnt)

  # Create infiltration and ventilation data
  l_inf_rate <- make_block_infiltration_rate(d_rate)
  l_inf_leak <- make_block_infiltration_leak(d_rate)
  l_ven_rate <- make_block_ventilation_rate(d_rate)
  l_ven_stck <- make_block_ventilation_stack(d_rate)

  # Combine infiltration and ventilation data for IDF output
  idf_res <- c('', l_inf_rate, l_inf_leak, l_ven_rate)

  return(list(idf=idf_res, sum=d_rate))
}
