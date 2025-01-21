
populate_idf_section_A <- function(hid, d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext,
                                   l_sup=l_idf_blocks){
  #' @title Populates IDF section A with basic simulation parameters
  #'
  #' @description Creates IDF content for section A based on provided housing code and configuration data.
  #'
  #' @param hid A character string representing the housing code.
  #' @param d_ids A data frame containing housing code information (default: the$core_index).
  #' @param l_ehs A list containing EHS data (default: s_ehs_2011_ext).
  #' @param l_sup A list of IDF blocks, used as a base for the new constructions (default: l_idf_blocks).
  #'
  #' @return A list containing:
  #'   - sum: A summary of the generated parameters.
  #'   - idf: A character vector representing the IDF content for section A.

  # Extract parameters
  i_time_step <- the$GLOBAL_PAR$simulation_timestep
  i_version <- the$.enhub.config$eplus$version

  # Get housing data
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Get complementary data
  d_compl <- l_ehs$compleme %>% dplyr::filter(V001_HousingCode == id_ehs)

  # Process variables
  i_north <- assign_house_orientation(the$GLOBAL_PAR, d_compl)
  i_terrain <- assign_terrain(d_compl$V573_Urbanity, d_compl$V514_Sparsity)
  i_archetype <- paste0("Archetype (based on ", d_hid$aacode, ")")

  # Define IDF parameters
  l_par_outputs <- list(
    "Version" = list(
      value = i_version),
    "SimulationControl" = list(
      zone_size='Yes',
      sys_size='Yes',
      pln_size='No',
      run_size='Yes',
      run_epw='Yes',
      run_hvac='No',
      max_hvac=1),
    "Building" = list(
      idh=i_archetype,
      north=i_north,
      terrain=i_terrain,
      l_conv=0.04,
      t_conv=0.4,
      solar_dist='FullInteriorAndExteriorWithReflections',
      max_warmup=25,
      min_warmup=6),
    "SurfaceConvectionAlgorithm:Inside" = list(
      alg='TARP'),
    "SurfaceConvectionAlgorithm:Outside" = list(
      alg='DOE-2'),
    "HeatBalanceAlgorithm" = list(
      alg='ConductionTransferFunction'),
    "ZoneAirHeatBalanceAlgorithm" = list(
      alg='AnalyticalSolution',
      dospc = 'No'),
    "ZoneAirContaminantBalance" = list(
      alg='No'),
    "Timestep" = list(
      value=as.character(i_time_step)),
    "ConvergenceLimits" = list(
      min_sys=0,
      min_hvac=25),
    "GlobalGeometryRules" = list(
      vertex_ini='UpperLeftCorner',
      vertex_dir='Counterclockwise',
      coords_sys='Relative'))

  # Generate IDF content
  idf_section <- lapply(
    names(l_par_outputs), iterate_on_output_list, l_par_outputs, l_sup) %>%
    unlist()

  # Assemble IDF content
  l_res_head <- c('',make_idf_block_header('ep_A'), '', '')
  txt_idf <- c(l_res_head, idf_section)

  # Create summary data
  l_sum <- list(
    aacode = d_hid$aacode,
    orientation = i_north,
    terrain = i_terrain,
    time_step = i_time_step)

  # Display generation status
  idf_built_status("IDF project configuration")

  return(list(sum = l_sum, idf = txt_idf))
}

populate_idf_section_B <- function(hid, l_weather,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext,
                                   l_sup=l_idf_blocks){
  #' @title Populates IDF section B with weather and simulation parameters
  #'
  #' @description Creates IDF content for section B based on provided housing code,
  #' weather data, and simulation parameters.
  #'
  #' @param hid A character string representing the housing code.
  #' @param l_weather A list containing weather data.
  #' @param d_ids A data frame containing housing code
  #' information (default: the$core_index).
  #' @param l_ehs A list containing EHS data (default: s_ehs_2011_ext).
  #' @param l_sup A list of IDF blocks, used as a base for the
  #' new constructions (default: l_idf_blocks).
  #'
  #' @return A list containing:
  #'   - sum: A summary of the generated parameters, including weather
  #'          data, design days, and simulation period.
  #'   - idf: A character vector representing the IDF content for section B.

  # Extract parameters
  i_lapse <- the$GLOBAL_PAR$simulation_period
  i_year <- the$GLOBAL_PAR$simulation_year

  # Get housing data
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Validate simulation period
  validate_sim_period(i_year, i_lapse)

  # Extract weather data
  d_epw <- l_weather$info
  i_lapse <- list(month_i=i_lapse[1], day_i=i_lapse[2],
                  month_e=i_lapse[3], day_e=i_lapse[4])

  # Calculate ground temperature
  d_gnd <- estimate_ground_temperature(0, l_weather) %>%
    append_ground_temperatures()

  # Define IDF parameters
  i_place <- paste(d_epw$station, d_epw$place, sep=" --- ")

  l_par_outputs <- list(
    "Site:Location" = list(
      lbl=i_place,
      lat=d_epw$lat,
      lon=d_epw$lon,
      tzn=d_epw$tzn,
      ele=d_epw$elv),
    "RunPeriod" = list(
      lbl='SimPeriod',
      ini_m=i_lapse$month_i,
      ini_d=i_lapse$day_i,
      ini_y=i_year,
      end_m=i_lapse$month_e,
      end_d=i_lapse$day_e,
      end_y=i_year,
      'Saturday','No','No','No','Yes','Yes'),
    "Site:GroundTemperature:BuildingSurface" = list(
      values=round(d_gnd$T_ground, 4)),
    "Site:GroundTemperature:FCfactorMethod" = list(
      values=round(d_gnd$FCfactor, 4)),
    "Site:GroundTemperature:Shallow" = list(
      values=round(d_gnd$Shallow, 4)),
    "Site:GroundTemperature:Deep" = list(
      values=round(d_gnd$Deep, 4)),
    "Site:GroundReflectance" = list(
      values=round(d_gnd$Reflectance, 4)),
    "Site:GroundReflectance:SnowModifier" = list(
      gnd=1,day=1))

  # Generate IDF content
  idf_section <-
    lapply(names(l_par_outputs),
           iterate_on_output_list, l_par_outputs, l_sup) %>% unlist()

  # Generate design days IDF content
  idf_ddys <- obtain_ddy(l_weather$ddy, l_weather$rain)
  d_epw_ddy <- idf_ddys$sum; idf_ddys <- idf_ddys$idf

  # Assemble IDF content
  l_res_head <- c('',make_idf_block_header('ep_B'), '', '')
  txt_idf <- c(l_res_head, idf_section, idf_ddys)

  # Create summary data
  l_sum <- list(
    epw = l_weather,
    ddy = d_epw_ddy %>% dplyr::filter(group == 'heating') %>% dplyr::select(-group),
    sim_lapse = i_lapse,
    sim_year = i_year)

  # Display generation status
  idf_built_status("Weather and simulation parameters")

  return(list(sum = l_sum, idf = txt_idf))
}

populate_idf_section_C <- function(hid, l_dim,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext,
                                   d_tus=s_tus,
                                   d_rom=d_rooms){
  #' @title Populates IDF section C with usage schedules and profiles
  #'
  #' @description Creates IDF content for section C based on provided housing code, building dimensions,
  #' and various other data sources.
  #'
  #' @param hid A character string representing the housing code.
  #' @param l_dim A list containing building dimension data.
  #' @param d_ids A data frame containing housing code information (default: the$core_index).
  #' @param l_ehs A list containing EHS data (default: s_ehs_2011_ext).
  #' @param d_tus A list containing TUS data (default: s_tus).
  #' @param d_rom A data frame containing room data (default: d_rooms).
  #'
  #' @return A list containing:
  #'   - sum: A summary of the generated parameters, including schedules, occupants, and zone appliance profiles.
  #'   - idf: A character vector representing the IDF content for section C.


  # Extract configuration parameters
  k_mode <- the$GLOBAL_PAR$fixed_schedule
  k_int <- the$GLOBAL_PAR$enery_use_intensity

  # Define resolution and schedule method
  k_resolution <- '60min'        # eg. '10min', '60min', '30min'
  k_subject <- '_household'      # eg. '_household', '_variable'

  # Load schedule templates from JSON files
  l_sched_base <- l_schedules_base
  l_sched_airflow <- l_schedules_airflow
  l_sched_people <- l_schedules_people
  l_sched_limits <- l_schedules_limit

  # Determine schedule type based on evaluation mode
  k_mode <- ifelse(k_mode==TRUE, 'sto', 'det')

  # Set random number generator state based on evaluation mode
  randomise_profiles(k_mode)

  # Get housing data
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Get complementary, summary, dwelling, individual, and diary data
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_summa <- l_ehs$summa %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_dwell <- l_ehs$dwell %>% dplyr::filter(V001_HousingCode == id_ehs)
  d_indiv <- d_tus$tbl_individual
  d_diary <- d_tus$tbl_diary

  # Get archetype parameters
  d_info <- get_effective_zones(id_ehs, l_ehs)$info
  i_area <- d_info$vTFA

  # Estimate and assign occupants based on evaluation mode
  d_people <- calculate_no_occupants(id_ehs, k_mode)
  d_occupants <- d_people$simple
  n_people <- sum(d_occupants, na.rm = T)

  # Estimate typical energy demand (BREDEM and HEFF)
  d_demand <-
    obtain_appliances_demand(hid, i_area, n_people, l_ehs, d_ids, k_int)

  # Define thermal zones based on building dimensions
  l_thermal_zones <- l_dim$zones$thermal_zone_groups

  # Generate usage schedules based on archetype, and export to model-directory
  l_schedules <- generate_usage_schedules(hid, k_int, k_mode, d_demand)

  # Obtain presence of occupants in thermal zones
  d_presence <- obtain_occupants_presence(
    d_compl, d_summa, d_dwell, d_indiv, d_diary,
    d_occupants, k_resolution, k_subject)

  # Distribute values
  d_zones_apps <- assign_appliances_profile(l_schedules, l_thermal_zones)
  d_zones_dist <- assign_occupants_profile(d_presence, l_thermal_zones)

  # Enrich single schedule file
  d_schedules <- l_schedules$schedules %>%
    plyr::join(d_zones_dist, by = 'Hour') %>% tibble::tibble()

  # Generate schedule limits blocks
  idf_section <- generate_schedule_limits(l_sched_limits)

  # Generate compact schedules
  l_compacts <- c(l_sched_base, l_sched_people, l_sched_airflow)
  idf_scheds <- generate_schedule_compact(l_compacts)

  # Compose idf_schedule
  idf_schedule <- generate_schedule_idf(hid, d_schedules, d_zones_apps)

  # Assemble IDF blocks
  txt_idf <- c(idf_section, idf_scheds, idf_schedule)


  # Create summary list
  l_sum <- list(
    schedule_labels = colnames(d_schedules),
    occupants = d_people,
    zones_appliances = d_zones_apps)

  # Display message for generation status
  k_mode <- ifelse(k_mode=='sto','stochastic-like','deterministic (fixed)')
  idf_built_status("Occupancy and basic schedules")

  return(list(sum = l_sum, idf = txt_idf))
}

populate_idf_section_D <- function(hid,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext){
  #' @title Populates IDF section D with building envelope information
  #'
  #' @description Creates IDF content for section D based on provided housing code and various EHS data sources.
  #'
  #' @param hid A character string representing the housing code.
  #' @param d_ids A data frame containing housing code information (default: the$core_index).
  #' @param l_ehs A list containing EHS data (default: s_ehs_2011_ext).
  #'
  #' @return A list containing:
  #'   - sum: A summary of heat loss parameters per building component.
  #'   - idf: A list of character vectors representing the IDF content for each component.


  # Extract configuration parameter
  underfloor_heat <- the$GLOBAL_PAR$underfloor_heat

  # Get housing data
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Get relevant EHS data
  d_geome <- l_ehs$geome %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_other <- l_ehs$other %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_dwell <- l_ehs$dwell %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_summa <- l_ehs$summa %>% dplyr::filter(V001_HousingCode==id_ehs)

  # Get archetype parameters
  d_info <- obtain_archetype_parameters(id_ehs, l_ehs)
  i_attic <- as.character(d_info$vRff)
  i_loftins <- d_other$D080_LoftInsulationThickness
  i_underheat <- assume_heated_underfloor(d_summa$.heatid)

  # Estimate heat loss parameters
  l_hlb <- estimate_heat_loss_parameter(hid, the$GLOBAL_PAR)


  # Define IDF sections

  #.... [VO] void boundary
  idf.VO <- assign_void_boundary()

  #.... [ES] exterior surface/wall
  idf.ES <- assign_external_surface(d_geome, l_hlb$opaques)
  hlp.ES <- idf.ES$summary; idf.ES <- idf.ES$idf

  #.... [IS] interior surface/wall
  idf.IS <- assign_internal_surface(d_other, l_hlb$internal)
  hlp.IS <- idf.IS$summary; idf.IS <- idf.IS$idf

  #.... [EW] + [IW] exterior & interior window
  idf.EW_IW <- assign_window_type(d_other, d_compl, l_hlb$opening)
  hlp.EW_IW <- idf.EW_IW$summary; idf.EW_IW <- idf.EW_IW$idf

  #.... [ED] + [ID] exterior & interior door
  idf.ED_ID <- assign_door_type(d_other, d_compl, l_hlb$openings)
  hlp.ED_ID <- idf.ED_ID$summary; idf.ED_ID <- idf.ED_ID$idf

  #.... [GF] ground floor
  idf.GF <- assign_ground_floor(l_hlb$floor)
  hlp.GF <- idf.GF$summary; idf.GF <- idf.GF$idf

  #.... [ER] exterior roof (new material added)
  idf.ER <- assign_external_roof(d_other, d_dwell, i_attic, l_hlb$opaques)
  hlp.ER <- idf.ER$summary; idf.ER <- idf.ER$idf

  #.... [ER] if loft present, check/adjust for insulation
  if(i_loftins>0 & i_attic=="N"){
    idf.ER <- assign_top_block_insulation(idf.ER, hlp.ER, i_loftins)
    hlp.ER <- idf.ER$summary; idf.ER <- idf.ER$idf
  }

  #.... [IR] interior roof / ceiling
  idf.IR <- assign_internal_roof(d_other, l_hlb$internal)
  hlp.IR <- idf.IR$summary; idf.IR <- idf.IR$idf

  #.... [IF] interior floor (reciprocal of [IR])
  idf.IF <- assign_internal_floor(idf.IR)
  hlp.IF <- hlp.IR

  #.... [LT] loft surface  (reciprocal of [LF)
  if(i_loftins>0 & i_attic=="W"){
    idf.LT <- assign_loft_material(idf.IR, hlp.IR, i_loftins)
    hlp.IR <- idf.LT$summary; idf.LT <- idf.LT$idf
    idf.LF <- assign_internal_floor(idf.LT, "Loft Floor")
  }else{
    idf.LT <- idf.LF <- ""
  }


  # Define remaining sections based on logic
  if(i_underheat==TRUE | (underfloor_heat==TRUE & i_underheat==FALSE)){
    idf.IF <- assign_underfloor_heat(idf.IF)
    idf.GF <- assign_underfloor_heat(idf.GF)
    idf.LF <- assign_underfloor_heat(idf.LF)
  }

  # Assemble HLP module
  l_hlp <- list(ES=hlp.ES, IS=hlp.IS, ER=hlp.ER, IR=hlp.IR,
                GF=hlp.GF, IF=hlp.IF, WN=hlp.EW_IW, DR=hlp.ED_ID)

  # Assemble idf module
  idf_sections <- c(idf.VO, idf.ES, idf.IS, idf.ER, idf.IR, idf.LT,
                    idf.LF, idf.GF, idf.IF, idf.EW_IW, idf.ED_ID)


  # Display message for generation status
  idf_built_status("Fabric and heat loss parameters")

  return(list(sum=l_hlp, idf=idf_sections))
}

populate_idf_section_E <- function(hid, d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext,
                                   l_rom=s_ehs_2011_rom,
                                   l_sup=l_idf_blocks){
  #' @title Populates the IDF section E with data based on provided parameters.
  #'
  #' @description This function processes housing data, calculates dwelling geometry, and defines
  #' thermal zone groups to populate the IDF section E.
  #'
  #' @param hid Character string representing the housing code.
  #' @param d_ids Dataframe containing housing code information.
  #' @param l_ehs List of EHS data.
  #' @param l_rom Unused argument.
  #' @param l_sup List of IDF blocks.
  #' @return A list containing the populated IDF section E.

  # Obtain id parameters
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Extract parameters
  i_layout_range <- the$GLOBAL_PAR$layout_range

  # Obtain pivotal datasets
  d_dwell <- l_ehs$dwell %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)

  # Obtain archetype parameters
  d_info <- obtain_archetype_parameters(id_ehs, l_ehs)
  dtaDim <- obtain_archetype_dimensions(id_ehs, l_ehs)

  # Obtain core geometrical parameters
  k_typ <- d_dwell$D002_DwellingType %>% as.character()
  k_tfa <- dtaDim['varTFA'] %>% as.numeric()
  k_sto <- d_info['vFlr'] %>% as.integer()
  k_att <- ifelse(as.character(d_info['vRff'])=='W', TRUE, FALSE)
  k_bsm <- ifelse(as.character(d_info['vBsm'])=='W', TRUE, FALSE)

  # Obtain likely number of rooms
  l_pred <-
    predict_room_info(l_room_lm, k_typ, k_tfa, i_nstoreys = k_sto) %>%
    jsonlite::fromJSON() %>%
    tibble::tibble()

  # Obtain suitable layout set
  l_lays <- assign_house_layout(
    k_sto = k_sto,
    k_rom = l_pred$number_of_rooms,
    k_bed = l_pred$bedroom.floor_level,
    k_att = k_att,
    k_bsm = k_bsm,
    k_dim = i_layout_range)

  # Define dwelling geometry input card, to overwrite global-defaults
  l_dwell_info <- list(width=dtaDim$CuW,
                       depth=dtaDim$CuD,
                       floor_height=dtaDim$GfH,
                       window_to_wall=dtaDim$GzR,
                       cuboid = dtaDim$varCub,
                       flat_roof = !k_att,
                       access = 'front',
                       origin=c(0,0,0),
                       layouts = l_lays)

  # Generate idf and summary
  l_epD <- generate_dwelling_geometry(l_dwell_info)

  # Define thermal zone groups
  l_zones <- l_epD$card$zones$thermal_zones
  l_thermal_zones <- obtain_list_zone_summary(d_info, d_compl, l_zones)
  l_epD$card$zones$thermal_zone_groups <- l_thermal_zones


  # Display message for generation status
  idf_built_status("Geometry and thermal zones")

  return(l_epD)
}

populate_idf_section_F <- function(hid, l_weather, l_sched, l_dims,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext){
  #' @title Populates the IDF section F with data based on provided parameters.
  #'
  #' @description This function processes housing data, calculates building characteristics,
  #' and generates IDF sections for internal loads, infiltration, mixing, and
  #' a summary of key parameters.
  #'
  #' @param hid Character string representing the housing code.
  #' @param l_weather List containing weather data.
  #' @param l_sched List containing schedule data.
  #' @param l_dims List containing building dimensions data.
  #' @param d_ids Dataframe containing housing code information (default: the$core_index).
  #' @param l_ehs List containing EHS data (default: s_ehs_2011_ext).
  #' @return A list containing a summary and IDF sections.

  # Obtain id parameters
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Obtain pivotal datasets
  d_geome <- l_ehs$geome %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_other <- l_ehs$other %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_dwell <- l_ehs$dwell %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_venti <- l_ehs$venti %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_hotwa <- l_ehs$hot %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_sheat <- l_ehs$space %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_lowlt <- l_ehs$low %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_summa <- l_ehs$summa %>% dplyr::filter(V001_HousingCode==id_ehs)

  # Obtain objects from module:C
  path_hid <- file.path(the$.enhub.paths$eval$A, hid)
  path_sch <- file.path(path_hid, "default_schedules.csv.gz")
  d_sched <- l_sched$schedule_labels

  # Obtain archetype parameters
  d_apps <- l_sched$zones_appliances
  d_info <- obtain_archetype_parameters(id_ehs, l_ehs)
    i_floors <- d_info$vFlr
    i_attic <- as.character(d_info$vRff)
    i_base <- as.character(d_info$vBsm)
    i_attach <- as.character(d_info$vAtt)
    i_flat <- as.character(d_info$vFlt)
    i_area <- l_dims$dimensions$varTFA
  dtaRen <- estimate_solar_parameters(id_ehs, l_weather)
  i_lelight <-
    mean(l_ehs$low.energy.lighting$D112_LowEnergyLightsFraction, na.rm=T)
  i_auxHeat <- as.integer(d_sheat$D095_SecondaryHeatingSystem)
  i_auxHeat <- s_chm$CHM_SecondaryHeatingSystem$name[i_auxHeat]
  i_ventilation <- the$GLOBAL_PAR$ventilation_on


  # Parse variables

  #.... allocate infiltration & ventilation
  if(the$GLOBAL_PAR$infiltration_mode %in% c("simple")){

    idf_air <- make_block_infiltration(d_venti, d_summa, l_dims, i_ventilation)
    d_air <- idf_air$sum; idf_air <- idf_air$idf

  }else{

    idf_air <- ''
    d_air <- NULL

  }

  #.... allocate inter-zone mixing
  if(the$GLOBAL_PAR$infiltration_mixing==TRUE){
    # idf_mix <- assign_zone_mixing(i_zones.eff, idf_mix, i_attach)
    idf_mix <- ""
  }else{
    idf_mix <- ""
  }

  #.... distribute/allocate appliances' specifications
  if(the$GLOBAL_PAR$home_appliances == FALSE) d_apps$usage__kwh___yr <- NA
  idf_hh_app <- assign_loads(d_apps, d_sched)


  # Assemble idf module
  idf_sections <- c(idf_hh_app, idf_air, idf_mix, "")

  # Assemble summary
  l_sum <- list(solar_panel=dtaRen,
                low_e_light=i_lelight,
                secondary_heating = i_auxHeat,
                zone_air=d_air)


  # Display message for generation status
  idf_built_status("Internal loads and infiltration")

  return(list(sum=l_sum, idf=idf_sections))
}

populate_idf_section_G <- function(hid, l_dim, l_scd, l_lds,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext){
  #' @title Populates the IDF section G with energy system and water service data.
  #'
  #' @description This function processes housing data, calculates energy system components,
  #' generates water service data, and creates IDF sections for energy systems,
  #' schedules, and curves.
  #'
  #' @param hid Character string representing the housing code.
  #' @param l_dim List containing building dimensions data.
  #' @param l_scd List containing schedule data.
  #' @param l_lds List containing load data.
  #' @param d_ids Dataframe containing housing code information (default: the$core_index).
  #' @param l_ehs List containing EHS data (default: s_ehs_2011_ext).
  #'
  #' @return A list containing a summary and IDF sections.

  # Read global parameters
  heating_mode <- the$GLOBAL_PAR$heating_availability

  # Obtain id parameters
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Obtain pivotal datasets
  d_summa <- l_ehs$summa %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_hotwa <- l_ehs$hot %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_znair <- l_lds$zone_air

  # Assign custom/survey heating system
  if(is.null(the$GLOBAL_PAR$heating_system_code)){
    k_heating_code <- d_summa$.heatid
  }else{
    k_heating_code <- the$GLOBAL_PAR$heating_system_code
  }

  # Obtain archetype parameters
  l_zones_function <- l_dim$zones$thermal_zone_groups

  # Estimate values
  k_occupants <- l_scd$occupants$full$occupancy_eq

  # Load insulation data
  k_insulation <- list(type = d_hotwa$D106_DHWCylinderInsulationType,
                       thickness = d_hotwa$D107_DHWCylinderInsulationThickness)

  # Infer loop information
  l_in_loops <- obtain_heating_components(k_heating_code, k_insulation)

  # Generate energy system collection
  l_sys <- generate_energy_system(k_heating_code, l_in_loops, d_znair)

  # Generate water service collection
  l_dhw <- generate_water_service(l_zones_function, l_sys$info$branches, k_occupants)

  # Generate schedules
  l_zone_sp <- obtain_zonal_setpoint_schedules(l_sys$info$zones)
  l_sys_sched <- define_selected_system_schedule(k_heating_code, l_in_loops, l_sys$info$loops, l_sys$info$schedules)
  l_epH_sched <- generate_schedule_compact(c(l_sched_compact, l_sys_sched, l_zone_sp))

  # Generate curves
  l_epH_curves <- generate_performance_curves(l_sys$info$systems$curves)

  # Summarise data
  l_summary <- l_sys$info
  l_summary$water_services <- l_dhw$info

  # Combine idf sections
  l_idf <- c(l_sys$idf, l_epH_sched, unlist(l_epH_curves), l_dhw$idf)

  # Display message for generation status
  idf_built_status("Energy system and water services")

  return(list(sum=l_summary, idf=l_idf))
}

populate_idf_section_I <- function(hid, l_weather, l_context, l_dim,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext){
  #' @title Populates the IDF section I with low-zero carbon (LZC) technology data.
  #'
  #' @description This function processes housing data, determines LZC technologies, and generates
  #' IDF sections for solar, water heating, wind, and biomass systems.
  #'
  #' @param hid Character string representing the housing code.
  #' @param l_weather List containing weather data.
  #' @param l_context List containing contextual data.
  #' @param l_dim List containing building dimensions.
  #' @param d_ids Dataframe containing housing code information (default: the$core_index).
  #' @param l_ehs List containing EHS data (default: s_ehs_2011_ext).
  #' @return A list containing a summary and IDF sections.

  # Extract parameters
  lzc_battery <- the$GLOBAL_PAR$battery

  # Obtain id parameters
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Obtain pivotal datasets
  d_geome <- l_ehs$geome %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_venti <- l_ehs$venti %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_hotwa <- l_ehs$hot %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_sheat <- l_ehs$space %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_dwell <- l_ehs$dwell %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_other <- l_ehs$other %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_lowlt <- l_ehs$low %>% dplyr::filter(V001_HousingCode==id_ehs)

  # Obtain archetype parameters
  d_info <- obtain_archetype_parameters(id_ehs, l_ehs)
  i_attic <- as.character(d_info$vRff)
  i_north <- l_context$orientation

  # Reconcile LZC custom parameters
  if(is.null(the$GLOBAL_PAR$low_zero_carbon_code)){

    lzc_code <- '0000'
    lzc_code <- encode_renewable(lzc_code)

    # Read parameters from survey
    i_shw_panel <- d_hotwa$D109_SolarDHWSystem
    i_pv_panel <- d_compl$V510_SolarPhotovoltaics
    i_pv_panel <- ifelse(i_pv_panel != '----', TRUE, FALSE)

    #.. retrieve renewable-related outputs and update
    lzc_code['y.water'] <- i_shw_panel
    lzc_code['z.solarpv'] <- i_pv_panel

  }else{

    lzc_code <- the$GLOBAL_PAR$low_zero_carbon_code
    lzc_code <- encode_renewable(lzc_code)

  }

  # Generate LZC objects
  idf_solar <- generate_pv_solar_objects(id_ehs,l_dim,lzc_code,l_weather,lzc_battery)
  idf_water_solar <- generate_water_solar_objects(lzc_code)
  idf_turbine <- generate_wind_turbine_objects(id_ehs,lzc_code,l_weather,lzc_battery)
  idf_biomass <- generate_biomass_store_objects(lzc_code)

  # Assemble idf module
  idf_section <- c(idf_solar, idf_water_solar, idf_turbine, idf_biomass)

  # Assemble summary
  l_sum <- list(lzc_technology=lzc_code, battery=lzc_battery)

  # Display message for generation status
  idf_built_status("Low-zero carbon technology")

  return(list(sum=l_sum, idf=idf_section))
}

populate_idf_section_J <- function(hid, l_sched, l_dim, l_heat, l_lzc,
                                   d_ids=the$core_index,
                                   l_ehs=s_ehs_2011_ext,
                                   l_sup=l_idf_blocks){
  #' @title Populates the IDF section J with output variables.
  #'
  #' @description This function processes housing data, determines output variables, and generates
  #' IDF sections for different output types.
  #'
  #' @param hid Character string representing the housing code.
  #' @param l_sched List containing schedule data.
  #' @param l_dim List containing building dimensions.
  #' @param l_heat List containing heating system data.
  #' @param l_lzc List containing LZC technology data.
  #' @param d_ids Dataframe containing housing code information (default: the$core_index).
  #' @param l_ehs List containing EHS data (default: s_ehs_2011_ext).
  #' @param l_sup List of IDF blocks (unused).
  #'
  #' @return A character vector representing the IDF section J.

  # Parse input parameters
  lzc_code <- l_lzc$lzc_technology
  sim_resolution <- the$GLOBAL_PAR$simulation_resolution

  # Obtain id parameters
  d_hid <- d_ids %>% dplyr::filter(index == hid)
  id_ehs <- get_aacode(d_hid$index)

  # Obtain pivotal datasets
  d_compl <- l_ehs$compl %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_hotwa <- l_ehs$hot %>% dplyr::filter(V001_HousingCode==id_ehs)
  d_summa <- l_ehs$summ %>% dplyr::filter(V001_HousingCode==id_ehs)

  # Load list of requested outputs
  l_out_lbl <- l_idf_outputs

  # Obtain archetype parameters
  i_zones.eff <- l_dim$zones$thermal_zones$zone
  d_info <- l_sched$dimensions
  i_HeatCode <- d_summa$.heatid
  i_res <- expand_output_resolution(sim_resolution)

  # Filter renewable-related outputs (only PV is currently implemented)
  l_out_lzc <-
    obtain_idf_output_lzc(lzc_code, l_dim$zones$thermal_zones, l_out_lbl)


  # Generate IDF blocks

  #.. block -- output:variables
  idf_vars <- make_block_output_variables(l_out_lbl, l_out_lzc, i_HeatCode, i_res)

  #.. block -- output:variables:zonal
  idf_zonal <- make_block_output_zonal(l_out_lbl, i_zones.eff, i_res)

  #.. block -- output:metres
  idf_metered <- make_block_output_metres(l_out_lbl, l_heat, l_out_lzc, i_res)

  #.. block -- Output:General
  idf_section <- make_block_output_general()


  # Assemble IDF blocks
  idf_head <- c('', make_idf_block_header('ep_J'), '', '')
  idf_section <- c(idf_head, '',
                   idf_section, '',
                   idf_vars, '',
                   idf_zonal, '',
                   idf_metered, '',
                   make_idf_block_header("End of file ", "Section")) %>%
    gsub("\t","", .)

  # Display message for generation status
  idf_built_status("IDF requested output variables")

  return(idf_section)
}
