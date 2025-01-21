
# energy system module ---------------------------------------------------------

append_heating_connections <- function(df, l_loop){
  #' @title Append heating connections to a data frame
  #'
  #' @description This function takes a data frame (`df`) and appends columns
  #' indicating the zone behind and ahead of each zone in the data frame.
  #'
  #' @param df: The data frame containing zone information.
  #' @param l_loop: List of energy system parameters
  #'
  #' @return A data frame with additional columns:
  #'         `level`, `behind`, `ahead`, `clg_sp`, `htg_schedules`,
  #'         `clg_schedules`, `htg_available`, `thermostat`, `thermostat_ctrl`
  #'

  # Define labels for zone levels
  lbl_sto <- c('BB', 'GF', paste0(seq(1, 99), 'F'), 'AT', 'TF')

  # Create a temporary data frame and manipulate zone information
  d_res <- df %>%
    dplyr::mutate(level = gsub('^\\((.*)\\-(\\d+\\-\\d+)\\)', '\\1', zone)) %>%
    dplyr::mutate(level = factor(level, levels = lbl_sto, ordered = TRUE)) %>%
    dplyr::arrange(level, zone) %>%
    dplyr::filter(!grepl('TF', level)) %>%
    droplevels() %>%
    dplyr::mutate(behind = dplyr::lag(zone, default = 'ini'),
      ahead = dplyr::lead(zone, default = 'end')) %>%
    dplyr::mutate(clg_sp = 27.0102) %>%
    dplyr::mutate(htg_schedules = paste0("schedule:heat_up_to_", htg_sp),
           clg_schedules = paste0("schedule:cool_over_", round(clg_sp,0)),
           thermostat = paste0("thermostat:zone:", zone),
           thermostat_ctrl = paste0("thermostat:dual-sp:", zone),
           htg_available = paste0("schedule:heat-availability:",
                                  l_loop$heating_availability))

  # Return the modified data frame
  return(d_res)
}

generate_heating_branch_list <- function(k, d_zon, l_sys){
  #' @title Generate heating branch list
  #'
  #' @description This function takes a system type (`k`), zone data
  #' frame (`d_zon`), and loop parameters (`l_sys`), and generates a
  #' data frame listing the heating branches for that system.
  #'
  #' @param k: Character representing the system type (e.g., 'wls', 'wld').
  #' @param d_zon: Data frame containing zone information
  #' @param l_sys: List of loop parameters
  #'
  #' @return A data frame with columns:
  #'         `group` (system type)
  #'         `branch` (heating branch name)
  #'         `type` (heating branch function)
  #'         `loc` (heating branch location)
  #'         `link_in` (connection in pipes)
  #'         `link_out` (connection out pipes)
  #'         `has_pipe` (presence of adiabatic pipe)
  #'         `has_pump` (presence of pump device)
  #' @examples
  #' \dontrun{
  #' k = define_heating_loops(l_loop$heating_codes$msh, l_in_loops)[4]
  #' d_zon = d_zones
  #' l_sys = l_in_loops
  #' generate_heating_branch_list(k, d_zon, l_sys)
  #' }

  # Helper to better enumerate connections
  assign_loop_id <- function(ig){
    #' @title Assign loop ID based on input group
    #' @description This function assigns a loop ID based on the provided input group.
    #' @param ig The input group.
    #' @return The assigned loop ID.

    # Check for water loop types
    if(grepl('^wld$|^wls$|^wlts$', ig)){
      res <- 1
    }else if(grepl('^fld$|^fls$|^xld$|^xls$', ig)){
      res <- 3
    }else{
      # Default loop type
      res <- 2
    }
    return(res)
  }

  # Define base branches for all system types
  k_baseb <- c('inlet', 'outlet', 'bypass')

  # Define core branch based on system type
  if(k == 'wls'){
    k_coreb <- 'main-heater'
  }else if(k == 'wld'){
    k_coreb <- paste0('in--', d_zon$zone)
    if(l_sys$loop == 'zone-loop-double') k_coreb <- c(k_coreb, 'dhw-heater')
  }else if(k == 'wlss'){
    k_coreb <- 'dhw-heater'
  }else if(k == 'wlsd'){
    k_coreb <- c('services-cold','services-warm')
  }else if(k == 'wlts'){
    k_coreb <- c('main-heater','hp-heater')
  }else if(k == 'wxts'){
    k_coreb <- c('heat-exchanger','hp-heater')
  }else if(k == 'fls'){
    k_coreb <- 'heat-exchanger'
  }else if(k == 'fld'){
    k_coreb <- 'hp-heater'
  }else if(k == 'xls'){
    k_coreb <- 'hp-heater'
  }else if(k == 'xld'){
    k_coreb <- c('heat-exchanger','hp-cylinder')
  }else{
    stop("this loop is not supported")
  }

  # Combine base and core branches
  all_branches <- c(k_baseb, k_coreb)

  # Create result data frame with group and branch information
  d_res <-
    data.frame(group=k, branch=paste0(k,'-branch-',all_branches)) %>%
    tibble::tibble() %>%
    dplyr::mutate(type = ifelse(grepl('\\-bypass$', branch), 'bypass',
                         ifelse(grepl('\\-inlet$|\\-outlet$', branch), 'distribution',
                         ifelse(grepl('\\-heat\\-exchanger$', branch), 'exchanger',
                         ifelse(grepl('\\-heater$|\\-cylinder', branch), 'heating',
                         ifelse(grepl('\\-services-', branch), 'services', 'emitters'
                         )))))) %>%
    dplyr::mutate(loc = ifelse(grepl('\\-inlet$', branch), 'in',
                        ifelse(grepl('\\-outlet$', branch), 'out', 'mid'))) %>%
    dplyr::arrange(loc) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(idl = assign_loop_id(group),
           ids = ifelse(grepl('s$', group), 1, 2),
           idi = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(idi = 2 * cumsum(idi)) %>%
    dplyr::mutate(ido = idi - 1) %>%
    dplyr::mutate(idi = sprintf("%03d", idi),
           ido = sprintf("%03d", ido)) %>%
    dplyr::mutate(link_in = paste0('n-', idl, ids, ido),
           link_out = paste0('n-', idl, ids, idi)) %>%
    dplyr::select(-idl, -ids, -ido, -idi) %>%
    dplyr::mutate(has_adpipe = ifelse(grepl('^bypa|^dist', type), TRUE, FALSE)) %>%
    dplyr::mutate(has_pump = ifelse(grepl('s$', group) & grepl('^in$', loc), TRUE, FALSE)) %>%
    dplyr::mutate(has_adpipe = ifelse(has_adpipe==TRUE & has_pump==TRUE, FALSE, has_adpipe))

  # Correct nomenclature when adding extra component
  if(k == 'wlts'){
    d_res <- d_res %>%
      dplyr::mutate(group = gsub('^wlts', 'wls', group),
             branch = gsub('^wlts', 'wls', branch))
  }

  # Correct nomenclature when adding extra component
  if(k == 'wxts'){
    d_res <- d_res %>%
      dplyr::mutate(link_in = gsub('^n\\-\\d', 'n-1', link_in),
             link_out = gsub('^n\\-\\d', 'n-1', link_out),
             group = gsub('^wxts', 'wls', group),
             branch = gsub('^wxts', 'wls', branch))
  }

  # Return the result data frame
  return(d_res)
}

define_heating_loops <- function(k_msh, l_loop){
  #' @title Define heating loops based on main system type
  #'
  #' @description This function takes a main system type code (`k_msh`) and
  #' determines the possible water loop types associated with that system.
  #'
  #' @param k_msh: Integer representing the master system type code.
  #'
  #' @return A character vector containing the possible water loop types.
  #' @examples
  #' define_heating_loops(1, 3)
  #' define_heating_loops(15, 8)

  k_main_loops <- c('wls','wld')
  k_dhw_loops <- c('wlss','wlsd')
  k_gshp_loops <- c('fls','fld', 'wlts', 'wld', k_dhw_loops)
  k_ashp_loops <- c('xls','xld', 'wxts', 'wld', k_dhw_loops)

  # Obtain default ASHP technology
  if(is.null(the$GLOBAL_PAR$heating_ashp)){
    k_ashp <- "integrated"
  }else{
    k_ashp <- the$GLOBAL_PAR$heating_ashp
  }

  # Obtain loop based on main-space-heating type
  if(k_msh %in% c(1:6,11:13,15)){

    # Define default water loop types
    l_res <- k_main_loops

    if(l_loop$loop == 'zone-loop-double'){

      # Allow 'wlss' and 'wlsd' loops for specific master system types
      l_res <- c(l_res, k_dhw_loops)

    }

  }else if(k_msh %in% c(14)){

    # Define additional water loop types
    l_res <- k_gshp_loops

  }else{

    # Define secondary water loop types only
    l_res <- k_dhw_loops

  }

  # Define exception for ASHP system
  if((k_msh == 15) & (k_ashp != "integrated")) l_res <- k_ashp_loops

  # Return the possible water loop types
  return(l_res)
}

define_heating_components <- function(k_msh, k_dhw){
  #' @title Define heating fuels and heater types based on system codes
  #'
  #' @description This function takes master system type (`k_msh`) and
  #' domestic hot water type (`k_dhw`) codes and determines the associated
  #' heating fuel and heater type.
  #'
  #' @param k_msh: Integer representing the master system type code.
  #' @param k_dhw:  Integer representing the domestic hot water code.
  #'
  #' @return A list with four elements:
  #'   * `main_fuel`: Character vector indicating the main space-heating fuel
  #'   * `main_heater`: Character vector indicating the main space-heating heater
  #'   * `main_device`: Character vector indicating the main space-heating device
  #'   * `main_emitter`: Character vector indicating the main space-heating emitter
  #'   * `main_efficiency`: Numeric vector indicating the main space-heating efficiency
  #'   * `main_cylinder`: Numeric vector indicating the main cylinder volume
  #'   * `main_regime`: Character vector indicating the main space-heating
  #'   * `main_emitters`: Character vector indicating the main space-heating availability
  #'   * `dhw_fuel`: Character vector indicating the domestic hot-water fuel
  #'   * `dhw_heater`: Character vector indicating the domestic hot-water heater
  #'   * `dhw_device`: Character vector indicating the domestic hot-water device
  #'   * `radiators_temperature`: Character vector indicating the radiators design temperature
  #'   * `curves`: Character vector indicating the curves associated to the energy system
  #'
  #' @examples
  #' \dontrun{
  #' k_msh=l_loop$heating_codes$msh
  #' k_dhw=l_loop$heating_codes$dhw
  #' define_heating_components(k_msh, k_dhw)
  #' }

  # Process dhw exception
  if(as.integer(k_dhw) == 0 | as.integer(k_dhw)>11) k_dhw <- 10

  # Ensure a character is passed
  k_msh <- k_msh %>% as.character()
  k_dhw <- k_dhw %>% as.character()

  # Define fuel types based on main space-heating code
  fuel_msh <- switch(k_msh,
                     "0" = "DistrictHeatingWater",
                     "1" = "NaturalGas",
                     "2" = "NaturalGas",
                     "3" = "NaturalGas",
                     "4" = "FuelOilNo1",
                     "5" = "Coal",
                     "6" = "Electricity",
                     "7" = "Electricity",
                     "8" = "Electricity",
                     "9" = "NaturalGas",
                     "10" = "NaturalGas",
                     "11" = "DistrictHeatingWater",
                     "12" = "DistrictHeatingWater",
                     "13" = "OtherFuel1",
                     "14" = "Electricity",
                     "15" = "Electricity"
  )

  # Define heater types based on main space-heating code
  htr_msh <- switch(k_msh,
                    "0" = "DistrictHeating:Water",
                    "1" = "Boiler:HotWater",
                    "2" = "Boiler:HotWater",
                    "3" = "WaterHeater:Mixed",
                    "4" = "Boiler:HotWater",
                    "5" = "Boiler:HotWater",
                    "6" = "Boiler:HotWater",
                    "7" = "",
                    "8" = "",
                    "9" = "Furnace",
                    "10" = "Furnace",
                    "11" = "DistrictHeating:Water",
                    "12" = "DistrictHeating:Water",
                    "13" = "Boiler:HotWater",
                    "14" = "HeatPump:WaterToWater:EquationFit:Heating",
                    "15" = "WaterHeater:HeatPump:PumpedCondenser"
  )

  # Define heater efficiency based on main space-heating code
  if(is.null(the$GLOBAL_PAR$heating_system_efficiency)){
    effh_msh <- switch(k_msh,
                       "0" = NA,
                       "1" = 0.845,
                       "2" = 0.875,
                       "3" = 0.731,
                       "4" = 0.845,
                       "5" = 0.845,
                       "6" = 0.845,
                       "7" = NA,
                       "8" = NA,
                       "9" = 0.837,
                       "10" = 0.837,
                       "11" = NA,
                       "12" = NA,
                       "13" = 0.845,
                       "14" = 3.2,
                       "15" = 3.2)
  }else{
    effh_msh <- the$GLOBAL_PAR$heating_system_efficiency
  }

  #... parse hp COP parameter
  has_hp_cop <- !is.null(the$GLOBAL_PAR$heat_pump_cop)
  is_relevant_msh <- k_msh %in% c(14, 15)
  if(has_hp_cop & is_relevant_msh) effh_msh <- the$GLOBAL_PAR$heat_pump_cop


  # Define (default) cylinder volume based on main space-heating code
  cylv_msh <- switch(k_msh,
                     "0" = NA,
                     "1" = 180,
                     "2" = 1, # In EP, a zero volume is assumed for combi
                     "3" = 180,
                     "4" = 180,
                     "5" = 180,
                     "6" = 180,
                     "7" = NA,
                     "8" = NA,
                     "9" = NA,
                     "10" = NA,
                     "11" = 1, # In EP, a zero volume is assumed for DH
                     "12" = 1, # In EP, a zero volume is assumed for DH
                     "13" = 180,
                     "14" = 120,
                     "15" = 120
  )

  # Define heater devices based on main space-heating code
  fun_msh <- switch(k_msh,
                    "0" = "hw-district-heating",
                    "1" = "hw-standard-boiler",
                    "2" = "hw-combi-boiler",
                    "3" = "hw-back-boiler",
                    "4" = "hw-standard-boiler",
                    "5" = "hw-standard-boiler",
                    "6" = "hw-standard-boiler",
                    "7" = "",
                    "8" = "",
                    "9" = "warm-air-furnace",
                    "10" = "warm-air-furnace",
                    "11" = "hw-district-heating",
                    "12" = "hw-district-heating",
                    "13" = "hw-standard-boiler",
                    "14" = "hw-heat-pump",
                    "15" = "hw-heat-pump"
  )

  # Define heater types based on main space-heating code
  k_ashp_set <- c("ASHP HighT CAPFT", "ASHP HighT COPFT", "HPWHPLFFPLR",
                  "DefaultFanPowerRatioCurve", "DefaultFanEffRatioCurve")
  k_gshp_set <- c("WWHPHeatCapCurve", "WWHPHeatPowCurve")
  is_hp_cylinder <- the$GLOBAL_PAR$heating_ashp == 'cylinder'
  if(is_hp_cylinder) k_ashp_set <- c('Boiler Efficiency',k_ashp_set)

  crv_msh <- switch(k_msh,
                    "0" = "",
                    "1" = "Boiler Efficiency",
                    "2" = "Boiler Efficiency",
                    "3" = "",
                    "4" = "Boiler Efficiency",
                    "5" = "Boiler Efficiency",
                    "6" = "Boiler Efficiency",
                    "7" = "",
                    "8" = "",
                    "9" = "",
                    "10" = "",
                    "11" = "",
                    "12" = "",
                    "13" = "Boiler Efficiency",
                    "14" = k_gshp_set,
                    "15" = k_ashp_set
  )

  # Define fuel types based on domestic hot-water (DHW) code
  fuel_dhw <- switch(k_dhw,
                     "1" = "NaturalGas",
                     "2" = "NaturalGas",
                     "3" = "NaturalGas",
                     "4" = "NaturalGas",
                     "5" = "FuelOilNo1",
                     "6" = "Coal",
                     "7" = "OtherFuel1",
                     "8" = "Electricity",
                     "9" = "Electricity",
                     "10" = "DistrictHeatingWater",
                     "11" = "DistrictHeatingWater"
  )

  # Define heater types based on domestic hot-water (DHW) code
  htr_dhw <- switch(k_dhw,
                    "1" = "WaterHeater:Mixed",
                    "2" = "WaterHeater:Mixed",
                    "3" = "WaterHeater:Mixed",
                    "4" = "WaterHeater:Mixed",
                    "5" = "WaterHeater:Mixed",
                    "6" = "WaterHeater:Mixed",
                    "7" = "WaterHeater:Mixed",
                    "8" = "WaterHeater:Mixed",
                    "9" = "WaterHeater:Mixed",
                    "10" = "WaterHeater:Mixed",
                    "11" = "WaterHeater:Mixed"
  )

  # Define heater devices based on domestic hot-water (DHW) code
  fun_dhw <- switch(k_dhw,
                    "1" = "dhw-water-heater",
                    "2" = "dhw-zero-storage",
                    "3" = "dhw-zero-storage",
                    "4" = "dhw-back-boiler",
                    "5" = "dhw-water-heater",
                    "6" = "dhw-water-heater",
                    "7" = "dhw-water-heater",
                    "8" = "dhw-water-heater",
                    "9" = "dhw-water-heater",
                    "10" = "dhw-zero-storage",
                    "11" = "dhw-zero-storage",
  )

  # Define heater devices based on main space-heating code
  emi_msh <- switch(k_msh,
                    "0" = "ZoneHVAC:IdealLoadsAirSystem",
                    "1" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "2" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "3" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "4" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "5" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "6" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "7" = "ZoneHVAC:Baseboard:RadiantConvective:Electric",
                    "8" = "ZoneHVAC:Baseboard:Convective:Electric",
                    "9" = "ZoneHVAC:AirDistributionUnit",
                    "10" = "ZoneHVAC:AirDistributionUnit",
                    "11" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "12" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "13" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "14" = "ZoneHVAC:Baseboard:RadiantConvective:Water",
                    "15" = "ZoneHVAC:Baseboard:RadiantConvective:Water"
  )

  #... parse underfloor heating
  is_underfloor <- the$GLOBAL_PAR$underfloor_heat
  is_relevant_msh <- k_msh %in% c(1:8,11:15)
  if(is_underfloor & is_relevant_msh) emi_msh <-
    'ZoneHVAC:LowTemperatureRadiant:VariableFlow'

  # Define heater devices based on main space-heating code
  is_low_temp_hp <- the$GLOBAL_PAR$heating_hp_variation == 'low-temp'
  is_relevant_msh <- k_msh %in% c(14, 15)
  k_emi_tmp <- ifelse(is_low_temp_hp & is_relevant_msh, 52.7025, 75.7025)

  emi_tmp <- switch(k_msh,
                    "0" = NA,
                    "1" = k_emi_tmp,
                    "2" = k_emi_tmp,
                    "3" = k_emi_tmp,
                    "4" = k_emi_tmp,
                    "5" = k_emi_tmp,
                    "6" = k_emi_tmp,
                    "7" = NA,
                    "8" = NA,
                    "9" = NA,
                    "10" = NA,
                    "11" = k_emi_tmp,
                    "12" = k_emi_tmp,
                    "13" = k_emi_tmp,
                    "14" = k_emi_tmp,
                    "15" = k_emi_tmp
  )

  # Parse regime parameters (from global)
  hsp_msh <- ifelse(is.null(the$GLOBAL_PAR$heating_setpoints),
                    "constant", the$GLOBAL_PAR$heating_setpoints)

  # Parse emitters parameters (from global)
  ava_msh <- ifelse(is.null(the$GLOBAL_PAR$heating_availability),
                    "constant", the$GLOBAL_PAR$heating_availability)


  # Create list of properties
  l_res <- list(
    main_device = fun_msh,
    main_fuel = fuel_msh,
    main_heater = htr_msh,
    main_emitter = emi_msh,
    main_efficiency = effh_msh,
    main_cylinder = cylv_msh,
    main_regime = ava_msh,
    main_emitters = hsp_msh,
    dhw_device = fun_dhw,
    dhw_fuel = fuel_dhw,
    dhw_heater = htr_dhw,
    radiators_temperature = emi_tmp,
    curves = crv_msh
  )

  # Return list
  return(l_res)
}

define_loop_parameters <- function(k_loop){
  #' @title Define loop parameters based on loop type
  #'
  #' @description This function takes a loop type code (`k_loop`) and
  #' defines a list of parameters for that loop.
  #'
  #' @param k_loop: Character string representing the loop type code.
  #'
  #' @return A list containing loop parameters for the specified type.
  #'   The list can contain additional loop types if defined by `k_loop`.
  #'
  #' @examples
  #' \dontrun{
  #' define_loop_parameters("zone-loop-double")
  #' }

  # Define default loop parameters (domestic hot water loop)
  l_res <- list(
    dhw_loop=list(
      name='hot-water-loop',
      type='heating',
      fluid='Water',
      distribution='Optimal',
      temperature_exit=67.15,
      temperature_max=80.00,
      temperature_min=0.00
    )
  )

  # Add main loop parameters for specific loop type
  if(k_loop %in% c("zone-loop-double","zone-loop-triple")){

    l_res$main_loop <- list(
      name='main-water-loop',
      type='heating',
      fluid='Water',
      distribution='Optimal',
      temperature_exit=71.151,
      temperature_max=90.00,
      temperature_min=0.00
    )

    if(k_loop %in% "zone-loop-triple"){

      l_res$triple_loop <- list(
        name='hp-water-loop',
        type='heating',
        fluid='Water',
        distribution='Optimal',
        temperature_exit=71.151,
        temperature_max=90.00,
        temperature_min=0.00
      )

    }

  }

  # Return the list of loop parameters
  return(l_res)
}

obtain_heating_components <- function(id, ins, l_ref=l_energy_systems){
  #' @title Obtain heating components based on energy system ID
  #'
  #' @description This function extracts heating components based on the
  #' provided energy system ID.
  #'
  #' @param id: The energy system ID
  #' @param ins: Additional insulation parameters
  #' @param l_ref: A reference list containing energy system
  #' data (default: l_energy_systems).
  #'
  #' @return A list containing loop type, emitter type, space heating,
  #' and hot water information.
  #'
  #' @examples
  #' \dontrun{
  #' id=k_heating_code
  #' ins=k_insulation
  #' l_ref=l_energy_systems
  #' obtain_heating_components(id, ins, l_ref)
  #' }

  # Decode energy system code
  l_heat_code <- decode_energy_system(id)
  k_msh <- l_heat_code$msh
  k_dhw <- l_heat_code$dhw

  # Determine loop type based on main space heating (msh) code
  k_loop <- ifelse(k_msh %in% c(1:6,11:15), "double", "single")

  # Process ashp exception
  if(k_msh==15 & the$GLOBAL_PAR$heating_ashp!="integrated") k_loop <- "triple"

  # Determine emitter technology based on msh code
  k_tech <- ifelse(k_msh==9 | k_msh==10, "zone-warm-air",
            ifelse(k_msh==7, "zone-radiator-electric",
            ifelse(k_msh==8, "zone-radiator-room",
            ifelse(k_msh==14, "zone-gshp",
            ifelse(k_msh==15, "zone-ashp", "zone-radiator-water")))))

  # Process exception for ideal-loads otpion
  k_space <- ifelse(k_msh == 0, 'ideal-loads', l_ref$space_heating[k_msh])

  # Create result list
  l_res <- list(loop = paste0("zone-loop-", k_loop),
                emitter = k_tech,
                energy_system_code = id,
                space_heating = k_space,
                hot_water = l_ref$hot_water[k_dhw],
                insulation = ins)

  # Return list
  return(l_res)
}

define_emitters <- function(di, dz, ln){
  #' @title Define emitters based on input data
  #'
  #' @description This function defines emitter information based
  #' on provided data.
  #'
  #' @param di A data frame containing input data.
  #' @param dz A data frame containing zone data.
  #' @param ln A list of data frames.
  #'
  #' @return A data frame containing emitter information.

  # Define regular expression pattern for electric emitter types
  rgx_pattern <- '\\:Electric$|AirDistributionUnit$|IdealLoadsAirSystem$'

  # Check if main emitter matches pattern
  if(grepl(rgx_pattern, di$main_emitter)){

    # Create emitter data from zone data
    d_branches <- dz %>%
      dplyr::select(zone, htg_available, thermostat) %>%
      dplyr::mutate(device_type = di$main_emitter,
             device_name = paste0(zone, '--emitter'))

  }else{

    # Create emitter data from list of data frames
    d_branches <- plyr::ldply(ln, data.frame) %>%
      tibble::tibble() %>%
      dplyr::filter(type == 'emitters') %>%
      dplyr::select(-has_adpipe, -has_pump)

  }

  # Return data frame
  return(d_branches)
}

define_heat_pump_components <- function(ln, di){
  #' @title Create data frame of HP components with corresponding
  #' inlets and outlets
  #'
  #' @description This function generates a data frame indicating connections
  #' to heat pump, using an iterator (`ln`) defining main device properties,
  #' and a data frame (di) with system information.
  #'
  #' @param ln: List containing main device properties
  #' @param di: Data frame containing system information
  #'
  #' @return A data frame indicating connections to heat pump

  # Filter heating emitters
  d_branches <- plyr::ldply(ln, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(grepl('^heating',type)) %>%
    dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^Water', device_type) & !grepl('^dhw',.id)) %>%
    dplyr::mutate(fuel = di$dhw_fuel,
           cylinder = ifelse(is.na(di$main_cylinder),
                             180, di$main_cylinder),
           efficiency = ifelse(is.na(di$main_efficiency),
                               0.88, di$main_efficiency))

  # Split supply-demand, to pick inlet/outlets
  d_heater_m <- d_branches %>% dplyr::filter(grepl('.*s$',group))
  d_heater_c <- d_branches %>% dplyr::filter(grepl('.*d$',group)) %>%
    dplyr::mutate(link_in = gsub('^n\\-1','n-5',link_in),
           link_out = gsub('^n\\-1','n-5',link_out),
           device_type = 'WaterHeater:Mixed',
           device_name = 'hw-heat-pump--vessel')
  d_heater_r <- d_heater_c %>%
    dplyr::mutate(link_in = gsub('^n\\-5','n-6',link_in),
           link_out = gsub('^n\\-5','n-6',link_out),
           device_type = 'Coil:WaterHeating:AirToWaterHeatPump:Pumped',
           device_name = 'hw-heat-pump--coil')
  d_heater_d <- d_heater_c %>%
    dplyr::mutate(link_in = gsub('^n\\-5','n-4',link_in),
           link_out = gsub('^n\\-5','n-4',link_out))

  # Append source inlet/outlets
  d_res <- d_branches %>% head(1) %>%
    dplyr::mutate(condenser_in = d_heater_d$link_in,
           condenser_out = d_heater_d$link_out,
           source_in = d_heater_c$link_in,
           source_out = d_heater_c$link_out,
           source_type = d_heater_c$device_type,
           source_name = d_heater_c$device_name,
           coil_in = d_heater_r$link_in,
           coil_out = d_heater_r$link_out,
           coil_type = d_heater_r$device_type,
           coil_name = d_heater_r$device_name)

  return(d_res)
}

define_heat_pump_components_w_auxiliary <- function(ln, di){
  #' @title Create data frame of HP components with corresponding
  #' inlets and outlets
  #'
  #' @description This function generates a data frame indicating connections
  #' to heat pump, using an iterator (`ln`) defining main device properties,
  #' and a data frame (di) with system information.
  #'
  #' @param ln: List containing main device properties
  #' @param di: Data frame containing system information
  #'
  #' @return A data frame indicating connections to heat pump

  # Filter heating emitters
  d_branches <- plyr::ldply(ln, data.frame) %>%
    tibble::tibble() %>%
    dplyr::filter(grepl('^heating|^dx',type)) %>%
    dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^Water|^HeatEx', device_type)) %>%
    dplyr::mutate(fuel = di$dhw_fuel,
           cylinder = ifelse(is.na(di$main_cylinder),
                             180, di$main_cylinder),
           efficiency = ifelse(is.na(di$main_efficiency),
                               0.88, di$main_efficiency))


  # Split supply-demand, to pick inlet/outlets

  d_heater_source <- d_branches %>%
    dplyr::filter(grepl('.*d$',group) & grepl('^Water', device_type))

  d_heater_use <- d_branches %>%
    dplyr::filter(grepl('.*ss$',group))

  d_heater_hp_vessel <- d_branches %>%
    dplyr::filter(grepl('.*ls$',group)) %>%
    dplyr::mutate(type = gsub('(.*)(\\-\\-\\-\\-.*)','\\1',type),
           device_type = gsub('(.*)(\\-\\-\\-\\-.*)','\\1',device_type),
           device_name = gsub('(.*)(\\-\\-\\-\\-.*)','\\1',device_name)) %>%
    dplyr::mutate(link_in = paste0(link_out,'--',link_in))

  d_heater_hp_hx <- d_branches %>%
    dplyr::filter(grepl('.*ls$',group)) %>%
    dplyr::mutate(type = gsub('(.*)\\-\\-\\-\\-(.*)','\\2',type),
           device_type = gsub('(.*)\\-\\-\\-\\-(.*)','\\2',device_type),
           device_name = gsub('(.*)\\-\\-\\-\\-(.*)','\\2',device_name)) %>%
    dplyr::mutate(link_out = paste0(link_out,'--',link_in))

  d_heater_coil <- d_branches %>%
    dplyr::filter(grepl('.*d$',group) & !grepl('^dx', type)) %>%
    dplyr::mutate(link_in = gsub('^n\\-\\d','n-6',link_in),
           link_out = gsub('^n\\-\\d','n-6',link_out),
           device_type = 'Coil:WaterHeating:AirToWaterHeatPump:Pumped',
           device_name = 'hw-heat-pump--coil')

  d_heater_hx <- d_branches %>%
    dplyr::filter(grepl('.*d$',group) & grepl('^dx', type))


  # Append source inlet/outlets
  d_res <- d_branches %>% head(1) %>% dplyr::select(fuel, cylinder, efficiency) %>%

    dplyr::mutate(coil_in = d_heater_coil$link_in,
           coil_out = d_heater_coil$link_out,
           coil_type = d_heater_coil$device_type,
           coil_name = d_heater_coil$device_name,

           hx_in = d_heater_hp_hx$link_in,
           hx_out = d_heater_hp_hx$link_out,
           hx_type = d_heater_hp_hx$device_type,
           hx_name = d_heater_hp_hx$device_name,

           hx_aux_in = d_heater_hx$link_in,
           hx_aux_out = d_heater_hx$link_out,
           hx_aux_type = d_heater_hx$device_type,
           hx_aux_name = d_heater_hx$device_name,

           tank_in = d_heater_hp_vessel$link_in,
           tank_out = d_heater_hp_vessel$link_out,
           tank_type = d_heater_hp_vessel$device_type,
           tank_name = d_heater_hp_vessel$device_name,

           heater_source_in = d_heater_source$link_in,
           heater_source_out = d_heater_source$link_out,
           heater_source_type = d_heater_source$device_type,
           heater_source_name = d_heater_source$device_name,

           heater_use_in = d_heater_use$link_in,
           heater_use_out = d_heater_use$link_out,
           heater_use_type = d_heater_use$device_type,
           heater_use_name = d_heater_use$device_name)

  return(d_res)
}

define_ground_heat_pump_heater_components <- function(ln, di){
  #' @title Create data frame of HP components with corresponding
  #' inlets and outlets
  #'
  #' @description This function generates a data frame indicating connections
  #' to heat pump, using an iterator (`ln`) defining main device properties,
  #' and a data frame (di) with system information.
  #'
  #' @param ln: List containing main device properties
  #' @param di: Data frame containing system information
  #'
  #' @return A data frame indicating connections to heat pump

  # Filter heating emitters
  d_branches <- plyr::ldply(ln, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'heating') %>%
    dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^HeatPump', device_type) & grepl('^main',.id)) %>%
    dplyr::mutate(efficiency = ifelse(is.na(di$main_efficiency), 3.2, di$main_efficiency))

  # Split supply-demand, to pick and inlet/outlets
  d_heater_s <- d_branches %>% dplyr::filter(grepl('.*s$',group))
  d_heater_d <- d_branches %>% dplyr::filter(grepl('.*d$',group))

  # Append source inlet/outlets
  d_heater_d <- d_heater_d %>%
    dplyr::mutate(
      source_in = d_heater_s$link_in,
      source_out = d_heater_s$link_out)

  return(d_heater_d)
}

assign_devices <- function(i, d_lp, d_it){
  #' @title Assign device information based on loop type
  #'
  #' @description This function extracts information about nodes-connections
  #' for a specific loop type (`i`) from a loop data frame (`d_lp`).
  #'
  #' @param i: Character string representing the loop type
  #' @param d_lp: Data frame containing loop information (groups, types, ...)
  #' @param d_it: Data frame containing system information
  #'
  #' @return A list containing information about plant inlet/outlet,
  #' demand inlet/outlet, branches, and connectors for the loop type.

  # Define node information based on loop type
  if(i=='dhw_loop'){
    k_group <- c('wlss','wlsd')
    k_item <- d_it[grepl('dhw',names(d_it))]
    k_item$dhw_emitter <- 'WaterUse:Connections'
    k_altr <- d_it[grepl('main',names(d_it))]
  }else if(i=='triple_loop'){
    k_group <- c('xls', 'xld')
    k_item <- d_it[grepl('main',names(d_it))]
    k_altr <- d_it[grepl('main',names(d_it))]
  }else{
    k_group <- c('wls', 'wld', 'fls', 'fld')
    k_item <- d_it[grepl('main',names(d_it))]
    k_altr <- d_it[grepl('dhw',names(d_it))]
  }
  names(k_item) <- gsub('^[a-z]+\\_','',names(k_item))
  names(k_altr) <- gsub('^[a-z]+\\_','',names(k_altr))

  # Filter loop data for relevant groups
  d_loop_sub <- d_lp %>% dplyr::ungroup() %>% dplyr::filter(group %in% k_group)

  # Split loop data for relevant types
  d_loop_sub_pump <- d_loop_sub %>% dplyr::filter(has_pump == TRUE)
  d_loop_sub_pipe <- d_loop_sub %>% dplyr::filter(has_adpipe == TRUE)
  d_loop_sub_more <- d_loop_sub %>% dplyr::filter(has_adpipe == FALSE & has_pump == FALSE)
  d_loop_sub_heat <- d_loop_sub_more %>% dplyr::filter(type == 'heating')
  d_loop_sub_emit <- d_loop_sub_more %>% dplyr::filter(type != 'heating')

  # Append device information
  d_loop_sub_pump <- d_loop_sub_pump %>%
    dplyr::mutate(device_type = 'Pump:VariableSpeed',
           device_name = paste0(branch,'--pump'))

  d_loop_sub_pipe <- d_loop_sub_pipe %>%
    dplyr::mutate(device_type = 'Pipe:Adiabatic',
           device_name = paste0(link_in,'--',link_out))

  d_loop_sub_heat <- d_loop_sub_heat %>%
    dplyr::mutate(device_type = k_item$heater,
           device_name = k_item$device) %>%
    dplyr::mutate(device_type = ifelse(group %in% c('wld'), k_altr$heater, device_type),
           device_name = ifelse(group %in% c('wld'), k_altr$device, device_name))

  d_loop_sub_emit <- d_loop_sub_emit %>%
    dplyr::mutate(device_type = k_item$emitter,
           device_name = paste0(gsub('(^.*)\\-\\-(.*$)','\\2',branch),'--emitter')) %>%
    dplyr::mutate(device_type = ifelse(group %in% c('wls') & grepl('\\-heat\\-exchanger$',branch), 'HeatExchanger:FluidToFluid', device_type)) %>%
    dplyr::mutate(type = ifelse(group %in% c('wls') & grepl('\\-heat\\-exchanger$',branch), 'dx', type)) %>%
    dplyr::mutate(device_name = ifelse(group %in% c('wls') & grepl('\\-heat\\-exchanger$',branch), 'heat-exchanger--dx', device_name))


  # Additional adjustments for GSHPs
  if('fls' %in% k_group){

    d_loop_sub_heat <- d_loop_sub_heat %>%
      dplyr::mutate(device_type = ifelse(
        group %in% c('wls') & grepl('\\-hp\\-heater$',branch),
        'WaterHeater:Mixed', device_type)) %>%
      dplyr::mutate(device_name = ifelse(
        grepl('^HeatPump\\:Water', device_type),
        paste0(device_name,'--water-to-water'), device_name))

    k_gnd_sys <- ifelse(the$GLOBAL_PAR$heating_gshp=='borehole',
                        'GroundHeatExchanger:System', 'GroundHeatExchanger:Slinky')

    d_loop_sub_emit <- d_loop_sub_emit %>%
      dplyr::mutate(device_type = ifelse(
        group %in% c('fls') & grepl('\\-heat\\-exchanger$',branch),
        k_gnd_sys, device_type)) %>%
      dplyr::mutate(type = ifelse(
        group %in% c('fls') & grepl('\\-heat\\-exchanger$',branch),
        'dx', type)) %>%
      dplyr::mutate(device_name = ifelse(
        group %in% c('fls') & grepl('\\-heat\\-exchanger$',branch),
        'heat-exchanger--dx', device_name))
  }

  # Additional adjustments for GSHPs
  if(any(grepl('^x',k_group))){

    d_loop_sub_heat <- d_loop_sub_heat %>%
      dplyr::mutate(device_type = ifelse(group %in% c('xls') & grepl('\\-hp\\-heater$',branch), 'Boiler:HotWater', device_type)) %>%
      dplyr::mutate(device_type = ifelse(group %in% c('xld') & grepl('\\-hp\\-cylinder$',branch), 'WaterHeater:Mixed', device_type)) %>%
      dplyr::mutate(device_name = ifelse(group %in% c('xls') & grepl('\\-hp\\-heater$',branch), 'hw-heat-pump--heater', device_name)) %>%
      dplyr::mutate(device_name = ifelse(group %in% c('xld') & grepl('\\-hp\\-cylinder$',branch), 'dhw-water-heater', device_name)) %>%
      dplyr::mutate(device_name = ifelse(grepl('^HeatPump\\:Water', device_type), paste0(device_name,'--water-to-water'), device_name)) %>%
      dplyr::mutate(link_in = ifelse(group %in% c('xld') & grepl('\\-hp\\-cylinder$',branch), 'n-41001', link_in)) %>%
      dplyr::mutate(link_out = ifelse(group %in% c('xld') & grepl('\\-hp\\-cylinder$',branch), 'n-41002', link_out))

    d_loop_sub_emit <- d_loop_sub_emit %>%
      dplyr::mutate(device_type = ifelse(group %in% c('xld') & grepl('\\-heat\\-exchanger$',branch), 'HeatExchanger:FluidToFluid', device_type)) %>%
      dplyr::mutate(type = ifelse(group %in% c('xld') & grepl('\\-heat\\-exchanger$',branch), 'dx', type)) %>%
      dplyr::mutate(device_name = ifelse(group %in% c('xld') & grepl('\\-heat\\-exchanger$',branch), 'heat-exchanger--dx', device_name))
  }

  # Merge split data-frames
  d_loop_merge <- d_loop_sub_pump %>%
    dplyr::bind_rows(d_loop_sub_heat) %>%
    dplyr::bind_rows(d_loop_sub_emit) %>%
    dplyr::bind_rows(d_loop_sub_pipe)

  # Merge branch (applicable to heat exchangers)
  if(all(c('wls-branch-heat-exchanger','wls-branch-hp-heater') %in% d_loop_merge$branch)){
    d_left <- d_loop_merge %>%
      dplyr::filter(branch == 'wls-branch-hp-heater') %>%
      dplyr::mutate(device_type = 'WaterHeater:HeatPump:PumpedCondenser')
    d_right <- d_loop_merge %>%
      dplyr::filter(branch == 'wls-branch-heat-exchanger')
    d_base <- d_loop_merge %>%
      dplyr::filter(branch != 'wls-branch-heat-exchanger' & branch != 'wls-branch-hp-heater')

    d_merge <- d_left %>%
      dplyr::mutate(link_out = d_right$link_out,
             type = paste(type, d_right$type, sep='----'),
             device_name = paste(device_name, d_right$device_name, sep='----'),
             device_type = paste(device_type, d_right$device_type, sep='----'))

    d_loop_merge <- d_base %>%
      dplyr::bind_rows(d_merge) %>%
      dplyr::arrange(group,branch)
  }

  # Return the extracted node information
  return(d_loop_merge)
}

assign_cylinder_insulation_transfer <- function(insulation_data){
  #' Assigns thermal conductance based on insulation data.
  #'
  #' This function takes a data frame containing insulation information
  #' and calculates the thermal conductance based on type and thickness.
  #'
  #' @param insulation_data A data frame with columns 'type'
  #' (insulation type) and 'thickness' (insulation in millimeters).
  #'
  #' @return Thermal conductance in W/mK.
  #'

  # Extract type and thickness information
  insulation_type <- insulation_data$type
  insulation_thickness <- insulation_data$thickness

  # Handle missing type information
  insulation_type <- ifelse(
    is.na(insulation_type), "-", insulation_type)

  # Convert thickness to meters (assuming millimeters originally)
  insulation_thickness_m <- ifelse(
    insulation_type == "-", 0, insulation_thickness) / 100

  # Assign thermal conductivity based on type
  insulation_material <- ifelse(
    insulation_type == "Foam", 1.48016, ifelse(
      insulation_type == "Jacket", 3.45338, ifelse(
        insulation_type == "Other", 6.23786, NA)
    )
  )

  # Calculate thermal conductance
  thermal_conductance <- 5.511 - insulation_thickness_m / insulation_material

  return(thermal_conductance)
}

summarise_water_services <- function(nocc, l_zones){
  #' Summarizes water services for a given number of occupants and zone data.
  #'
  #' This function calculates water service parameters, assigns services
  #' to zones, and creates a summary data frame.
  #'
  #' @param nocc Number of occupants.
  #' @param l_zones A list containing zone information.
  #'
  #' @return A data frame with water service details (service, zone, volume,
  #' schedule, and hot water schedule).

  # Assign system parameters (m3 per day, based on SAP assumptions).
  flow_rate <- 24*60*60                     # seconds
  volume_total <- 125 * nocc * 0.001        # in m3
  volume_dhw <- (36 + 25 * nocc) * 0.001    # in m3
  ratio_dhw <- volume_dhw / volume_total    # fraction

  # Define water service names.
  l_services <- c(
    'KitchenSink', 'KitchenWasher', 'KitchenOther',
    'BathroomShower', 'BathroomBath', 'BathroomSink', 'BathroomService')

  # Assign services to zones.

  if('cooking' %in% names(l_zones)){
    k_cook <- sample(l_zones$cooking$zone, 3, replace = TRUE)
  }else{
    k_cook <- sample(l_zones$all_zones$zone, 3, replace = TRUE)
  }

  if('hygiene' %in% names(l_zones)){
    k_wash <- sample(l_zones$hygiene$zone, 4, replace = TRUE)
  }else{
    k_wash <- sample(l_zones$all_zones$zone, 4, replace = TRUE)
  }

  lbl_zones <- c(k_cook, k_wash)

  # Assign volumetric flow rates.
  l_vol <- c(
    round(volume_total * 13/100 / flow_rate, 15),
    round(volume_total * 15/100 / flow_rate, 15),
    round(volume_total * 5/100 / flow_rate, 15),
    round(volume_total * 35/100 / flow_rate, 15),
    round(volume_total * 15/100 / flow_rate, 15),
    round(volume_total * 7/100 / flow_rate, 15),
    round(volume_total * 10/100 / flow_rate, 15)) * 24

  # Assign usage schedules
  l_sched_use <- paste0("schedule:water-services:{",l_services,"}")

  # Assign hot water schedules
  l_sched_dhw <- c(
    'schedule:hot-water-services-warm',
    '',
    '',
    'schedule:hot-water-services-hot',
    'schedule:hot-water-services-hot',
    'schedule:hot-water-services-warm',
    '')

  # Create summary data frame.
  d_sum <- data.frame(
    service = l_services,
    zone = lbl_zones,
    vol = l_vol,
    schedule = l_sched_use,
    hw = l_sched_dhw
  ) %>% tibble::tibble()

  return(d_sum)
}


# water services ---------------------------------------------------------------

make_block_water_mains <- function(l_sup = l_idf_blocks){
  #' Creates an IDF block for water mains temperature.
  #'
  #' This function generates an IDF block for water mains temperature based on provided data.
  #'
  #' @param l_sup A list containing IDF block data.
  #'
  #' @return A character vector representing the IDF block.

  # Define block identifier and section title.
  iz <- "Site:WaterMainsTemperature"
  txt_title <- make_idf_block_title(iz)

  # Extract comments.
  com <- l_sup[[iz]]

  # Set water mains temperature parameters.
  lbl_method <- "Correlation"
  lbl_temp_avg <- 11.46
  lbl_delta_max <- 27.21

  # Create block content.
  obj <- c(iz, lbl_method, '',lbl_temp_avg, lbl_delta_max)

  # Add comments to block content.
  res <- add_idf_comment(obj, c("", com))

  # Create final IDF block.
  idf_res <- c(txt_title, '', res)

  return(idf_res)
}

make_block_water_connections <- function(d_ser, d_nodes, l_sup = l_idf_blocks){
  #' Creates IDF blocks for water connections.
  #'
  #' This function generates IDF blocks for water connections based on service and node data.
  #'
  #' @param d_ser A data frame containing service information.
  #' @param d_nodes A data frame containing node information.
  #' @param l_sup A list containing IDF block data (optional).
  #'
  #' @return A character vector representing the IDF block content.

  # Helper to create IDF
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]
    l_comment_base <- l_com[[i_act]][1:10]
    l_comment_ext <- rep('Water Use Equipment Name', length(l_in$zones))
    l_comment <- c(l_comment_base, l_comment_ext)

    # Define loop parameter list
    l_values <- list(
      namel = paste0(l_in$branch$branch, '--emitter'),
      nodei = l_in$branch$link_in,
      nodeo = l_in$branch$link_out,
      sptnk = '',
      reclm = '',
      hwsch = '',
      cosch = '',
      drnty = '',
      drnds = '',
      drnuv = '')

    # Prepend object type to parameter list
    l_block_set <- c(i_act, c(l_values,l_in$zones))

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Filter and prepare node data.
  d_node <- d_nodes %>% dplyr::filter(type == 'services') %>% dplyr::ungroup() %>%
    dplyr::select(-group, -has_adpipe, -has_pump, -type, -loc)
  d_node_cold <- d_node %>% dplyr::filter(grepl('cold$', branch))
  d_node_warm <- d_node %>% dplyr::filter(grepl('warm$', branch))

  # Categorize services based on hot water usage.
  d_ser <- d_ser %>% dplyr::mutate(is_warm = ifelse(hw == '', TRUE, FALSE))
  d_ser_cold <- d_ser %>% dplyr::filter(is_warm == TRUE)
  d_ser_warm <- d_ser %>% dplyr::filter(is_warm == FALSE)

  # Create data structure for cold and warm water connections.
  l_iter <- list(cold=list(branch = d_node_cold, zones = d_ser_cold$service),
                 warm=list(branch = d_node_warm, zones = d_ser_warm$service))

  # Active object for plant loop
  i_active <- "WaterUse:Connections"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_block_water_equipment <- function(d_w, l_sup = l_idf_blocks){
  #' Creates IDF blocks for water equipment.
  #'
  #' This function generates IDF blocks for water equipment based on water service data.
  #'
  #' @param d_w A data frame containing water service information.
  #' @param l_sup A list containing IDF block data (optional).
  #'
  #' @return A character vector representing the IDF block content.

  #' Helper function to create water equipment block.
  iterate_water_equipment <- function(i_ser, d_ser, l_coms){

    # Define active object and section title.
    i_active <- "WaterUse:Equipment"
    l_section <- make_idf_block_title(i_active)

    # Extract comments for the active object.
    l_com <- l_coms[[i_active]]

    # Extract service information.
    lbl_subcat <- "Water Services"
    lbl_zone <- d_ser$zone[d_ser$service==i_ser]
    lbl_vol <- d_ser$vol[d_ser$service==i_ser]
    lbl_flow_sched <- d_ser$schedule[d_ser$service==i_ser]
    lbl_dhw <- d_ser$hw[d_ser$service==i_ser]

    # Create block content.
    obj <- c(i_active, i_ser, lbl_subcat, lbl_vol,
             lbl_flow_sched, lbl_dhw, '', '', lbl_zone)

    # Add comments to block content.
    res <- add_idf_comment(obj, c("", l_com))

    return(res)
  }

  # Define active object and create block title.
  i_active <- 'Wateruse:Equipment'
  idf_title <- make_idf_block_title(i_active)

  # Create IDF blocks for each water service.
  idf_body <- lapply(d_w$service, iterate_water_equipment, d_w, l_sup) %>% unlist()

  # Combine title, empty lines, and block content.
  idf_res <- c(idf_title, '', idf_body)

  return(idf_res)
}


# performance curves -----------------------------------------------------------

generate_performance_curves <- function(l_used, l_curves = l_htg_curves, l_sup = l_idf_blocks){
  #' @title Generates performance curves in IDF format
  #'
  #' @description Creates IDF blocks for performance curves based
  #' on provided data.
  #'
  #' @param l_used: A vector of curve names to be included.
  #' @param l_curves: A list of curve data, indexed by curve type.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector containing the generated IDF blocks.

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_val, i_act, l_com=l_sup){
    # @title Creates an IDF block with comments
    # @description Constructs an IDF block from a list of values and a comment.
    # @param l_val A list of values for the IDF block.
    # @param i_act The object type for the IDF block.
    # @param l_com Supplementary data for comments.
    # @return A character vector representing the IDF block.

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- l_val %>% unlist() %>% as.character()

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Helper function to order biquadratic curves
  order_vector_biquadratic <- function(i, lc){
    # @title Orders curve data for biquadratic curves
    # @description Reorders curve data into a specific format for biquadratic curves.
    # @param i Index of the curve.
    # @param lc Curve data.
    # @return A character vector containing the ordered curve data.

    # Filter curve type
    lc <- lc[['Curve:Biquadratic']][[i]]

    # Define ordered vector
    l_order <- c(
      i,
      lc$coefficient1_constant,
      lc$coefficient2_x,
      lc$coefficient3_x_2,
      lc$coefficient4_y,
      lc$coefficient5_y_2,
      lc$coefficient6_x_y,
      lc$minimum_value_of_x,
      lc$maximum_value_of_x,
      lc$minimum_value_of_y,
      lc$maximum_value_of_y,
      '',
      '',
      lc$input_unit_type_for_x,
      lc$input_unit_type_for_y,
      lc$output_unit_type
    )

    return(l_order)
  }

  # Helper function to order cubic curves
  order_vector_cubic <- function(i, lc) {
    # @title Orders curve data for biquadratic curves
    # @description Reorders curve data into a specific format for biquadratic curves.
    # @param i Index of the curve.
    # @param lc Curve data.
    # @return A character vector containing the ordered curve data.

    # Filter curve type
    lc <- lc[["Curve:Cubic"]][[i]]

    # Define ordered vector
    l_order <- c(
      i,
      lc$coefficient1_constant,
      lc$coefficient2_x,
      lc$coefficient3_x_2,
      lc$coefficient4_x_3,
      lc$minimum_value_of_x,
      lc$maximum_value_of_x,
      lc$minimum_curve_output,
      lc$maximum_curve_output
    )
    return(l_order)
  }

  # Helper function to order exponent curves
  order_vector_exponent <- function(i, lc){
    # @title Orders curve data for biquadratic curves
    # @description Reorders curve data into a specific format for biquadratic curves.
    # @param i Index of the curve.
    # @param lc Curve data.
    # @return A character vector containing the ordered curve data.

    # Filter curve type
    lc <- lc[['Curve:Exponent']][[i]]

    # Define ordered vector
    l_order <- c(
      i,
      lc$coefficient1_constant,
      lc$coefficient2_constant,
      lc$coefficient3_constant,
      lc$minimum_value_of_x,
      lc$maximum_value_of_x,
      lc$minimum_curve_output,
      lc$maximum_curve_output)
    return(l_order)
  }

  # Helper function to order quadratic curves
  order_vector_quadratic <- function(i, lc){
    # @title Orders curve data for biquadratic curves
    # @description Reorders curve data into a specific format for biquadratic curves.
    # @param i Index of the curve.
    # @param lc Curve data.
    # @return A character vector containing the ordered curve data.

    # Filter curve type
    lc <- lc[['Curve:Quadratic']][[i]]

    # Define ordered vector
    l_order <- c(
      i,
      lc$coefficient1_constant,
      lc$coefficient2_x,
      lc$coefficient3_x_2,
      lc$minimum_value_of_x,
      lc$maximum_value_of_x,
      '',
      '',
      lc$input_unit_type_for_x,
      lc$output_unit_type
    )
    return(l_order)
  }

  # Helper function to order quadlinear curves
  order_vector_quadlinear <- function(i, lc){
    # @title Orders curve data for quadlinear curves
    # @description Reorders curve data into a specific format for biquadratic curves.
    # @param i Index of the curve.
    # @param lc Curve data.
    # @return A character vector containing the ordered curve data.

    # Filter curve type
    lc <- lc[['Curve:QuadLinear']][[i]]

    # Define ordered vector
    l_order <- c(
      i,
      lc$coefficient1_constant,
      lc$coefficient2_w,
      lc$coefficient3_x,
      lc$coefficient4_y,
      lc$coefficient5_z,
      lc$minimum_value_of_w,
      lc$maximum_value_of_w,
      lc$minimum_value_of_x,
      lc$maximum_value_of_x,
      lc$minimum_value_of_y,
      lc$maximum_value_of_y,
      lc$minimum_value_of_z,
      lc$maximum_value_of_z
    )

    return(l_order)
  }


  # Initialize variables for curve types, output list, and IDF text
  k_types <- names(l_curves)
  l_to <- list()
  txt_idf <- NULL

  # Iterate over curve types
  for(j in k_types){

    # Extract available curves of current type
    l_available <- l_curves[[j]]

    # Determine curves to generate based on used curves
    l_to_generate <- l_used[l_used %in% names(l_available)]

    # Select appropriate ordering function based on curve type
    fn_reorder <- switch (j,
                          "Curve:Biquadratic" = order_vector_biquadratic,
                          "Curve:Cubic" = order_vector_cubic,
                          "Curve:Exponent" = order_vector_exponent,
                          "Curve:Quadratic" = order_vector_quadratic,
                          "Curve:QuadLinear" = order_vector_quadlinear
    )

    # Apply ordering function to selected curves
    l_to <- lapply(l_to_generate, fn_reorder, l_curves)

    # Create block content for each loop in iterator, and combine
    txt_body <- lapply(l_to, make_idf_block, j) %>%
      unlist() %>% as.character()

    # Create block title
    l_head <- make_idf_block_title(j)

    # Append block title, empty lines, and block content to IDF text
    txt_idf <- c(txt_idf, '', l_head, '', txt_body)
  }

  # Return the complete IDF block content
  return(txt_idf)
}


# specific objects -------------------------------------------------------------

make_energy_block_plant_loop <- function(l_iter, d_loop, l_sup = l_idf_blocks){
  #' @title Create IDF block for Plant Loop based on loop data
  #'
  #' @description This function generates an IDF block representing a plant
  #' loop using loop data (`d_loop`) and an iterator (`l_iter`) defining loop
  #' properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namel = l_in$name,
      fluid = l_in$fluid,
      userf = '',
      plneq = paste0(l_in$name, '--operation-scheme'),
      loopt = l_in$plant_out,
      maxtm = l_in$temperature_max,
      mintm = l_in$temperature_min,
      maxlp = 'Autosize',
      minlp = 0,
      vollp = 'Autocalculate',
      plnin = l_in$plant_in,
      plnou = l_in$plant_out,
      plnbr = l_in$plant_branches,
      plncn = l_in$plant_connectors,
      demin = l_in$demand_in,
      demou = l_in$demand_out,
      dembr = l_in$demand_branches,
      demcn = l_in$demand_connectors,
      loadd = l_in$distribution,
      avail = '',
      plndm = 'SingleSetpoint',
      commn = 'None')

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Helper function to obtain loop parameters
  obtain_components <- function(i, d_lp){
    # @title Extract loop node information based on loop type
    #
    # @description This function extracts information about nodes-connections
    # for a specific loop type (`i`) from a loop data frame (`d_lp`).
    #
    # @param i: Character string representing the loop type
    # @param d_lp: Data frame containing loop information (groups, types, ...)
    #
    # @return A list containing information about plant inlet/outlet,
    # demand inlet/outlet, branches, and connectors for the  loop type.

    # Define node information based on loop type
    if(i=='triple_loop'){
      k_group <- c('xls','xld')
      k_branch <- list(supply='xls--branches', demand='xld--branches')
      k_cnnctr <- list(supply='xls--connector-list', demand='xld--connector-list')
    }else if(i=='dhw_loop'){
      k_group <- c('wlss','wlsd')
      k_branch <- list(supply='wlss--branches', demand='wlsd--branches')
      k_cnnctr <- list(supply='wlss--connector-list', demand='wlsd--connector-list')
    }else{
      k_group <- c('wls','wld')
      k_branch <- list(supply='wls--branches', demand='wld--branches')
      k_cnnctr <- list(supply='wls--connector-list', demand='wld--connector-list')
    }

    # Filter loop data for relevant groups and types
    d_loop_sub <- d_lp %>% dplyr::ungroup() %>%
      dplyr::filter(group %in% k_group & type == 'distribution')

    # Separate supply and demand data based on group name pattern
    d_loop_sub_s <- d_loop_sub %>% dplyr::filter(grepl('s$',group)) %>% dplyr::arrange(loc)
    d_loop_sub_d <- d_loop_sub %>% dplyr::filter(grepl('d$',group)) %>% dplyr::arrange(loc)

    # Extract node information from filtered data
    l_res <- list(
      plant_in = d_loop_sub_s %>% dplyr::filter(loc == "in") %>%
        dplyr::select(link_in) %>% unlist() %>% as.character(),
      plant_out = d_loop_sub_s %>% dplyr::filter(loc == "out") %>%
        dplyr::select(link_out) %>% unlist() %>% as.character(),
      demand_in = d_loop_sub_d %>% dplyr::filter(loc == "in") %>%
        dplyr::select(link_in) %>% unlist() %>% as.character(),
      demand_out = d_loop_sub_d %>% dplyr::filter(loc == "out") %>%
        dplyr::select(link_out) %>% unlist() %>% as.character(),
      plant_branches = k_branch$supply,
      plant_connectors = k_cnnctr$supply,
      demand_branches = k_branch$demand,
      demand_connectors = k_cnnctr$demand
    )

    # Return the extracted node information
    return(l_res)
  }


  # Active object for plant loop
  i_active <- "PlantLoop"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), obtain_components, d_loop)
  names(l_nodes) <- names(l_iter)

  # Update loop iterator with nodes
  l_iter <- modifyList(l_iter, l_nodes)

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_plant_sizing <- function(l_iter, l_sup = l_idf_blocks){
  #' @title Create IDF block for Sizing Plant based on loop data
  #'
  #' @description This function generates an IDF block representing a sized
  #' plant loop using an iterator (`l_iter`) defining loop properties.
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namel = l_in$name,
      loopt = l_in$type,
      texit = l_in$temperature_exit,
      dsgnt = 18,
      sizop = 'NonCoincident',
      steps = 1,
      coinf = 'None')

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
  i_active <- "Sizing:Plant"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_zone_sizing <- function(l_iter, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for Sizing Zone based on loop data
  #'
  #' @description This function generates an IDF block representing a plant
  #' loop using zone data (`d_zone`) and an iterator (`l_iter`) defining loop
  #' properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      zonen = l_in,
      clgin = 'SupplyAirTemperature',
      clgtm = 14,
      clgdf = 11.11,
      hgtin = 'SupplyAirTemperature',
      htgtm = 40,
      htgdf = 11.12,
      clghr = 0.0085,
      htghr = 0.008,
      outar = 'outdoor-air-node',
      htgsz = '',
      clgsz = '',
      clgam = 'DesignDay',
      clgar = 0,
      clgtf = '',
      clgmi = '',
      clgmf = 0,
      htgam = 'DesignDay',
      htgar = 0,
      htgtf = 0.002032,
      htgmi = 0.1415762,
      htgmf = 0.3,
      dspec = paste0(l_in, '--design-spec-zone-air-dist'),
      acair = 'No',
      dectr = 'NeutralSupplyAir',
      delow = 'autosize',
      dehgh = 'autosize',
      zlsmt = 'Sensible Load Only No Latent Load',
      zlcds = 'HumidityRatioDifference',
      zddsa = '',
      zcdsa = 0.005,
      zlhds = 'HumidityRatioDifference',
      zhdsa = '',
      zhdsr = 0.005)

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
  i_active <- "Sizing:Zone"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_zone$zone, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_zone_air_dist <- function(l_iter, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for Zone Air Distribution based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' air distribution using zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      zonen = paste0(l_in, '--design-spec-zone-air-dist'),
      clgtm = 1,
      clgdf = 1)

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
  i_active <- "DesignSpecification:ZoneAirDistribution"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_zone$zone, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_setpoint_manager <- function(l_iter, d_loop, l_sup = l_idf_blocks){
  #' @title Create IDF block for Setpoint Manager based on loop data
  #'
  #' @description This function generates an IDF block representing a setpoint
  #' manager using loop data (`d_loop`) and an iterator (`l_iter`) defining loop
  #' properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define name of schedule
    k_sched <- paste0('schedule:',l_in$name, '--setpoint-temperature') %>%
      gsub('\\s\\d+','',.)

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$name, '--setpoint'),
      ctrlv = 'Temperature',
      sched = k_sched,
      sptmp = l_in$node_sp)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Helper function to obtain loop parameters
  obtain_sp_nodes <- function(i, d_lp){
    #' @title Extract loop node information based on loop type
    #'
    #' @description This function extracts information about nodes-connections
    #' for a specific loop type (`i`) from a loop data frame (`d_lp`).
    #'
    #' @param i: Character string representing the loop type
    #' @param d_lp: Data frame containing loop information (groups, types, ...)
    #'
    #' @return A list containing information about plant inlet/outlet,
    #' demand inlet/outlet, branches, and connectors for the  loop type.
  

    # Define node information based on loop type
    k_filter <- k_type <- 'distribution'
    k_loc <- 'out'

    if(i=='triple_loop'){
      k_group <- c('xls','wls')
      k_filter <- 'exchanger'
      k_type <- c(k_type, k_filter)
      k_loc <- c(k_loc, 'mid')
    }else if(i=='dhw_loop'){
      k_group <- c('wlss','wlsd')
    }else{
      k_group <- c('wls','wld')
    }

    # Filter loop data for relevant groups and types
    d_loop_sub <- d_lp %>% dplyr::ungroup() %>%
      dplyr::filter(group %in% k_group & type %in% k_type & loc %in% k_loc) %>%
      dplyr::filter(grepl('s$',group)) %>% dplyr::arrange(loc) %>%
      dplyr::filter((type == 'distribution' & group == 'xls') |
               (type == k_filter & grepl('wls', group)))

    # Extract node information from filtered data
    l_res <- list(
      node_sp = d_loop_sub %>% dplyr::select(link_out) %>% unlist() %>% as.character()
    )

    # Return the extracted node information
    return(l_res)
  }

  # Helper function to obtain nodes
  assign_sp_nodes <- function(i, li, ln){
    #' Assigns spatial nodes to data.
    #'
    #' This function assigns spatial nodes to data based on given parameters.
    #'
    #' @param i A character string specifying the node name.
    #' @param li A list containing data for different nodes.
    #' @param ln A list containing node information.
    #'
    #' @return A data frame with columns `name`, `fluid`, and `node_sp`.
    

    # Extract data for the specified node.
    d_up <- li[[i]] %>% data.frame()

    # Create a list to store node information.
    l_obj <- list()
    j <- 1

    # Iterate over node specific values.
    for(k in ln[[i]]$node_sp){

      # Update node data with current node_sp and name.
      d_up$node_sp <- k
      d_up$name <- ifelse(j>1, paste(d_up$name, j), d_up$name)

      # Append updated data to the list.
      l_obj[[j]] <- d_up
      j <- j+1

    }

    # Combine list elements into a data frame and select columns.
    d_res <- plyr::ldply(l_obj, data.frame) %>% tibble::tibble() %>%
      dplyr::select(name, fluid, node_sp)

    return(d_res)
  }


  # Active object for plant loop
  i_active <- "SetpointManager:Scheduled"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), obtain_sp_nodes, d_loop)
  names(l_nodes) <- names(l_iter)

  # Assign nodes from loop data using helper function
  d_nodes <- lapply(names(l_nodes), assign_sp_nodes, l_iter, l_nodes) %>%
    plyr::ldply(data.frame) %>% tibble::tibble()
  l_iter <- split(d_nodes, seq(1, nrow(d_nodes)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_branch <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Branch objects based on loop data
  #'
  #' @description This function generates an IDF block representing a branch
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$branch,
      press = '',
      otype = l_in$device_type,
      oname = l_in$device_name,
      olnin = l_in$link_in,
      olnou = l_in$link_out)

    # Make exception for combined elements in branch
    if(grepl('----',l_in$device_type)){

      k_types <- strsplit(l_in$device_type, '----') %>% unlist()
      k_names <- strsplit(l_in$device_name, '----') %>% unlist()

      l_values <- list(
        names = l_in$branch,
        press = '',
        otype = k_types[1],
        oname = k_names[1],
        olnin = l_in$link_in,
        olnou = paste0(l_in$link_out, '--', l_in$link_in),
        ptype = k_types[2],
        pname = k_names[2],
        plnin = paste0(l_in$link_out, '--', l_in$link_in),
        plnou = l_in$link_out)

      l_comment_extra <- gsub('1','2',l_comment[3:6])
      l_comment <- c(l_comment,l_comment_extra)
    }

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
  i_active <- "Branch"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble()
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_branch_list <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Branch List based on loop data
  #'
  #' @description This function generates an IDF block representing a branch
  #' list using loop data (`d_loop`), system info (`d_item`), and an
  #' iterator (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]
    l_comment <- c(l_comment[1], rep(l_comment[2], length(l_in$branch)))

    # Define loop parameter list
    l_values <- c(paste0(l_in$group[1],'--branches'), l_in$branch)

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
  i_active <- "BranchList"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::select(group, branch, loc) %>% dplyr::arrange(loc) %>% dplyr::group_by(group)
  l_nodes <- d_branches %>% dplyr::group_split()

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_nodes, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_branch_splitter <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Branch Splitter based on loop data
  #'
  #' @description This function generates an IDF block representing a branch
  #' splitter using loop data (`d_loop`), system info (`d_item`), and an
  #' iterator (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]
    l_comment <- c(l_comment[1:2], rep(l_comment[3], length(l_in)))

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_in)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Helper function to filter water loops data
  filter_branches <- function(df){
    # @title Filter branches based on group and location
    # @description This function filters branches from a data frame based
    # on group name pattern and location.
    #
    # @param df: A data frame containing columns named 'group' and 'loc'.
    # @return: A list of filtered branches.

    # Extract the first element of the 'group' column
    k_set <- df$group[1]

    # Filter data frame based on location and group pattern
    df_fil <- df %>% dplyr::filter(loc %in% c('in', 'mid')) %>% dplyr::arrange(loc)

    # Extract the filtered branches
    l_res <- c(paste0(k_set,'--splitter'), df_fil$branch)

    # Return the list of filtered branches
    return(l_res)
  }

  # Active object for plant loop
  i_active <- "Connector:Splitter"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::select(group, branch,loc) %>% dplyr::group_by(group)
  l_nodes <- d_branches %>% dplyr::group_split()

  # Filter loop data using helper function
  l_branches <- lapply(l_nodes, filter_branches)

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_branch_mixer <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Branch Splitter based on loop data
  #'
  #' @description This function generates an IDF block representing a branch
  #' splitter using loop data (`d_loop`), system info (`d_item`), and an
  #' iterator (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]
    l_comment <- c(l_comment[1:2], rep(l_comment[3], length(l_in)))

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_in)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Helper function to filter water loops data
  filter_branches <- function(df){
    # @title Filter branches based on group and location
    #
    # @description This function filters branches from a data frame based
    # on group name pattern and location.
    #
    # @param df: A data frame containing columns named 'group' and 'loc'.
    # @return: A list of filtered branches.


    # Extract the first element of the 'group' column
    k_set <- df$group[1]

    # Filter data frame based on location and group pattern
    df_fil <- df %>% dplyr::filter(loc %in% c('out', 'mid')) %>% dplyr::arrange(dplyr::desc(loc))

    # Extract the filtered branches
    l_res <- c(paste0(k_set,'--mixer'), df_fil$branch)

    # Return the list of filtered branches
    return(l_res)
  }


  # Active object for plant loop
  i_active <- "Connector:Mixer"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::select(group, branch,loc) %>% dplyr::group_by(group)
  l_nodes <- d_branches %>% dplyr::group_split()

  # Filter loop data using helper function
  l_branches <- lapply(l_nodes, filter_branches)

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_connector_list <- function(l_iter, d_loop, l_sup = l_idf_blocks){
  #' @title Create IDF block for Connector List based on loop data
  #'
  #' @description This function generates an IDF block representing a connector
  #' list using loop data (`d_loop`) and an iterator (`l_iter`) defining loop
  #' properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namec = paste0(l_in,'--connector-list'),
      objat = 'Connector:Splitter',
      objan = paste0(l_in,'--splitter'),
      objbt = 'Connector:Mixer',
      objbn = paste0(l_in,'--mixer'))

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
  i_active <- "ConnectorList"

  # Extract nodes from loop data using helper function
  l_nodes <- unique(d_loop$group)

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_nodes, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_adpipe <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Pipe:Adiabatic objects based on loop data
  #'
  #' @description This function generates an IDF block representing a pipe
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      olnin = l_in$link_in,
      olnou = l_in$link_out)

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
  i_active <- "Pipe:Adiabatic"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(has_adpipe == TRUE) %>% dplyr::select(-has_adpipe, -has_pump)
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_pump <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Pump:VariableSpeed objects based on loop data
  #'
  #' @description This function generates an IDF block representing a pump
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      nodin = l_in$link_in,
      nodou = l_in$link_out,
      demax = 'Autosize',
      dhead = 861844.7,
      depwr = 'Autosize',
      mteff = 0.93,
      mtfra = 0,
      coefa = 0,
      coefb = 1,
      coefc = 0,
      coefd = 0,
      demin = 0,
      pmctl = 'Continuous',
      pmrte = '',
      pmcve = '',
      implr = '',
      vfdct = '',
      rpmsc = '',
      pamin = '',
      pamax = '',
      rpmmi = '',
      rpmmx = '',
      zonen = '',
      skinl = 0.5,
      pwrmt = 'PowerPerFlowPerPressure',
      elecp = 348701.1,
      shaft = 1.282051282,
      dflfr = 0)

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
  i_active <- "Pump:VariableSpeed"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(has_pump == TRUE) %>% dplyr::select(-has_adpipe, -has_pump)
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_plant_equipment <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for PlantEquipmentList objects based on loop data
  #'
  #' @description This function generates an IDF block representing a plant
  #' equipment list using loop data (`d_loop`), system info (`d_item`), and
  #' an iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$loop, '--heating-equipment-list'),
      dtype = l_in$device_type,
      nodin = l_in$device_name)

    # Make exception for combined elements in branch
    if(grepl('----',l_in$device_type)){

      k_types <- strsplit(l_in$device_type, '----') %>% unlist()
      k_names <- strsplit(l_in$device_name, '----') %>% unlist()

      l_values <- list(
        names = paste0(l_in$loop, '--heating-equipment-list'),
        dtype = k_types[1],
        nodin = k_names[1],
        ptype = k_types[2],
        pname = k_names[2])

    }

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
  i_active <- "PlantEquipmentList"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- lapply(l_iter, '[[', "name") %>% unlist() %>% as.character()

  # Merge lists of branches

  if('hp-water-loop' %in% names(l_nodes)){
    d_branches <- plyr::ldply(l_nodes, data.frame, .id = 'loop') %>% tibble::tibble() %>%
      dplyr::filter(grepl('^heating',type) & grepl('*.s$',group)) %>%
      dplyr::select(-has_adpipe, -has_pump) %>%
      dplyr::mutate(loop = ifelse(grepl('^wls.*hp\\-heater$',branch),
                           'main-water-loop', as.character(loop)))

  }else{
    d_branches <- plyr::ldply(l_nodes, data.frame, .id = 'loop') %>% tibble::tibble() %>%
      dplyr::filter(type == 'heating' & grepl('*.s$',group)) %>%
      dplyr::select(-has_adpipe, -has_pump) %>%
      dplyr::mutate(loop = ifelse(grepl('hp\\-heater$',branch),
                           'main-heat-pump', as.character(loop)))
  }

  # Split rows into lists
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_plant_operation <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for PlantEquipmentOperation:HeatingLoad objects
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a plant
  #' equipment operation heating load object using loop data (`d_loop`),
  #' system info (`d_item`), and an iterator (`l_iter`) defining loop
  #' properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in, '--heating-operation-scheme'),
      lowli = 0,
      higli = '1000000000',
      range = paste0(l_in, '--heating-equipment-list'))

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
  i_active <- "PlantEquipmentOperation:HeatingLoad"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- lapply(l_iter,'[[','name') %>% unlist() %>% as.character()

  # Merge lists of branches
  if('hp-water-loop' %in% names(l_nodes)){
    d_branches <- plyr::ldply(l_nodes, data.frame, .id = 'loop') %>% tibble::tibble() %>%
      dplyr::filter(grepl('^heating',type) & grepl('*.s$',group)) %>%
      dplyr::select(-has_adpipe, -has_pump) %>%
      dplyr::mutate(loop = ifelse(grepl('^wls.*hp\\-heater$',branch),
                           'main-water-loop', as.character(loop)))

  }else{
    d_branches <- plyr::ldply(l_nodes, data.frame, .id = 'loop') %>% tibble::tibble() %>%
      dplyr::filter(type == 'heating' & grepl('*.s$',group)) %>%
      dplyr::select(-has_adpipe, -has_pump) %>%
      dplyr::mutate(loop = ifelse(grepl('hp\\-heater$',branch),
                           'main-heat-pump', as.character(loop)))
  }

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_branches$loop, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_plant_schemes <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for PlantEquipmentOperationSchemes objects
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a plant
  #' equipment operation scheme object using loop data (`d_loop`),
  #' system info (`d_item`), and an iterator (`l_iter`) defining loop
  #' properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in, '--operation-scheme'),
      lowli = 'PlantEquipmentOperation:HeatingLoad',
      higli = paste0(l_in, '--heating-operation-scheme'),
      range = 'Always On')

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
  i_active <- "PlantEquipmentOperationSchemes"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- lapply(l_iter,'[[','name') %>% unlist() %>% as.character()

  # Merge lists of branches
  if('hp-water-loop' %in% names(l_nodes)){
    d_branches <- plyr::ldply(l_nodes, data.frame, .id = 'loop') %>% tibble::tibble() %>%
      dplyr::filter(grepl('^heating',type) & grepl('*.s$',group)) %>%
      dplyr::select(-has_adpipe, -has_pump) %>%
      dplyr::mutate(loop = ifelse(grepl('^wls.*hp\\-heater$',branch),
                           'main-water-loop', as.character(loop)))

  }else{
    d_branches <- plyr::ldply(l_nodes, data.frame, .id = 'loop') %>% tibble::tibble() %>%
      dplyr::filter(type == 'heating' & grepl('*.s$',group)) %>%
      dplyr::select(-has_adpipe, -has_pump) %>%
      dplyr::mutate(loop = ifelse(grepl('hp\\-heater$',branch),
                           'main-heat-pump', as.character(loop)))
  }

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_branches$loop, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_zone_thermostat <- function(l_iter, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneControl:Thermostat based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' thermostat using zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      zonet = paste0(l_in, '--thermostat'),
      zonem = l_in,
      zones = paste0('schedule:', 'thermostat-control-type'),
      typet = 'ThermostatSetpoint:DualSetpoint',
      ctrlt = paste0(l_in, '--thermostat-control'))

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
  i_active <- "ZoneControl:Thermostat"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_zone$zone, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_thermostat_control <- function(l_iter, d_zone, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for ThermostatSetpoint:DualSetpoint
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a
  #' thermostat control using an iterator (`l_iter`), zone data (`d_zone`),
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, d_ref, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    d_sub <- d_ref %>% dplyr::filter(zone == l_in) %>%
      dplyr::select(zone, htg_schedules, clg_schedules)

    # Define loop parameter list
    l_values <- list(
      zonet = paste0(l_in, '--thermostat-control'),
      typet = d_sub$htg_schedules,
      typet = d_sub$clg_schedules)

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
  i_active <- "ThermostatSetpoint:DualSetpoint"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_zone$zone, make_idf_block, d_zone, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_water_radiator <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:Baseboard:RadiantConvective:Water
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' emitter using loop data (`d_loop`) and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_rad, i_rgm, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Obtain thermal zone
    l_zn <- l_in$device_name %>%
      gsub('\\-\\-emitter$','',.) %>% gsub('\\(|\\)','',.)

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      named = paste0(l_in$device_name, '-design-object'),
      regim = paste0('schedule:', 'heating-availability-{',i_rgm,'}'),
      nodin = l_in$link_in,
      nodou = l_in$link_out,
      avgtm = i_rad,
      wmass = 0.063,
      htgde = 'AutoSize',
      maxwt = 'AutoSize',
      paste0(l_zn,':bottom'),
      0.05,
      paste0(l_zn,':front'),
      0.175,
      paste0(l_zn,':rear'),
      0.175,
      paste0(l_zn,':left'),
      0.225,
      paste0(l_zn,':right'),
      0.225)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'emitters') %>% dplyr::select(-has_adpipe, -has_pump)

  # Active object for plant loop
  #   eg. ZoneHVAC:Baseboard:RadiantConvective:Water
  i_active <- d_branches$device_type[1]
  i_regime <- d_item$main_emitters
  i_radtmp <- d_item$radiators_temperature

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_radtmp, i_regime, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_wall_radiator_design <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:Baseboard:RadiantConvective:Water:Design
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' emitter design object using loop data (`d_loop`) and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Obtain thermal zone
    l_zn <- l_in$device_name %>%
      gsub('\\-\\-emitter$','',.) %>% gsub('\\(|\\)','',.)

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$device_name, '-design-object'),
      htgmt = 'HeatingDesignCapacity',
      htgin = '',
      autof = '',
      cnvrg = 0.001,
      frcrd = 0.3,
      radpp = 0.15)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'emitters') %>% dplyr::select(-has_adpipe, -has_pump)

  # Active object for plant loop
  #   eg. ZoneHVAC:Baseboard:RadiantConvective:Water
  i_active <- 'ZoneHVAC:Baseboard:RadiantConvective:Water:Design'

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_underfloor_radiator <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:LowTemperatureRadiant:VariableFlow
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' emitter using loop data (`d_loop`) and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_rad, i_rgm, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Obtain thermal zone
    l_zn <- l_in$device_name %>%
      gsub('\\-\\-emitter$','',.) %>% gsub('\\(|\\)','',.)

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      desin = 'radiant-floor-design',
      avlsn = paste0('schedule:', 'heating-availability-{',i_rgm,'}'),
      zonnm = paste0('(',l_zn,')'),
      snorsgn = paste0(l_zn,':bottom'),
      hytlm = 'autosize',
      htdcw = 'autosize',
      maxflw = 0.00010,
      hwinn = l_in$link_in,
      hwonn = l_in$link_out,
      collmet = '',
      maxcold = 0.0035,
      cwinn = '',
      cwonn = '',
      nocir = '',
      crlen = '')

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'emitters') %>% dplyr::select(-has_adpipe, -has_pump)

  # Active object for plant loop
  #   eg. ZoneHVAC:LowTemperatureRadiant:VariableFlow
  i_active <- d_branches$device_type[1]
  i_regime <- d_item$main_emitters
  i_radtmp <- d_item$radiators_temperature

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_radtmp, i_regime, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_underfloor_radiator_design <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:LowTemperatureRadiant:VariableFlow:Design
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' emitter design object using loop data (`d_loop`) and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      names = 'radiant-floor-design',
      'ConvectionOnly',
      htidm = 0.0130,
      htodm = 0.0160,
      htcon = 0.3500,
      tmpct = 'MeanAirTemperature',
      sethp = 'HalfFlowPower',
      htdcm = 'HeatingDesignCapacity',
      hdcpfaw = '',
      foahdc = '',
      hctrd = 2.00,
      hctsn = 'schedule:day-radiant-heat',
      cldcm = '',
      cldcw = '',
      cdcpfaw = '',
      cctrd = '',
      cwinn = '',
      cwonn = '',
      cctsn = '')

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
  i_active <- 'ZoneHVAC:LowTemperatureRadiant:VariableFlow:Design'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_electric_radiator <- function(l_iter, d_zone, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:Baseboard:RadiantConvective:Electric
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' emitter using zone data (`d_zone`) and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_rgm, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Obtain thermal zone
    l_zn <- l_in$device_name %>%
      gsub('\\-\\-emitter$','',.) %>% gsub('\\(|\\)','',.)

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      regim = paste0('schedule:', 'heating-availability-{',i_rgm,'}'),
      methd = 'HeatingDesignCapacity',
      htgde = 'AutoSize',
      capfa = 0,
      capau = 1,
      effbs = 0.99,
      frcrd = 0.3,
      radpp = 0.15,
      paste0(l_zn,':bottom'),
      0.05,
      paste0(l_zn,':front'),
      0.175,
      paste0(l_zn,':rear'),
      0.175,
      paste0(l_zn,':left'),
      0.225,
      paste0(l_zn,':right'),
      0.225)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Merge lists of branches
  d_branches <- d_zone %>% dplyr::select(zone, htg_available, thermostat) %>%
    dplyr::mutate(zone_id = zone) %>%
    dplyr::mutate(device_type = d_item$main_emitter,
           device_name = paste0(zone_id, '--emitter'))

  # Active object for device
  i_active <- 'ZoneHVAC:Baseboard:RadiantConvective:Electric'
  i_regime <- d_item$main_emitters

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_regime, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_electric_room_radiator <- function(l_iter, d_zone, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:Baseboard:Convective:Electric
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' emitter using zone data (`d_zone`) and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_rgm, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      regim = paste0('schedule:', 'heating-availability-{',i_rgm,'}'),
      methd = 'HeatingDesignCapacity',
      htgde = 'AutoSize',
      capfa = 0,
      capau = 1,
      effbs = 0.99)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Merge lists of branches
  d_branches <- d_zone %>% dplyr::select(zone, htg_available, thermostat) %>%
    dplyr::mutate(zone_id = zone) %>%
    dplyr::mutate(device_type = d_item$main_emitter,
           device_name = paste0(zone_id, '--emitter'))

  # Active object for device
  i_active <- 'ZoneHVAC:Baseboard:Convective:Electric'
  i_regime <- d_item$main_emitters

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_regime, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_equipment <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:EquipmentList objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' equipment list using loop data (`d_loop`), zone info (`d_zone`) and an
  #' iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      names = paste0(k_zone, '--equipment-list'),
      regim = 'SequentialLoad',
      nodin = l_in$device_type,
      nodou = l_in$device_name,
      htgmt = 1,
      htgde = 1,
      htgmt = '',
      htgde = '')

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
  i_active <- 'ZoneHVAC:EquipmentList'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_equipment_con <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:EquipmentConnections objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' equipment connection using loop data (`d_loop`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      namez = k_zone,
      names = paste0(k_zone, '--equipment-list'),
      inlet = '',
      exhst = '',
      airnd = paste0(k_zone, '--node-air'),
      rtrnn = paste0(k_zone, '--return-air'))

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
  i_active <- 'ZoneHVAC:EquipmentConnections'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_outdoor_air <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for OutdoorAir:Node objects based on loop data
  #'
  #' @description This function generates an IDF block representing an air
  #' node using default properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'outdoor-air-node',
      methd = 'sum',
      perpp = 0.00236,
      perza = 0.00305,
      perzn = 0.0,
      perac = 0.0,
      sched = '')

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
  i_active <- 'DesignSpecification:OutdoorAir'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_boiler <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Boiler:HotWater objects based on loop data
  #'
  #' @description This function generates an IDF block representing a boiler
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      fuelb = l_in$fuel,
      nompw = 'Autosize',
      bleff = l_in$efficiency,
      effcv = 'LeavingBoiler',
      normb = 'Boiler Efficiency',
      wtflw = 'Autosize',
      minpl = 0,
      maxpl = 1,
      optpl = 1,
      nodin = l_in$link_in,
      nodou = l_in$link_out,
      wtrou = 99,
      flowm = 'NotModulated',
      paras = 0,
      sizef = 1)

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
  i_active <- "Boiler:HotWater"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'heating') %>% dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^Boiler',device_type)) %>%
    dplyr::mutate(fuel = d_item$main_fuel,
           efficiency = ifelse(d_item$main_efficiency > 1,
                               0.885, d_item$main_efficiency))

  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heater_sizing <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:Sizing objects based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      desmd = 'PerFloorArea',
      storg = 0.6,
      tankr = 0.6,
      nomvl = 0.185,
      nobed = 4.0,
      nobat = 2.0,
      strca = 0.2,
      recov = 0.2,
      catfa = 0.02,
      recfa = 0.02,
      nunit = 4.0,
      stoun = 0.2,
      recun = 0.2,
      colun = 0.2,
      ratio = 1.0)

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
  i_active <- "WaterHeater:Sizing"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'heating' & grepl('.*s$',group)) %>%
    dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^Water', device_type)) %>%
    dplyr::mutate(fuel = d_item$dhw_fuel) %>%
    dplyr::mutate(device_name = ifelse(grepl('PumpedCondenser$',device_type),
                                paste0(device_name,'--vessel'),device_name))

  # Split list of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_water_re_heater <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:Mixed objects based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      tnkvl = l_in$cylinder/1000,
      sttsn = paste0('schedule:', l_in$device_name, '--setpoint-temperature'),
      ddbtd = 20,
      mxmtl = 82.22,
      htrct = 'Cycle',
      htrmc = 'Autosize',
      minpw = 0,
      minfr = 0,
      minid = 0,
      htrft = l_in$fuel,
      htrte = 0.91,
      plfcn = '',
      parpw = 20,
      parft = l_in$fuel,
      partt = 0.72,
      parfc = 0,
      parft = l_in$fuel,
      partk = 0,
      ambti = 'Schedule',
      amtsn = 'schedule:water-heater-ambient-temperature',
      amtzn = '',
      atoam = '',
      losst = l_in$cylinder_loss,
      lossf = 1,
      losss = l_in$cylinder_loss,
      lossg = 1,
      pkufr = '',
      ufrfsn = '',
      cwstsn = '',
      usinn = l_in$link_in,
      usonn = l_in$link_out,
      ussde = 1,
      ssinn = l_in$source_in,
      ssonn = l_in$source_out,
      srcse = 1,
      usdfr = 'Autosize',
      ssdfr = 'Autosize',
      iwhrt = 1.5)

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
  i_active <- "WaterHeater:Mixed"
  k_loop <- 'dhw'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'heating' & grepl('^dhw',device_name)) %>%
    dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^Water', device_type)) %>%
    dplyr::mutate(fuel = d_item$dhw_fuel,
           cylinder = ifelse(is.na(d_item$main_cylinder), 180, d_item$main_cylinder),
           efficiency = ifelse(is.na(d_item$main_efficiency), 0.88, d_item$main_efficiency),
           cylinder_loss = ifelse(is.na(d_item$cylinder_loss), 2.36, d_item$cylinder_loss))

  # Split supply-demand, to pick inlet/outlets
  d_heater_s <- d_branches %>% dplyr::filter(grepl('.*s$',group))
  d_heater_d <- d_branches %>% dplyr::filter(grepl('.*d$',group))

  # Append source inlet/outlets
  d_heater_s <- d_heater_s %>%
    dplyr::mutate(source_in = d_heater_d$link_in,
           source_out = d_heater_d$link_out)

  # Split main/hw, to pick inlet/outlets
  if(dim(d_heater_s)[1]>1){
    d_heater_s_msh <- d_heater_s %>% dplyr::filter(grepl('^main',.id))
    d_heater_s_dhw <- d_heater_s %>% dplyr::filter(grepl('^dhw',.id)) %>%
      dplyr::mutate(cylinder = 1.5)
    if(k_loop=='msh'){
      d_heater_s <- d_heater_s_msh
    }else{
      d_heater_s <- d_heater_s_dhw
    }
  }

  # Split list of branches
  l_branches <- split(d_heater_s, seq(1, nrow(d_heater_s)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_water_heater <- function(l_iter, d_loop, d_item, k_loop='msh', l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:Mixed objects based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param k_loop: Vector indicating loop to be processed
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      tnkvl = l_in$cylinder/1000,
      sttsn = paste0('schedule:', l_in$device_name, '--setpoint-temperature'),
      ddbtd = 20,
      mxmtl = 82.22,
      htrct = 'Cycle',
      htrmc = 'Autosize',
      minpw = 0,
      minfr = 0,
      minid = 0,
      htrft = l_in$fuel,
      htrte = l_in$efficiency,
      plfcn = '',
      parpw = 20,
      parft = l_in$fuel,
      partt = 0.72,
      parfc = 0,
      parft = l_in$fuel,
      partk = 0,
      ambti = 'Schedule',
      amtsn = 'schedule:water-heater-ambient-temperature',
      amtzn = '',
      atoam = '',
      losst = l_in$cylinder_loss,
      lossf = 1,
      losss = l_in$cylinder_loss,
      lossg = 1,
      pkufr = '',
      ufrfsn = '',
      cwstsn = '',
      usinn = l_in$link_in,
      usonn = l_in$link_out,
      ussde = 1,
      ssinn = '',
      ssonn = '',
      srcse = 1,
      usdfr = 'Autosize',
      ssdfr = 'Autosize',
      iwhrt = 1.5)

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
  i_active <- "WaterHeater:Mixed"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'heating') %>%
    dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^Water', device_type)) %>%
    dplyr::mutate(fuel = d_item$dhw_fuel,
           cylinder = ifelse(is.na(d_item$main_cylinder), 180, d_item$main_cylinder),
           efficiency = ifelse(is.na(d_item$main_efficiency), 0.88, d_item$main_efficiency),
           cylinder_loss = ifelse(is.na(d_item$cylinder_loss), 2.36, d_item$cylinder_loss))

  # Split supply-demand, to pick inlet/outlets
  d_heater_s <- d_branches %>% dplyr::filter(grepl('.*s$',group))
  d_heater_d <- d_branches %>% dplyr::filter(grepl('.*d$',group))

  # Split supply-demand, and then main/hw, to pick inlet/outlets
  if(dim(d_heater_s)[1]>1){
    d_heater_s_msh <- d_heater_s %>% dplyr::filter(grepl('^main',.id))
    d_heater_s_dhw <- d_heater_s %>% dplyr::filter(grepl('^dhw',.id)) %>%
      dplyr::mutate(cylinder = 1.5)
    if(k_loop=='msh'){
      d_heater_s <- d_heater_s_msh
      if(grepl('\\-pump$',d_heater_s$device_name)) d_heater_s$efficiency <- 0.94
    }else{
      d_heater_s <- d_heater_s_dhw
    }
  }

  # Split list of branches
  l_branches <- split(d_heater_s, seq(1, nrow(d_heater_s)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_district_heat_mains <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for DistrictHeating objects based on loop data
  #'
  #' @description This function generates an IDF block representing a
  #' district heating mains connection using loop data (`d_loop`), system
  #' info (`d_item`), and an iterator (`l_iter`) defining loop properties.
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      nodin = l_in$link_in,
      nodou = l_in$link_out,
      nompw = 'Autosize')

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
  i_active <- "DistrictHeating:Water"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'heating') %>% dplyr::select(-has_adpipe, -has_pump) %>%
    dplyr::filter(grepl('^DistrictH', device_type)) %>%
    dplyr::mutate(fuel = d_item$main_fuel,
           efficiency = d_item$main_efficiency)

  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:HeatPump:PumpedCondenser object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a heat-pump
  #' using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      name = l_in$device_name,
      avlsn = 'Always On',
      cstsn = 'schedule:main-water-loop--setpoint-temperature',
      dbtd = 11.0,
      cwinn = l_in$condenser_in,
      cwonn = l_in$condenser_out,
      cwfr = 'Autocalculate',
      eafr = 'Autocalculate',
      inlac = 'OutdoorAirOnly',
      arinn = '',
      aronn = '',
      otann = 'hp-heater-outdoor-air-node',
      exann = 'hp-water-heater-exhaust-air-node',
      iatsn = '',
      iahsn = '',
      inazn = '',
      tnkot = l_in$source_type,
      tnknm = l_in$source_name,
      tusinn = l_in$link_in,
      tusonn = l_in$link_out,
      dxcot = l_in$coil_type,
      dxcln = l_in$coil_name,
      miatfco = -4.8,
      mtrerrn = 48.9,
      cmprl = 'Outdoors',
      catsn = '',
      fnobt = 'Fan:OnOff',
      fannm = 'hp-supply-fan',
      fnplc = 'DrawThrough',
      ocpel = 0,
      feccd = 0,
      prhrl = 'Outdoors',
      iamnn = '',
      oasnn = '',
      iamsn = '',
      tnecl = 'Simultaneous',
      cs1hist = 0,
      cs1w = 0,
      cs2hist = 0)

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
  i_active <- "WaterHeater:HeatPump:PumpedCondenser"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components(l_nodes, d_item)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_w_auxiliary <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:HeatPump:PumpedCondenser object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a heat-pump
  #' using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      name = l_in$tank_name,
      avlsn = 'Always On',
      cstsn = 'schedule:main-water-loop--setpoint-temperature',
      dbtd = 11.0,
      cwinn = l_in$coil_in,
      cwonn = l_in$coil_out,
      cwfr = 'Autocalculate',
      eafr = 'Autocalculate',
      inlac = 'OutdoorAirOnly',
      arinn = '',
      aronn = '',
      otann = 'hp-heater-outdoor-air-node',
      exann = 'hp-water-heater-exhaust-air-node',
      iatsn = '',
      iahsn = '',
      inazn = '',
      tnkot = l_in$heater_use_type,
      tnknm = paste0(l_in$tank_name, '--tank'),
      tusinn = l_in$hx_in,
      tusonn = l_in$hx_out,
      dxcot = l_in$coil_type,
      dxcln = l_in$coil_name,
      miatfco = -4.8,
      mtrerrn = 48.9,
      cmprl = 'Outdoors',
      catsn = '',
      fnobt = 'Fan:OnOff',
      fannm = 'hp-supply-fan',
      fnplc = 'DrawThrough',
      ocpel = 0,
      feccd = 0,
      prhrl = 'Outdoors',
      iamnn = '',
      oasnn = '',
      iamsn = '',
      tnecl = 'Simultaneous',
      cs1hist = 0,
      cs1w = 0,
      cs2hist = 0)

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
  i_active <- "WaterHeater:HeatPump:PumpedCondenser"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components_w_auxiliary(l_nodes, d_item)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_coil <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Coil:WaterHeating:AirToWaterHeatPump:Pumped
  #' object based on loop data
  #'
  #' @description This function generates an IDF block representing a heat pump
  #' coil using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      name = paste0(l_in$device_name,'--coil'),
      rhcw = 10000,
      rcop = l_in$efficiency,
      rtshr = 0.6956,
      reiadt = '',
      reiawt = '',
      rciwt = '',
      reafr = 'Autocalculate',
      rcwfr = 'Autocalculate',
      efpiirc = '',
      cppiirc = '',
      cphiirhcarc = '',
      cwpp = 100,
      focphtw = 0.2,
      eainn = 'hp-heater-outdoor-air-node',
      eaonn = 'hp-heating-coil-air-outlet-node',
      cwinn = l_in$condenser_in,
      cwonn = l_in$condenser_out,
      chcw = 100,
      crncrve = '',
      matfcho = 5,
      eattfco = 'WetBulbTemperature',
      hcfotcn = 'ASHP HighT CAPFT',
      hcfoaffcn = '',
      hcfowffcn = '',
      gpnfeee = 'ASHP HighT COPFT',
      hcofoaffcn = '',
      hcofowffcn = '',
      plfccn = 'HPWHPLFFPLR')

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
  i_active <- "Coil:WaterHeating:AirToWaterHeatPump:Pumped"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components(l_nodes, d_item)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_coil_w_auxiliary <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Coil:WaterHeating:AirToWaterHeatPump:Pumped
  #' object based on loop data
  #'
  #' @description This function generates an IDF block representing a heat pump
  #' coil using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      name = l_in$coil_name,
      rhcw = 10000,
      rcop = l_in$efficiency,
      rtshr = 0.6956,
      reiadt = '',
      reiawt = '',
      rciwt = '',
      reafr = 'Autocalculate',
      rcwfr = 'Autocalculate',
      efpiirc = '',
      cppiirc = '',
      cphiirhcarc = '',
      cwpp = 100,
      focphtw = 0.2,
      eainn = 'hp-heater-outdoor-air-node',
      eaonn = 'hp-heating-coil-air-outlet-node',
      cwinn = l_in$coil_in,
      cwonn = l_in$coil_out,
      chcw = 100,
      crncve = '',
      matfcho = 5,
      eattfco = 'WetBulbTemperature',
      hcfotcn = 'ASHP HighT CAPFT',
      hcfoaffcn = '',
      hcfowffcn = '',
      gpnfeee = 'ASHP HighT COPFT',
      hcofoaffcn = '',
      hcofowffcn = '',
      plfccn = 'HPWHPLFFPLR')

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
  i_active <- "Coil:WaterHeating:AirToWaterHeatPump:Pumped"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components_w_auxiliary(l_nodes, d_item)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_tank <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:Mixed object based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$device_name, '--vessel'),
      tnkvl = l_in$cylinder/1000,
      sttsn = paste0('schedule:', l_in$device_name, '-vessel--setpoint-temperature'),
      ddbtd = 20,
      mxmtl = l_in$heater_temperature,
      htrct = 'Cycle',
      htrmc = 'Autosize',
      minpw = 0,
      minfr = 0,
      minid = 0,
      htrft = l_in$fuel,
      htrte = 0.91,
      plfcn = '',
      parpw = 20,
      parft = l_in$fuel,
      partt = 0.72,
      parfc = 0,
      parft = l_in$fuel,
      partk = 0,
      ambti = 'Schedule',
      amtsn = 'schedule:water-heater-ambient-temperature',
      amtzn = '',
      atoam = '',
      losst = 5.42654967030591,
      lossf = 1,
      losss = 5.42654967030591,
      lossg = 1,
      pkufr = '',
      ufrfsn = '',
      cwstsn = '',
      usinn = l_in$link_in,
      usonn = l_in$link_out,
      ussde = 1,
      ssinn = l_in$condenser_out,
      ssonn = l_in$condenser_in,
      srcse = 1,
      usdfr = 'Autosize',
      ssdfr = 'Autosize',
      iwhrt = 1.5)

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
  i_active <- "WaterHeater:Mixed"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Obtain system temperatures
  hp_heater_t <-
    ifelse(the$GLOBAL_PAR$heating_hp_variation == 'high-temp', 82.22, 62.22)

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components(l_nodes, d_item) %>%
    dplyr::mutate(heater_temperature = hp_heater_t)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_tank_w_auxiliary <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:Mixed object based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$tank_name, '--tank'),
      tnkvl = l_in$cylinder/1000,
      sttsn = paste0('schedule:', l_in$tank_name, '-vessel--setpoint-temperature'),
      ddbtd = 20,
      mxmtl = l_in$heater_temperature,
      htrct = 'Cycle',
      htrmc = 0,  #'Autosize'
      minpw = 0,
      minfr = '',
      minid = '',
      htrft = l_in$fuel,
      htrte = 0.91,
      plfcn = '',
      parpw = 20,
      parft = l_in$fuel,
      partt = 0.72,
      parfc = 0,
      parft = l_in$fuel,
      partk = 0,
      ambti = 'Schedule',
      amtsn = 'schedule:water-heater-ambient-temperature',
      amtzn = '',
      atoam = '',
      losst = 5.42654967030591,
      lossf = 1,
      losss = 5.42654967030591,
      lossg = 1,
      pkufr = '',
      ufrfsn = '',
      cwstsn = '',
      usinn = l_in$hx_in,
      usonn = l_in$tank_in,
      ussde = 1,
      ssinn = l_in$coil_out,
      ssonn = l_in$coil_in,
      srcse = 1,
      usdfr = 'Autosize',
      ssdfr = 'Autosize',
      iwhrt = 1.5)

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
  i_active <- "WaterHeater:Mixed"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Obtain system temperatures
  hp_heater_t <-
    ifelse(the$GLOBAL_PAR$heating_hp_variation == 'high-temp', 82.22, 62.22)
  hp_heater_vol <- 300

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components_w_auxiliary(l_nodes, d_item) %>%
    dplyr::mutate(heater_temperature = hp_heater_t, cylinder = hp_heater_vol)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_fan <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for Fan:OnOff object based on loop data
  #'
  #' @description This function generates an IDF block representing a heat pump
  #' fan using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'hp-supply-fan',
      avlsn = 'Always On',
      faneff = 0.7,
      prise = 600.0,
      maxfr = 'Autosize',
      moeff = 0.9,
      mofrc = 1,
      ndein = 'hp-heating-coil-air-outlet-node',
      ndeou = 'hp-water-heater-exhaust-air-node',
      crvfn = 'DefaultFanPowerRatioCurve',
      crvsp = 'DefaultFanEffRatioCurve',
      subcat = 'heating')

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
  i_active <- "Fan:OnOff"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_dx <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for a HeatExchanger:FluidToFluid object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a heat-pump
  #' exchanger using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      name = l_in$device_name,
      avlsn = 'Always On',
      ldsinn = l_in$link_in,
      ldsonn = l_in$link_out,
      ldsdfrm = 'Autosize',
      lssinn = paste0(l_in$source_out,'--',l_in$source_in),
      lssonn = l_in$source_out,
      lssdfrm = 'Autosize',
      htemt = 'CounterFlow',
      heutavw = 'Autosize',
      cntrt = 'HeatingSetpointModulated',
      hesnn = l_in$source_out,
      mtdtahed = '2.0',
      htmeut = 'LoopToLoop',
      colssinn = '',
      coldsinn = '',
      cocctm = 'Loop')

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
  i_active <- "HeatExchanger:FluidToFluid"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(grepl('dx$',type)) %>% dplyr::select(-has_adpipe, -has_pump)
  d_heater_s <- d_heater %>% dplyr::filter(grepl('s$',group))
  d_heater_m <- d_heater %>% dplyr::filter(grepl('d$',group)) %>%
    dplyr::mutate(source_in = d_heater_s$link_in,
           source_out = d_heater_s$link_out)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_heater_sizing <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for WaterHeater:Sizing objects based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$tank_name, '--tank'),
      desmd = 'PerFloorArea',
      storg = 0.6,
      tankr = 0.6,
      nomvl = 0.185,
      nobed = 4.0,
      nobat = 2.0,
      strca = 0.2,
      recov = 0.2,
      catfa = 0.02,
      recfa = 0.02,
      nunit = 4.0,
      stoun = 0.2,
      recun = 0.2,
      colun = 0.2,
      ratio = 1.0)

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
  i_active <- "WaterHeater:Sizing"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- define_heat_pump_components_w_auxiliary(l_nodes, d_item)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ground_temperature_ka <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a
  #' Site:GroundTemperature:Undisturbed:KusudaAchenbach object
  #'
  #' @description This function generates an IDF block representing a ground
  #' temperature KusudaAchenbach using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'ka-temperature-profile',
      soilc = 1.81,
      soild = 920,
      soilh = 2200,
      soilt = 13.38,
      amtmp = 3.20,
      shift = 8)

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
  i_active <- 'Site:GroundTemperature:Undisturbed:KusudaAchenbach'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ground_heat_exchanger_profile <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a
  #' GroundHeatExchanger:Vertical:Properties object
  #'
  #' @description This function generates an IDF block representing a ground
  #' exchanger vertical profile using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'ground-heat-exchanger-vertical-profile',
      boreh = 1.0,
      borel = 100.0,
      bored = 0.114,
      gndca = 0.7443,
      gndhc = 3900000,
      pipeh = 0.3895,
      pipec = 1770000,
      piped = 0.03341,
      pipet = 0.002984,
      utube = 0.04913)

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
  i_active <- 'GroundHeatExchanger:Vertical:Properties'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ground_heat_exchanger_response <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a
  #' GroundHeatExchanger:ResponseFactors object
  #'
  #' @description This function generates an IDF block representing a ground
  #' exchanger response factor object using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]
    l_comment <- l_comment[1:4]

    # Define numeric profile
    k_factors <- c(
      -15.795833, -0.868005, -15.646939, -0.841532, -15.498044,
      -0.813862, -15.349150, -0.784824, -15.200255, -0.754311,
      -15.051361, -0.722260, -14.902466, -0.688635, -14.753571,
      -0.653424, -14.604677, -0.616633, -14.455782, -0.578288,
      -14.306888, -0.538426, -14.157993, -0.497093, -14.009099,
      -0.454339, -13.860204, -0.410206, -13.711310, -0.364726,
      -13.562415, -0.317904, -13.413520, -0.269714, -13.264626,
      -0.220096, -13.115731, -0.168953, -12.966837, -0.116163,
      -12.817942, -0.061585, -12.669048, -0.005079, -12.520153,
      0.053480, -12.371259, 0.114187, -12.222364, 0.177098,
      -12.073469, 0.242221, -11.924575, 0.309516, -11.775680,
      0.378895, -11.626786, 0.450230, -11.477891, 0.523360,
      -11.328997, 0.598093, -11.180102, 0.674224, -11.031208,
      0.751534, -10.882313, 0.829805, -10.733418, 0.908825,
      -10.584524, 0.988392, -10.435629, 1.068324, -10.286735,
      1.148459, -10.137840, 1.228656, -9.988946, 1.308799,
      -9.840051, 1.388797, -9.691156, 1.468579, -9.542262,
      1.548092, -9.393367, 1.627303, -9.244473, 1.706193,
      -9.095578, 1.784752, -8.946684, 1.862983, -8.797789,
      1.940892, -8.648895, 2.018493, -8.500000, 2.157703,
      -7.800000, 2.501703, -7.200000, 2.798169, -6.500000,
      3.154434, -5.900000, 3.513287, -5.200000, 4.132497,
      -4.500000, 5.190094, -3.963000, 6.328059, -3.270000,
      8.914195, -2.864000, 10.949425, -2.577000, 12.650707,
      -2.171000, 15.383839, -1.884000, 17.526914, -1.191000,
      23.017708, -0.497000, 28.529688, -0.274000, 30.101558,
      -0.051000, 31.550580, 0.196000, 32.967472, 0.419000,
      34.090581, 0.642000, 35.056826, 0.873000, 35.901876,
      1.112000, 36.616997, 1.335000, 37.151187, 1.679000,
      37.757509, 2.028000, 38.169434, 2.275000, 38.369297,
      3.003000, 38.700624)

    # Assign labels based on vector of values
    k_factors_lbl <- NULL
    k_factors_seq <- seq(1, length(k_factors)/2)
    for(j in k_factors_seq){
      k_ln <- paste('g-Function Ln(T/Ts) Value', j)
      k_g <- paste('g-Function g Value', j)
      k_factors_lbl <- c(k_factors_lbl,k_ln,k_g)
    }
    l_comment <- c(l_comment,k_factors_lbl)

    # Define loop parameter list
    l_values <- list(
      namez = 'ground-heat-exchanger-response-factors',
      verpf = 'ground-heat-exchanger-vertical-profile',
      boren = 16,
      ratio = 0.00083600,
      value = k_factors)

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
  i_active <- 'GroundHeatExchanger:ResponseFactors'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_equation_fit <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for HeatPump:WaterToWater:EquationFit:Heating
  #' object based on loop data
  #'
  #' @description This function generates an IDF block representing a heater
  #' using system info (`d_item`), and an iterator (`l_iter`) defining
  #' loop properties. It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      srcin = l_in$link_in,
      srcou = l_in$link_out,
      loadi = l_in$source_in,
      loado = l_in$source_out,
      reffr = 0.002210,
      refsf = 0.002210,
      refhc = 'autosize',
      refpc = 'autosize',
      hetca = 'WWHPHeatCapCurve',
      hetcb = 'WWHPHeatPowCurve',
      rfcop = 3.6,
      sizen = 1.0)

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
  i_active <- "HeatPump:WaterToWater:EquationFit:Heating"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- define_ground_heat_pump_heater_components(l_nodes, d_item)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_ground_exchanger <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for GroundHeatExchanger:System object based
  #' on loop data
  #'
  #' @description This function generates an IDF block representing a ground
  #' temperature exchanger (type system) using system info (`d_item`), and
  #' an iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$device_name,
      srcin = l_in$link_in,
      srcou = l_in$link_out,
      dflow = 0.002210,
      gndty = 'Site:GroundTemperature:Undisturbed:KusudaAchenbach',
      gndtp = 'ka-temperature-profile',
      gndca = 0.6926,
      gndhc = 2347000,
      gheve = 'ground-heat-exchanger-response-factors')

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
  i_active <- "GroundHeatExchanger:System"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'dx') %>%
    dplyr::select(-has_adpipe, -has_pump)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_heat_pump_ground_exchanger_slinky <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for GroundHeatExchanger:Slinky object based
  #' on loop data
  #'
  #' @description This function generates an IDF block representing a ground
  #' temperature exchanger (type slinky) using system info (`d_item`), and
  #' an iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in$device_name),
      srcin = l_in$link_in,
      srcou = l_in$link_out,
      dflow = 0.0033,
      soilc = 1.2,
      soild = 3200,
      soilh = 850,
      pipec = 1.8,
      pipep = 920,
      pipeh = 2200,
      piped = 0.02667,
      pipet = 0.002413,
      confh = 'Vertical',
      coild = 1,
      coilp = 0.2,
      trncd = 2.5,
      trncl = 40,
      trncn = 35,
      horsp = 2,
      gndty = 'Site:GroundTemperature:Undisturbed:KusudaAchenbach',
      gndtp = 'ka-temperature-profile',
      simyr = 10)

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
  i_active <- "GroundHeatExchanger:Slinky"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_heater_m <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'dx') %>%
    dplyr::select(-has_adpipe, -has_pump)

  # Split list of branches
  l_branches <- split(d_heater_m, seq(1, nrow(d_heater_m)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_condenser_loop <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for CondenserLoop objects based on loop data
  #'
  #' @description This function generates an IDF block representing a condenser
  #' loop using loop data (`d_loop`), system info (`d_item`), and an iterator
  #' (`l_iter`) defining loop properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = 'chilled-water-condenser-loop',
      fluid = 'Water',
      userf = '',
      schem = 'tower-loop-operation',
      loopt = l_in$side_out,
      lomax = 50,
      lomin = 5,
      flwmx = 0.002210,
      flwmi = 0,
      volco = 'autocalculate',
      conin = l_in$side_in,
      conou = l_in$side_out,
      conbr = l_in$condenser_branch,
      conli = l_in$condenser_links,
      demin = l_in$demand_in,
      demou = l_in$demand_out,
      dembr = l_in$demand_branch,
      demli = l_in$demand_links,
      loadd = 'SequentialLoad',
      press = 'None')

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
  i_active <- "CondenserLoop"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(loc %in% c('in','out')) %>%
    dplyr::filter(grepl('^main', .id)) %>%
    dplyr::select(-.id, -has_adpipe, -has_pump, -type, -device_name, -device_type)

  # Split supply-demand, to pick inlet/outlets
  d_heater_s <- d_branches %>% dplyr::filter(grepl('fls$',group))
  d_heater_d <- d_branches %>% dplyr::filter(grepl('fld$',group))
  d_heater_s_in <- d_heater_s %>% dplyr::filter(grepl('in$',loc))
  d_heater_s_out <- d_heater_s %>% dplyr::filter(grepl('out$',loc))
  d_heater_d_in <- d_heater_d %>% dplyr::filter(grepl('in$',loc))
  d_heater_d_out <- d_heater_d %>% dplyr::filter(grepl('out$',loc))

  # Define links
  d_condenser <- d_heater_s_in %>%
    dplyr::mutate(side_in = link_in,
           side_out = d_heater_s_out$link_out,
           demand_in = d_heater_d_in$link_in,
           demand_out = d_heater_d_out$link_out,
           condenser_branch = paste0(group, '--branches'),
           condenser_links = paste0(group, '--connector-list'),
           demand_branch = paste0(d_heater_d_in$group, '--branches'),
           demand_links = paste0(d_heater_d_out$group, '--connector-list'))

  # Split list of branches
  l_branches <- split(d_condenser, seq(1, nrow(d_condenser)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_condenser_equipment <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for a CondenserEquipmentList objects based on loop data
  #'
  #' @description This function generates an IDF block representing a condenser
  #' equipment list using loop data (`d_loop`), system info (`d_item`), and
  #' an iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = 'water-loop-heating-equipment-list',
      loopt = l_in$device_type,
      demli = l_in$device_name)

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
  i_active <- "CondenserEquipmentList"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(type == 'dx') %>%
    dplyr::filter(grepl('^main', .id)) %>%
    dplyr::select(-.id, -has_adpipe, -has_pump, -type)

  # Split list of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_equipment_uncontrolled <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a
  #' PlantEquipmentOperation:Uncontrolled object
  #'
  #' @description This function generates an IDF block representing a plant
  #' equipment operation object using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'water-loop-uncontrolled-operation-scheme',
      verpf = 'water-loop-heating-equipment-list')

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
  i_active <- 'PlantEquipmentOperation:Uncontrolled'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_condenser_schemes <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a
  #' CondenserEquipmentOperationSchemes object
  #'
  #' @description This function generates an IDF block representing a condenser
  #' operation scheme using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'tower-loop-operation',
      ctrlt = 'PlantEquipmentOperation:Uncontrolled',
      ctrln = 'water-loop-uncontrolled-operation-scheme',
      ctrls = 'Always On')

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
  i_active <- 'CondenserEquipmentOperationSchemes'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_sizing_plant <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a
  #' Sizing:Plant object
  #'
  #' @description This function generates an IDF block representing a sizing
  #' plant using default properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'chilled-water-condenser-loop',
      ltype = 'Condenser',
      extmp = 29.00,
      delta = 5.00)

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
  i_active <- 'Sizing:Plant'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_sp_follow_ground <- function(l_iter, d_loop, d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for SetpointManager:FollowGroundTemperature
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a setpoint
  #' manager (ground) using loop data (`d_loop`), system info (`d_item`),
  #' and an iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = 'condenser-control',
      ctrlv = 'Temperature',
      refgn = 'Site:GroundTemperature:Deep',
      offst = 0.0,
      maxsp = 50.0,
      minsp = 10.0,
      nodes = l_in$link_out)

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
  i_active <- "SetpointManager:FollowGroundTemperature"

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches
  d_branches <- plyr::ldply(l_nodes, data.frame) %>% tibble::tibble() %>%
    dplyr::filter(loc %in% c('in','out')) %>%
    dplyr::filter(grepl('^main', .id)) %>%
    dplyr::select(-.id, -has_adpipe, -has_pump, -type, -device_name, -device_type)

  # Split supply-demand, to pick inlet/outlets
  d_heater_s <- d_branches %>% dplyr::filter(grepl('fls$',group))
  d_heater_d <- d_branches %>% dplyr::filter(grepl('fld$',group))
  d_heater_s_in <- d_heater_s %>% dplyr::filter(grepl('in$',loc))
  d_heater_s_out <- d_heater_s %>% dplyr::filter(grepl('out$',loc))

  # Split list of branches
  l_branches <- split(d_heater_s_out, seq(1, nrow(d_heater_s_out)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_system <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a Sizing:System object
  #'
  #' @description This function generates an IDF block representing a sizing
  #' system using default properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      arlpn = 'furnace-loop',
      toltso = 'Sensible',
      doafrm = 'Autosize',
      chmsafr = 0.5,
      prdtc = 4.5,
      pdhrk = 0.008,
      lntec = 12.81,
      lnyor = 0.008,
      ccdsatc = 12.85,
      chdsatc = 40.25,
      tozstu = 'NonCoincident',
      oaic = 'No',
      oaih = 'No',
      ccdsahrk = 0.0085,
      chdsahrk = 0.008,
      csafrm = 'DesignDay',
      csafrm = 0,
      csafrpfam = 0.0099676501,
      cfoacsafr = 1,
      csafrpuccm = 3.9475456e-05,
      hsafrm = 'DesignDay',
      hsafrm = 0,
      hsafrpfam = 0.0099676501,
      hfoahsafr = 1,
      hfoacsafr = 1,
      hsafrpuhcm = 3.1588213e-05,
      syoam = 'ZoneSum',
      zmoafd = 0.51,
      cldcm = 'CoolingDesignCapacity',
      cldcw = 'Autosize',
      cdcpfaw = '',
      foacdc = '',
      htdcm = 'HeatingDesignCapacity',
      htdcw = 'Autosize',
      hdcpfaw = '',
      foahdc = '',
      ccccm = 'OnOff')

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
  i_active <- 'Sizing:System'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_duct <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for AirTerminal:SingleDuct:ConstantVolume:NoReheat
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' equipment connection using loop data (`d_loop`), system info (`d_item`),
  #' zone data (`d_zone`) and an iterator (`l_iter`) defining loop properties.
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      namez = paste0(k_zone, '--air-duct-uncontrolled'),
      sched = 'Always On',
      arins = paste0('node-in-', k_zone),
      arout = paste0('node-mid-', k_zone),
      maxaf = 'AutoSize',
      outda = '',
      perpp = '')

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
  i_active <- 'AirTerminal:SingleDuct:ConstantVolume:NoReheat'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_distribution_unit <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:AirDistributionUnit
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' equipment distribution using loop data (`d_loop`), system info (`d_item`),
  #' zone data (`d_zone`) and an iterator (`l_iter`) defining loop properties.
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      namez = l_in$device_name,
      arout = paste0('node-mid-', k_zone),
      artyp = 'AirTerminal:SingleDuct:ConstantVolume:NoReheat',
      arlbl = paste0(k_zone, '--air-duct-uncontrolled'),
      nomup = '',
      dwnst = '',
      desar = '')

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
  i_active <- 'ZoneHVAC:AirDistributionUnit'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_fan <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a Fan:ConstantVolume
  #' object
  #'
  #' @description This function generates an IDF block representing a fan
  #' component using default properties. It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'fan-constant-volume',
      sched = 'Always On',
      fneff = 0.7,
      press = 30,
      maxfr = 'AutoSize',
      mteff = 0.9,
      mtair = 1,
      inlet = 'node-mid-fancoil',
      oulet = 'node-out-supply')

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
  i_active <- 'Fan:ConstantVolume'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_coil <- function(d_item, l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a Coil:Heating:Fuel
  #' object
  #'
  #' @description This function generates an IDF block representing a coil
  #' component using system info (`d_item`) and default properties. It
  #' also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #' @param d_item: Data frame containing system loop information.
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, i_ful, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'coil-heating',
      sched = 'Always On',
      fuelt = i_ful,
      breff = 0.82,
      nomcp = 'AutoSize',
      inlet = 'node-in-coil',
      inlet = 'node-mid-fancoil',
      oulet = 'node-mid-fancoil',
      onpar = 0,
      pload = '',
      ofpar = 0)

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
  i_active <- 'Coil:Heating:Fuel'
  i_fuel <- d_item$main_fuel

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active, i_fuel) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_controller <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a Controller:OutdoorAir
  #' object
  #'
  #' @description This function generates an IDF block representing an outdoor
  #' air controller component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'controller-outdoor-air',
      raonn = 'node-relief',
      rtann = 'node-in-supply',
      mxann = 'node-in-coil',
      actnn = 'node-outboard-oa',
      moafrm = 0,
      mrrwes = 'Autosize',
      ecnct = 'NoEconomizer',
      eccat = 'ModulateFlow',
      emldtc = 28,
      emlej = 64000,
      rmttec = '',
      eelcn = '',
      rmtbec = -100,
      lcktt = 'NoLockout',
      mnmlt = 'FixedMinimum',
      moasn = '',
      mfooasn = '',
      mnfrree = '',
      mcvcn = 'controller-mechanical-ventilation',
      todecsn = '',
      hghhc = 'No',
      hmczn = '',
      hhoafr = '',
      chihboohr = 'Yes',
      hrbct = 'BypassWhenWithinEconomizerLimits')

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
  i_active <- 'Controller:OutdoorAir'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_ventilation_controller <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for Controller:MechanicalVentilation
  #' objects based on loop data
  #'
  #' @description This function generates an IDF block representing a
  #' ventilation controller using loop data (`d_loop`), system info (`d_item`),
  #' zone data (`d_zone`) and an iterator (`l_iter`) defining loop properties.
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_zon, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Parse zone
    k_cext <- k_spec <- NULL
    for(j in 1:length(l_zon)){
      k_spec <- c(k_spec, l_zon[j], '',
                  paste0(l_zon[j],'--design-spec-zone-air-dist'))
      k_cext <- c(k_cext, paste('Zone', j, 'Name'),
                  paste('Design Specification Outdoor Air Object Name', j),
                  paste('Design Specification Zone Air Distribution Object Name', j))
    }

    # Merge comments: base + extensible
    l_base <- l_comment[1:5]
    l_comment <- c(l_base, k_cext)

    # Define loop parameter list
    l_values <- list(
      namez = 'controller-mechanical-ventilation',
      sched = 'Always On',
      dmndv = 'No',
      metho = 'ZoneSum',
      arfrc = '',
      k_spec)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values) %>% unlist()

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Active object for plant loop
  i_active <- 'Controller:MechanicalVentilation'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Obtain controlled zones
  l_zones <- d_branches$zone

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active, l_zones) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_controller_list <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' AirLoopHVAC:ControllerList object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' controller component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'air-loop-outdoor-air-system-controller-list',
      raonn = 'Controller:OutdoorAir',
      rtann = 'controller-outdoor-air')

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
  i_active <- 'AirLoopHVAC:ControllerList'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' AirLoopHVAC object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' controller component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'furnace-loop',
      ctrls = '',
      avail = 'furnace-availability-manager-list',
      flwrt = 'AutoSize',
      brnhl = 'furnace-supply-branches',
      connl = '',
      supply = 'node-in-supply',
      demand = 'node-out-demand',
      demin = 'furnace-demand-inlet-nodes',
      supmou = 'furnace-supply-outlet-nodes')

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
  i_active <- 'AirLoopHVAC'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_equipment <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' AirLoopHVAC:OutdoorAirSystem:EquipmentList object
  #'
  #' @description This function generates an IDF block representing an
  #' air-loop controller component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'air-loop-outdoor-air-system-equipment-list',
      ctrls = 'OutdoorAir:Mixer',
      avail = 'air-loop-outdoor-air-system-mixer')

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
  i_active <- 'AirLoopHVAC:OutdoorAirSystem:EquipmentList'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_outdoor_system <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' AirLoopHVAC:OutdoorAirSystem object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' outdoor air system using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'air-loop-outdoor-air-system',
      ctrls = 'air-loop-outdoor-air-system-controller-list',
      avail = 'air-loop-outdoor-air-system-equipment-list')

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
  i_active <- 'AirLoopHVAC:OutdoorAirSystem'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_outdoor_mixer <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' OutdoorAir:Mixer object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' mixer component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'air-loop-outdoor-air-system-mixer',
      mixed = 'node-in-coil',
      outst = 'node-outboard-oa',
      relif = 'node-relief',
      retrn = 'node-in-supply')

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
  i_active <- 'OutdoorAir:Mixer'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_outdoor_supply_path <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' AirLoopHVAC:SupplyPath object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' supply path component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'furnace-supply-path',
      supply = 'node-in-demand',
      obtyp = 'AirLoopHVAC:ZoneSplitter',
      oblbl = 'air-loop-zone-splitter')

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
  i_active <- 'AirLoopHVAC:SupplyPath'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_outdoor_return_path <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' AirLoopHVAC:ReturnPath object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' return path component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'furnace-return-path',
      supply = 'node-out-demand',
      obtyp = 'AirLoopHVAC:ZoneMixer',
      oblbl = 'air-loop-zone-mixer')

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
  i_active <- 'AirLoopHVAC:ReturnPath'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_splitter <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for an AirLoopHVAC:ZoneSplitter object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing an
  #' air-loop zone splitter connection using loop data (`d_loop`),
  #' system info (`d_item`), zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_zon, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Parse zone
    k_cext <- k_spec <- NULL
    for(j in 1:length(l_zon)){
      k_spec <- c(k_spec, paste0('node-in-', l_zon[j]))
      k_cext <- c(k_cext, paste('Outlet', j, 'Node Name'))
    }

    # Merge comments: base + extensible
    l_base <- l_comment[1:2]
    l_comment <- c(l_base, k_cext)

    # Define loop parameter list
    l_values <- list(
      namez = 'air-loop-zone-splitter',
      sched = 'node-in-demand',
      k_spec)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values) %>% unlist()

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Active object for plant loop
  i_active <- 'AirLoopHVAC:ZoneSplitter'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Obtain controlled zones
  l_zones <- d_branches$zone

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active, l_zones) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_mixer <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for an AirLoopHVAC:ZoneSplitter object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing an
  #' air-loop zone mixer connection using loop data (`d_loop`),
  #' system info (`d_item`), zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_zon, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Parse zone
    k_cext <- k_spec <- NULL
    for(j in 1:length(l_zon)){
      k_spec <- c(k_spec, paste0('node-out-', l_zon[j]))
      k_cext <- c(k_cext, paste('Outlet', j, 'Node Name'))
    }

    # Merge comments: base + extensible
    l_base <- l_comment[1:2]
    l_comment <- c(l_base, k_cext)

    # Define loop parameter list
    l_values <- list(
      namez = 'air-loop-zone-mixer',
      sched = 'node-out-demand',
      k_spec)

    # Prepend object type to parameter list
    l_block_set <- c(i_act, l_values) %>% unlist()

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }


  # Active object for plant loop
  i_active <- 'AirLoopHVAC:ZoneMixer'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Obtain controlled zones
  l_zones <- d_branches$zone

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active, l_zones) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_branch <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing a Branch object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' branch component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Replicate comments for expanadable objects
    l_base <- l_comment[1:2]
    l_obj1 <- l_comment[3:6]
    l_obj2 <- gsub('1','2',l_obj1)
    l_obj3 <- gsub('1','3',l_obj1)
    l_comment <- c(l_base, l_obj1, l_obj2, l_obj3)

    # Define loop parameter list
    l_values <- list(
      namez = 'furnace-main-branch',
      press = '',
      oatyp = 'AirLoopHVAC:OutdoorAirSystem',
      oalbl = 'air-loop-outdoor-air-system',
      ninla = 'node-in-supply',
      noula = 'node-in-coil',
      ntypb = 'Coil:Heating:Fuel',
      nlblb = 'coil-heating',
      ninlb = 'node-in-coil',
      noutb = 'node-mid-fancoil',
      ntypc = 'Fan:ConstantVolume',
      nlblc = 'fan-constant-volume',
      ninlc = 'node-mid-fancoil',
      noutc = 'node-out-supply')

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
  i_active <- 'Branch'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_branch_list <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' BranchList object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' branch list component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'furnace-supply-branches',
      objnm = 'furnace-main-branch')

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
  i_active <- 'BranchList'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_node_list <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for a NodeList object based on loop data
  #'
  #' @description This function generates an IDF block representing a node list
  #' object using loop data (`d_loop`), system info (`d_item`), zone data
  #' (`d_zone`) and an iterator (`l_iter`) defining loop properties. It also
  #' uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Define loop parameter list
    l_values <- list(
      names = l_in$list,
      loopt = l_in$node)

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
  i_active <- 'NodeList'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Obtain controlled zones
  d_branch_mid <- d_branches %>% dplyr::select(zone) %>%
    dplyr::mutate(list = paste0(zone, '--inlet-air'),
           node = paste0('node-mid-',zone)) %>% dplyr::select(-zone)
  d_branch_out <- d_branches %>% dplyr::select(zone) %>%
    dplyr::mutate(list = paste0(zone, '--return-air'),
           node = paste0('node-out-',zone)) %>% dplyr::select(-zone)
  d_branch_sup <- d_branches %>% dplyr::select(zone) %>% head(1) %>%
    dplyr::mutate(list = 'furnace-supply-outlet-nodes',
           node = 'node-out-supply') %>% dplyr::select(-zone)
  d_branch_dem <- d_branches %>% dplyr::select(zone) %>% head(1) %>%
    dplyr::mutate(list = 'furnace-demand-inlet-nodes',
           node = 'node-in-demand') %>% dplyr::select(-zone)
  d_branches <- d_branch_mid %>% dplyr::bind_rows(d_branch_out) %>%
    dplyr::bind_rows(d_branch_sup) %>% dplyr::bind_rows(d_branch_dem)

  # Split list of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_outdoor_node <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' OutdoorAir:Node object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' outdoor node component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'node-model-outdoor-air')

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
  i_active <- 'OutdoorAir:Node'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_outdoor_node_list <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for an object representing an
  #' OutdoorAir:NodeList object
  #'
  #' @description This function generates an IDF block representing an air-loop
  #' outdoor node list component using default properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = 'node-outboard-oa')

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
  i_active <- 'OutdoorAir:NodeList'

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_schedule_manager <- function(d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for a AvailabilityManager:Scheduled object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a
  #' availability manager scheduled object using default parameters.
  #' It also  uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      names = l_in,
      sched = 'Always On')

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
  i_active <- 'AvailabilityManager:Scheduled'

  # Split list of branches
  l_branches <- c('furnace-availability-manager',
                  'air-loop-system-availability-manager')

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_assignment_manager <- function(d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for a AvailabilityManagerAssignmentList object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing an
  #' availability manager assignment list object using default parameters.
  #' It also  uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      names = paste0(l_in, '-list'),
      sched = 'AvailabilityManager:Scheduled',
      mangr = l_in)

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
  i_active <- 'AvailabilityManagerAssignmentList'

  # Split list of branches
  l_branches <- c('furnace-availability-manager',
                  'air-loop-system-availability-manager')

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_setpoint_mixed_air <- function(d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for a SetpointManager:MixedAir object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a
  #' setpoint mixed-air manager object using default parameters.
  #' It also  uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      names = paste0('node-', l_in, '-os-default-spm'),
      ctrlv = 'Temperature',
      refsp = 'node-out-supply',
      fanin = 'node-mid-fancoil',
      fanou = 'node-out-supply',
      nodsp = paste0('node-', l_in))

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
  i_active <- 'SetpointManager:MixedAir'

  # Split list of branches
  l_branches <- c('in-coil',
                  'mid-fancoil')

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_equipment_con <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:EquipmentConnections objects based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' equipment connection using loop data (`d_loop`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      namez = k_zone,
      names = paste0(k_zone, '--equipment-list'),
      inlet = paste0(k_zone, '--inlet-air'),
      exhst = '',
      airnd = paste0(k_zone, '--node-air'),
      rtrnn = paste0(k_zone, '--return-air'))

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
  i_active <- 'ZoneHVAC:EquipmentConnections'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_warm_air_out_air_loop_setpoint_reheat <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for a SetpointManager:SingleZone:Reheat object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a single
  #' zone setpoint manager object using loop data (`d_loop`) and an
  #' iterator (`l_iter`) defining loop properties. It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      namez = 'setpoint-manager-single-zone-reheat',
      ctrlv = 'Temperature',
      minsu = -99,
      maxsu = 99,
      znlbl = k_zone,
      noden = paste0(k_zone, '--node-air'),
      inlna = paste0('node-mid-', k_zone),
      splbl = 'node-out-supply')

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
  i_active <- 'SetpointManager:SingleZone:Reheat'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  d_branches <- d_branches %>% head(1)
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ideal_loads <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for an ZoneHVAC:IdealLoadsAirSystem object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing an
  #' ideal loads (zonal) system using loop data (`d_loop`), system
  #' info (`d_item`), zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, i_rgm, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = paste0(l_in, '--emitter'),
      avlsn = paste0('schedule:', 'heating-availability-{',i_rgm,'}'),
      zsann = paste0(l_in, '-ideal-loads--supply-inlet'),
      zeann = '',
      siann = '',
      mhsatc = 50,
      mcsatc = 13,
      mhsahrk = 0.0156,
      mcsahrk = 0.0077,
      htngl = 'NoLimit',
      mhafrm = '',
      mshcm = '',
      clngl = 'NoLimit',
      mcafrm = '',
      mtccm = '',
      htasn = '',
      clasn = '',
      dhmct = 'ConstantSensibleHeatRatio',
      clshr = 0.7,
      hmdct = 'None',
      dsoaon = paste0(l_in, '--sz-dsoa'),
      oainn = paste0(l_in, '-ideal-loads--outdoor-air-inlet'),
      dmcvt = 'None',
      otaet = 'NoEconomizer',
      htrct = 'None',
      snhre = 0.70,
      lthre = 0.65)

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
  i_active <- 'ZoneHVAC:IdealLoadsAirSystem'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Obtain controlled zones
  l_zones <- d_branches$zone

  # Obtain heating regime
  i_regime <- d_item$main_emitters

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_zones, make_idf_block, i_active, i_regime) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ideal_loads_out_air <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for an DesignSpecification:OutdoorAir object
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a design
  #' specification outdoor air object using loop data (`d_loop`), system
  #' info (`d_item`), zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = paste0(l_in, '--sz-dsoa'),
      avlsn = 'flow/area',
      mhsatc = 0.00944,
      mcsatc = 0.00124,
      mhsahrk = 0.0)

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
  i_active <- 'DesignSpecification:OutdoorAir'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Obtain controlled zones
  l_zones <- d_branches$zone

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_zones, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ideal_equipment_con <- function(l_iter, d_loop, d_item, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for ZoneHVAC:EquipmentConnections objects
  #' based on loop data
  #'
  #' @description This function generates an IDF block representing a zone
  #' equipment connection using loop data (`d_loop`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_loop: Data frame containing detailed loop information.
  #' @param d_item: Data frame containing system loop information.
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Convert list to list/vector
    l_in <- l_in %>% as.vector()

    # Parse zone
    k_zone <- gsub('--emitter$', '', l_in$device_name)

    # Define loop parameter list
    l_values <- list(
      namez = k_zone,
      names = paste0(k_zone, '--equipment-list'),
      inlet = paste0(k_zone, '-ideal-loads--supply-inlet'),
      exhst = '',
      airnd = paste0(k_zone, '--node-air'),
      rtrnn = paste0(k_zone, '--return-air'))

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
  i_active <- 'ZoneHVAC:EquipmentConnections'

  # Extract nodes from loop data using helper function
  l_nodes <- lapply(names(l_iter), assign_devices, d_loop, d_item)
  names(l_nodes) <- names(l_iter)

  # Merge lists of branches (parse exceptions: electric emitters, warm-air)
  d_branches <- define_emitters(d_item, d_zone, l_nodes)

  # Separate lists of branches
  l_branches <- split(d_branches, seq(1, nrow(d_branches)))

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_branches, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_energy_block_ideal_zone_sizing <- function(l_iter, d_zone, l_sup = l_idf_blocks){
  #' @title Create IDF block for Sizing Zone based on loop data
  #'
  #' @description This function generates an IDF block representing a sizing
  #' zone object  using zone data (`d_zone`) and an iterator (`l_iter`)
  #' defining loop properties. It also uses supplementary data (`l_sup`)
  #' for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param d_zone: Data frame containing detailed zone information.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      zonen = l_in,
      clgin = 'SupplyAirTemperature',
      clgtm = 14,
      clgdf = '',
      hgtin = 'SupplyAirTemperature',
      htgtm = 50,
      htgdf = '',
      clghr = 0.0077,
      htghr = 0.0156,
      outar = paste0(l_in, '--sz-dsoa'),
      htgsz = '',
      clgsz = '',
      clgam = 'DesignDay',
      clgar = 0,
      clgtf = '',
      clgmi = '',
      clgmf = 0,
      htgam = 'DesignDay',
      htgar = 0,
      htgtf = '',
      htgmi = '',
      htgmf = 0,
      dspec = '',
      acair = '',
      dectr = '',
      delow = '',
      dehgh = '',
      zlsmt = 'Sensible Load Only No Latent Load',
      zlcds = 'HumidityRatioDifference',
      zddsa = '',
      zcdsa = 0.005,
      zlhds = 'HumidityRatioDifference',
      zhdsa = '',
      zhdsr = 0.005)

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
  i_active <- "Sizing:Zone"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(d_zone$zone, make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}


# workflow ---------------------------------------------------------------------

generate_energy_system <- function(i_code, l_loop, d_zone){
  #' @title Generate an idf block containing all energy system components
  #'
  #' @description This function generates an energy system model based on
  #' input codes, loop data, and zone data.
  #'
  #' @param i_code: An integer representing the energy system code.
  #' @param l_loop: A list containing loop parameters.
  #' @param d_zone: A data frame containing zone data.
  #'
  #' @return: A list containing an idf of the energy system model, and
  #' a summary of parameters
  

  # a) parse inputs ----

  # Decode energy system codes and assign to loop
  l_loop$heating_codes <- decode_energy_system(i_code)

  # Define loop parameters
  l_loops <- define_loop_parameters(l_loop$loop)

  # Define heating fuels based on codes
  l_systems <- define_heating_components(
    l_loop$heating_codes$msh, l_loop$heating_codes$dhw)

  l_systems$cylinder_loss <-
    assign_cylinder_insulation_transfer(l_loop$insulation)

  # Update zone data with heating connections
  d_zones <- append_heating_connections(d_zone, l_loop)

  # Define water loops and generate branch lists
  d_water_loops <- define_heating_loops(l_loop$heating_codes$msh, l_loop) %>%
    lapply(generate_heating_branch_list, d_zones, l_loop) %>%
    plyr::ldply(data.frame) %>% tibble::tibble() %>% dplyr::group_by(group)


  # b) iterate common water loops ----

  # PlantLoop
  idf_plant_loop <-
    make_energy_block_plant_loop(l_loops, d_water_loops)

  # Sizing:Plant
  idf_plant_sizing <-
    make_energy_block_plant_sizing(l_loops)

  # Sizing:Zone
  idf_zone_sizing <-
    make_energy_block_zone_sizing(l_loops, d_zones)

  # DesignSpecification:ZoneAirDistribution
  idf_zone_air_dist <-
    make_energy_block_zone_air_dist(l_loops, d_zones)

  # SetpointManager:Scheduled
  idf_setpoint_manager <-
    make_energy_block_setpoint_manager(l_loops, d_water_loops)

  # Branch
  idf_branch <-
    make_energy_block_branch(l_loops, d_water_loops, l_systems)

  # BranchList
  idf_branch_list <-
    make_energy_block_branch_list(l_loops, d_water_loops, l_systems)

  # Connector:Splitter
  idf_branch_splitter <-
    make_energy_block_branch_splitter(l_loops, d_water_loops, l_systems)

  # Connector:Mixer
  idf_branch_mixer <-
    make_energy_block_branch_mixer(l_loops, d_water_loops, l_systems)

  # ConnectorList
  idf_connector_list <-
    make_energy_block_connector_list(l_loops, d_water_loops)

  # Pipe:Adiabatic
  idf_adpipe <-
    make_energy_block_adpipe(l_loops, d_water_loops, l_systems)

  # Pump:VariableSpeed
  idf_block_pump <-
    make_energy_block_pump(l_loops, d_water_loops, l_systems)

  # PlantEquipmentList
  idf_plant_equipment <-
    make_energy_block_plant_equipment(l_loops, d_water_loops, l_systems)

  # PlantEquipmentOperation:HeatingLoad
  idf_plant_operation <-
    make_energy_block_plant_operation(l_loops, d_water_loops, l_systems)

  # PlantEquipmentOperationSchemes
  idf_plant_schemes <-
    make_energy_block_plant_schemes(l_loops, d_water_loops, l_systems)

  # ZoneControl:Thermostat
  idf_zone_thermostat <-
    make_energy_block_zone_thermostat(l_loops, d_zones)

  # ThermostatSetpoint:DualSetpoint
  idf_thermostat_control <-
    make_energy_block_thermostat_control(l_loops, d_zones, l_systems)

  # OutdoorAir:Node
  idf_outdoor_air <-
    make_energy_block_outdoor_air()

  # ZoneHVAC:EquipmentList
  idf_equipment <-
    make_energy_block_equipment(l_loops, d_water_loops, l_systems, d_zones)

  # ZoneHVAC:EquipmentConnections
  idf_equipment_connections <-
    make_energy_block_equipment_con(l_loops, d_water_loops, l_systems, d_zones)


  # c) iterate specific energy-system objects ----

  # .. check main heater device
  if(l_systems$main_heater == 'Boiler:HotWater'){

    # Boiler:HotWater
    idf_boiler <-
      make_energy_block_boiler(l_loops, d_water_loops, l_systems)

    idf_heater <- c(idf_boiler)

  }else if(l_systems$main_heater == 'WaterHeater:Mixed'){

    # WaterHeater:Mixed
    idf_boiler <-
      make_energy_block_water_heater(l_loops, d_water_loops, l_systems, 'msh')

    idf_heater <- c(idf_boiler)

  }else if(l_systems$main_heater == 'DistrictHeating:Water'){

    # DistrictHeating:Water
    idf_boiler <-
      make_energy_block_district_heat_mains(l_loops, d_water_loops, l_systems)

    idf_heater <- c(idf_boiler)

  }else if(l_systems$main_heater == 'Furnace'){

    # Sizing:System
    idf_system <-
      make_energy_block_warm_air_system()

    # AirTerminal:SingleDuct:ConstantVolume:NoReheat
    idf_duct <-
      make_energy_block_warm_air_duct(l_loops, d_water_loops, l_systems, d_zones)

    # ZoneHVAC:AirDistributionUnit
    idf_distribution_unit <-
      make_energy_block_warm_air_distribution_unit(l_loops, d_water_loops, l_systems, d_zones)

    # Fan:ConstantVolume
    idf_fan <-
      make_energy_block_warm_air_fan()

    # Coil:Heating:Fuel
    idf_coil <-
      make_energy_block_warm_air_coil(l_systems)

    # Controller:OutdoorAir
    idf_out_air_controller <-
      make_energy_block_warm_air_out_air_controller()

    # Controller:MechanicalVentilation
    idf_ventilation_controller <-
      make_energy_block_warm_air_ventilation_controller(l_loops, d_water_loops, l_systems, d_zones)

    # AirLoopHVAC:ControllerList
    idf_out_air_controller_list <-
      make_energy_block_warm_air_out_air_controller_list()

    # AirLoopHVAC
    idf_out_air_loop <-
      make_energy_block_warm_air_out_air_loop()

    # AirLoopHVAC:OutdoorAirSystem:EquipmentList
    idf_out_air_loop_equipment <-
      make_energy_block_warm_air_out_air_loop_equipment()

    # AirLoopHVAC:OutdoorAirSystem
    idf_out_air_loop_outdoor_system <-
      make_energy_block_warm_air_out_air_loop_outdoor_system()

    # OutdoorAir:Mixer
    idf_out_air_loop_outdoor_mixer <-
      make_energy_block_warm_air_out_air_loop_outdoor_mixer()

    # AirLoopHVAC:SupplyPath
    idf_out_air_loop_outdoor_supply_path <-
      make_energy_block_warm_air_out_air_loop_outdoor_supply_path()

    # AirLoopHVAC:ReturnPath
    idf_out_air_loop_outdoor_return_path <-
      make_energy_block_warm_air_out_air_loop_outdoor_return_path()

    # AirLoopHVAC:ZoneSplitter
    idf_out_air_loop_splitter <-
      make_energy_block_warm_air_out_air_loop_splitter(l_loops, d_water_loops, l_systems, d_zones)

    # AirLoopHVAC:ZoneMixer
    idf_out_air_loop_mixer <-
      make_energy_block_warm_air_out_air_loop_mixer(l_loops, d_water_loops, l_systems, d_zones)

    # Branch
    idf_out_air_loop_branch <-
      make_energy_block_warm_air_out_air_loop_branch()

    # BranchList
    idf_out_air_loop_branch_list <-
      make_energy_block_warm_air_out_air_loop_branch_list()

    # NodeList
    idf_out_air_loop_node_list <-
      make_energy_block_warm_air_out_air_loop_node_list(l_loops, d_water_loops, l_systems, d_zones)

    # OutdoorAir:Node
    idf_out_air_loop_outdoor_node <-
      make_energy_block_warm_air_out_air_loop_outdoor_node()

    # OutdoorAir:NodeList
    idf_out_air_loop_outdoor_node_list <-
      make_energy_block_warm_air_out_air_loop_outdoor_node_list()

    # AvailabilityManager:Scheduled
    idf_out_air_loop_schedule_manager <-
      make_energy_block_warm_air_out_air_loop_schedule_manager()

    # AvailabilityManagerAssignmentList
    idf_out_air_loop_assignment_manager <-
      make_energy_block_warm_air_out_air_loop_assignment_manager()

    # SetpointManager:MixedAir
    idf_out_air_loop_setpoint_mixed_air <-
      make_energy_block_warm_air_out_air_loop_setpoint_mixed_air()

    # SetpointManager:SingleZone:Reheat
    idf_equipment_setpoint_reheat <-
      make_energy_block_warm_air_out_air_loop_setpoint_reheat(l_loops, d_water_loops, l_systems, d_zones)

    # ZoneHVAC:EquipmentConnections
    idf_equipment_connections <-
      make_energy_block_warm_air_equipment_con(l_loops, d_water_loops, l_systems, d_zones)

    # Combine components
    idf_heater <- c(
      idf_system, idf_duct, idf_distribution_unit, idf_fan, idf_coil,
      idf_out_air_controller, idf_ventilation_controller,
      idf_out_air_controller_list, idf_out_air_loop,
      idf_out_air_loop_equipment, idf_out_air_loop_outdoor_system,
      idf_out_air_loop_outdoor_mixer, idf_out_air_loop_outdoor_supply_path,
      idf_out_air_loop_outdoor_return_path, idf_out_air_loop_splitter,
      idf_out_air_loop_mixer, idf_out_air_loop_branch,
      idf_out_air_loop_branch_list, idf_out_air_loop_node_list,
      idf_out_air_loop_outdoor_node, idf_out_air_loop_outdoor_node_list,
      idf_out_air_loop_schedule_manager, idf_out_air_loop_assignment_manager,
      idf_out_air_loop_setpoint_mixed_air, idf_equipment_setpoint_reheat)

  }else if(l_systems$main_heater == 'HeatPump:WaterToWater:EquationFit:Heating'){

    # Check selected technology
    if(the$GLOBAL_PAR$heating_gshp == 'borehole'){

      # GroundHeatExchanger:System
      idf_gnd_exchanger <-
        make_energy_block_heat_pump_ground_exchanger(l_loops, d_water_loops, l_systems)

    }else{

      # GroundHeatExchanger:Slinky
      idf_gnd_exchanger <-
        make_energy_block_heat_pump_ground_exchanger_slinky(l_loops, d_water_loops, l_systems)

    }

    # Site:GroundTemperature:Undisturbed:KusudaAchenbach
    idf_gnd_temp <-
      make_energy_block_ground_temperature_ka()

    # GroundHeatExchanger:Vertical:Properties
    idf_gnd_profile <-
      make_energy_block_ground_heat_exchanger_profile()

    # GroundHeatExchanger:ResponseFactors
    idf_gnd_response <-
      make_energy_block_ground_heat_exchanger_response()

    # HeatPump:WaterToWater:EquationFit:Heating
    idf_eq_fit <-
      make_energy_block_heat_pump_equation_fit(l_loops, d_water_loops, l_systems)

    # WaterHeater:Mixed
    idf_water_heater <-
      make_energy_block_water_heater(l_loops, d_water_loops, l_systems)

    # CondenserLoop
    idf_condenser_loop <-
      make_energy_block_condenser_loop(l_loops, d_water_loops, l_systems)

    # CondenserEquipmentList
    idf_condenser_equipment <-
      make_energy_block_condenser_equipment(l_loops, d_water_loops, l_systems)

    # PlantEquipmentOperation:Uncontrolled
    idf_equipment_uncontrolled <-
      make_energy_block_equipment_uncontrolled()

    # CondenserEquipmentOperationSchemes
    idf_condenser_schemes <-
      make_energy_block_condenser_schemes()

    # SetpointManager:FollowGroundTemperature
    idf_sp_follow_ground <-
      make_energy_block_sp_follow_ground(l_loops, d_water_loops, l_systems)

    # Sizing:Plant
    idf_sizing_plant <-
      make_energy_block_sizing_plant()

    # GSHP components
    idf_heater <- c(idf_gnd_temp, idf_gnd_profile, idf_gnd_response,
                    idf_eq_fit, idf_gnd_exchanger, idf_water_heater,
                    idf_condenser_loop, idf_equipment_uncontrolled,
                    idf_condenser_schemes, idf_condenser_equipment,
                    idf_sp_follow_ground, idf_sizing_plant)

  }else if(l_systems$main_heater == 'WaterHeater:HeatPump:PumpedCondenser'){

    idf_boiler <- idf_dx <- idf_hp_tank_sizing <- ''

    # Check selected technology
    if(the$GLOBAL_PAR$heating_ashp == 'integrated'){

      # WaterHeater:HeatPump:PumpedCondenser
      idf_heater <-
        make_energy_block_heat_pump(l_loops, d_water_loops, l_systems)

      # WaterHeater:Mixed
      idf_tank <-
        make_energy_block_heat_pump_tank(l_loops, d_water_loops, l_systems)

      # Coil:WaterHeating:AirToWaterHeatPump:Pumped
      idf_coil <-
        make_energy_block_heat_pump_coil(l_loops, d_water_loops, l_systems)

    }else{

      # ASHP: auxiliary cylinder

      # Boiler:HotWater
      idf_boiler <-
        make_energy_block_boiler(l_loops, d_water_loops, l_systems)

      # HeatExchanger:FluidToFluid
      idf_dx <-
        make_energy_block_heat_pump_dx(l_loops, d_water_loops, l_systems)

      # WaterHeater:Sizing
      idf_hp_tank_sizing <-
        make_energy_block_heat_pump_heater_sizing(l_loops, d_water_loops, l_systems)

      # WaterHeater:HeatPump:PumpedCondenser
      idf_heater <-
        make_energy_block_heat_pump_w_auxiliary(l_loops, d_water_loops, l_systems)

      # WaterHeater:Mixed
      idf_tank <-
        make_energy_block_heat_pump_tank_w_auxiliary(l_loops, d_water_loops, l_systems)

      # Coil:WaterHeating:AirToWaterHeatPump:Pumped
      idf_coil <-
        make_energy_block_heat_pump_coil_w_auxiliary(l_loops, d_water_loops, l_systems)
    }

    # Fan:OnOff
    idf_fan <-
      make_energy_block_heat_pump_fan(l_loops, d_water_loops, l_systems)

    # ASHP components
    idf_heater <- c(idf_boiler, idf_fan, idf_dx, idf_hp_tank_sizing,
                    idf_tank, idf_coil, idf_heater)

  }else{

    idf_heater <- ''

  }


  # .. check main heater device
  if(l_systems$main_emitter == 'ZoneHVAC:Baseboard:RadiantConvective:Water'){

    # ZoneHVAC:Baseboard:RadiantConvective:Water:Design
    idf_emitter_design <-
      make_energy_block_wall_radiator_design(l_loops, d_water_loops, l_systems)

    # ZoneHVAC:Baseboard:RadiantConvective:Water
    idf_emitter <-
      make_energy_block_water_radiator(l_loops, d_water_loops, l_systems)

    idf_emitter <- c(idf_emitter_design, idf_emitter)

  }else if(l_systems$main_emitter == 'ZoneHVAC:Baseboard:RadiantConvective:Electric'){

    # ZoneHVAC:Baseboard:RadiantConvective:Electric
    idf_emitter <-
      make_energy_block_electric_radiator(l_loops, d_zones, l_systems)

  }else if(l_systems$main_emitter == 'ZoneHVAC:Baseboard:Convective:Electric'){

    # ZoneHVAC:Baseboard:Convective:Electric
    idf_emitter <-
      make_energy_block_electric_room_radiator(l_loops, d_zones, l_systems)

  }else if(l_systems$main_emitter == 'ZoneHVAC:LowTemperatureRadiant:VariableFlow'){

    # ZoneHVAC:LowTemperatureRadiant:VariableFlow:Design
    idf_emitter_design <-
      make_energy_block_underfloor_radiator_design()

    # ZoneHVAC:LowTemperatureRadiant:VariableFlow
    idf_emitter <-
      make_energy_block_underfloor_radiator(l_loops, d_water_loops, l_systems)

    idf_emitter <- c(idf_emitter_design, idf_emitter)

  }else if(l_systems$main_emitter == 'ZoneHVAC:IdealLoadsAirSystem'){

    # ZoneHVAC:IdealLoadsAirSystem
    idf_ideal_loads <-
      make_energy_block_ideal_loads(l_loops, d_water_loops, l_systems, d_zones)

    # DesignSpecification:OutdoorAir
    idf_ideal_out_air <-
      make_energy_block_ideal_loads_out_air(l_loops, d_water_loops, l_systems, d_zones)

    # ZoneHVAC:EquipmentConnections
    idf_equipment_connections <-
      make_energy_block_ideal_equipment_con(l_loops, d_water_loops, l_systems, d_zones)

    # Sizing:Zone
    idf_zone_sizing <-
      make_energy_block_ideal_zone_sizing(l_loops, d_zones)

    # IdealLoads components
    idf_emitter <- c(idf_ideal_loads, idf_ideal_out_air)
    idf_heater <- ''

  }else{
    idf_emitter <- ''
  }


  # .. check dhw heater device
  if(l_systems$dhw_heater == 'WaterHeater:Mixed'){

    # WaterHeater:Sizing
    idf_water_heater_sizing <-
      make_energy_block_heater_sizing(l_loops, d_water_loops, l_systems)

    if(l_loop$loop == 'zone-loop-single'){

      # WaterHeater:Mixed
      idf_water_mixed <-
        make_energy_block_water_heater(l_loops, d_water_loops, l_systems)

    }else{

      # WaterHeater:Mixed
      idf_water_mixed <-
        make_energy_block_water_re_heater(l_loops, d_water_loops, l_systems)

    }

    idf_water_heater <- c(idf_water_heater_sizing, idf_water_mixed)

  }else{
    idf_water_heater <- ''
  }


  # d) assemble objects ----------------------------------------------------------

  # Merge idf objects
  idf_res <- c(
    idf_plant_loop, idf_plant_sizing, idf_zone_sizing, idf_zone_air_dist,
    idf_setpoint_manager, idf_branch, idf_branch_list, idf_branch_splitter,
    idf_branch_mixer, idf_connector_list, idf_adpipe, idf_block_pump,
    idf_plant_equipment, idf_plant_operation, idf_plant_schemes,
    idf_zone_thermostat, idf_thermostat_control, idf_equipment,
    idf_equipment_connections, idf_outdoor_air, idf_heater,
    idf_emitter, idf_water_heater
  )

  # Merge key lists and data frames
  l_sum <- list(loops = l_loops,
                systems = l_systems,
                zones = d_zones %>%
                  dplyr::select(-ach, -area__m2, -exposed_mm2, -glazed, -vent),
                branches = d_water_loops,
                schedules = extract_schedule_information(idf_res))

  # Return the generated energy system model
  return(list(idf=idf_res, info = l_sum))
}

generate_water_service <- function(l_zone_use, d_branch, n_occ){
  #' Generates water service IDF blocks.
  #'
  #' This function creates IDF blocks for water services based on zone usage and number of occupants.
  #'
  #' @param l_zone_use A list containing zone usage information.
  #' @param n_occ Number of occupants.
  #'
  #' @return A list containing the IDF blocks and water service summary data.

  # Summarize water services.
  d_watersys <- summarise_water_services(n_occ, l_zone_use)

  # Create IDF blocks for water mains, connections, and equipment.
  idf_water_mains <- make_block_water_mains()
  idf_water_connections <- make_block_water_connections(d_watersys, d_branch)
  idf_water_equipment <- make_block_water_equipment(d_watersys)

  # Combine IDF blocks.
  idf_res <- c(idf_water_mains, idf_water_equipment, idf_water_connections)

  # Return IDF blocks and water service data.
  return(list(idf=idf_res, info = d_watersys))
}
