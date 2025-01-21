
obtain_appliances_demand <- function(hid, in_area, in_occ, l_ref, d_id, in_eui="normal"){
  #' @title Calculates yearly appliance demand for a dwelling
  #'
  #' @description This function calculates the yearly electricity demand for lighting, appliances,
  #' pumps, cooking (electric and gas) based on various dwelling characteristics.
  #'
  #' @description It uses BREDEM calculation:
  #'        \deqn{ Lighting = 59.73 * (TFA * N)^{0.4714} }
  #'        \deqn{ Appliances = 184.8 * (TFA * N)^{0.4714} }
  #'
  #' @param hid ID of the dwelling.
  #' @param in_area Floor area of the dwelling.
  #' @param in_occ Occupancy level of the dwelling.
  #' @param l_ref Reference data for dwelling characteristics.
  #' @param d_id Data frame containing housing codes.
  #' @param in_eui Intensity level ('normal', 'high', or 'low'). Defaults to 'normal'.
  #'
  #' @return Data frame with yearly demand for different categories.


  # Access data from reference data frames
  d_Lss <- l_ref$other.heat.loss.elements
  d_Cmp <- l_ref$complementary
  d_Lel <- l_ref$low.energy.lighting
  d_Msh <- l_ref$space.heating
  d_Dhw <- l_ref$hot.water.system

  # Extract dwelling code and translate to EHS code
  hid <- d_id[hid,]
  idEhs <- get_aacode(hid$index)

  # Process intensity level
  in_eui <- switch (in_eui, 'normal' = 1, 'high' = 1.16, 'low' = 0.84)

  # Lighting efficiency
  in_light_eff <- d_Lel %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D112_LowEnergyLightsFraction) %>% unlist() %>% as.numeric()

  # Heating and DHW system types
  in_main_system <- d_Msh %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D082_MainHeatingSystemType) %>% unlist() %>% as.integer()
  in_secondary_system <- d_Msh %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D095_SecondaryHeatingSystem) %>% unlist() %>% as.integer()
  in_dhw <- d_Dhw %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D097_DHWSystemType) %>% unlist() %>% as.integer()

  # Additional dwelling characteristics
  in_main_pump <- d_Msh %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D089_MainHeatingOilPumpLocation) %>% unlist() %>% as.character()
  in_dhw_solar <- d_Dhw %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D109_SolarDHWSystem) %>% unlist() %>% as.logical()

  # Assign custom/survey heating system
  if(!is.null(the$GLOBAL_PAR$heating_system_code)){
    k_heating_code <- the$GLOBAL_PAR$heating_system_code
    l_code <- decode_energy_system(k_heating_code)
    in_main_system <- l_code$msh
    in_dhw <-  l_code$dhw
    if(in_main_system %in% c(14,15)) in_main_pump <- 'Outside dwelling'
  }

  # Window properties
  in_window_type_A <- d_Lss %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D068_Windows1Type) %>% unlist() %>% as.character()
  in_window_type_B <- d_Lss %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D070_Windows2Type) %>% unlist() %>% as.character()
  in_window_frameA <- d_Lss %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D071_Windows1FrameType) %>% unlist() %>% as.character()
  in_window_frameB <- d_Lss %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(D072_Windows2FrameType) %>% unlist() %>% as.character()
  in_window_overshadingA <- d_Lss %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(H041_Windows1Overshading) %>% unlist() %>% as.character()
  in_window_overshadingB <- d_Lss %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(H046_Windows1Overshading) %>% unlist() %>% as.character()

  # Calculate Window Area
  in_window_area <-d_Cmp %>% dplyr::filter(V001_HousingCode==idEhs) %>%
    dplyr::select(V562_SurfaceWallArea, V563_GlazingRatio) %>%
    dplyr::mutate(window_area = V562_SurfaceWallArea * V563_GlazingRatio) %>%
    dplyr::select(window_area) %>% unlist() %>% as.numeric()
  in_window_areaA <- in_window_areaB <- in_window_area / 2

  # Calculate Window Type and Frame
  in_window_type <-
    ifelse(grepl('triple', in_window_type_A) |
             grepl('triple', in_window_type_B), 3,
           ifelse(grepl('double', in_window_type_A) |
                    grepl('double', in_window_type_B), 2, 1))
  in_window_frame_main <-
    ifelse(grepl('upvc', in_window_frameA) |
             grepl('upvc', in_window_frameB), 3,
           ifelse(grepl('metal', in_window_frameA) |
                    grepl('metal', in_window_frameB), 2, 1))
  in_window_shade_main <-
    ifelse(grepl('average', in_window_overshadingA) |
             grepl('average', in_window_overshadingB), 0.5, 0)
  in_window_type_A <- in_window_type_B <- in_window_type
  in_light_access_A <- in_light_access_B <- in_window_shade_main

  # Estimate daylight parameters
  in_light_transmission_A <- switch(in_window_type, 0.9, 0.8, 0.7, 0)
  in_light_transmission_B <- switch(in_window_type, 0.9, 0.8, 0.7, 0)
  in_light_frame_factor_A <- switch(in_window_frame_main, 0.7, 0.8, 0.8, 0.7)
  in_light_frame_factor_B <- switch(in_window_frame_main, 0.7, 0.8, 0.8, 0.7)
  in_light_correction <- 1 - 0.5 * in_light_eff

  # Estimate light demand
  in_light_basic <- 59.73 * (in_area * in_occ)^0.4714 * in_eui

  in_light_daylight_A <-
    (0.9 * in_window_areaA * in_light_transmission_A *
       in_light_frame_factor_A * in_light_access_A) / in_area
  in_light_daylight_B <-
    (0.9 * in_window_areaB * in_light_transmission_B *
       in_light_frame_factor_B * in_light_access_B) / in_area
  in_light_daylight_ <- in_light_daylight_A + in_light_daylight_B
  in_light_correction_factor <-
    ifelse(in_light_daylight_ <= 0.095,
           52.2 * in_light_daylight_^2 - 9.94 * in_light_daylight_ + 1.433,
           0.96)

  in_light_annual <-
    in_light_basic * in_light_correction * in_light_correction_factor
  in_light_monthly <- lapply(1:12, function(x) {
    daysmonth <- lubridate::days_in_month(
      lubridate::ymd(paste("2011-", x, "-01", sep = ""))
    ) %>%
      as.integer()
    res <- in_light_annual * (1 + 0.5 * cos(2 * pi * (x - 0.2)/12)) * daysmonth/365
    return(res)})
  in_light_yearly <- sum(plyr::ldply(in_light_monthly, data.frame))


  # Estimate appliances demand

  appliances_basic <- 184.8 * (in_area * in_occ) ^ 0.4714 * in_eui

  appliances_monthly <- lapply(1:12, function(x) {
  
    daysmonth <- lubridate::days_in_month(
      lubridate::ymd(paste("2011-", x, "-01", sep = ""))
    ) %>%
      as.integer()

    res <- appliances_basic *
      (1 + 0.157 * cos(2 * pi * (x - 1.78)/12)) * daysmonth / 365
      
    return(res)
  })

  appliances_yearly <- sum(plyr::ldply(appliances_monthly, data.frame))

  # Estimate additional components (eg. pumps)
  in_main_pump <- ifelse(in_main_pump=='Outside dwelling', 2,
                  ifelse(in_main_pump=='-', 3, 1))
  in_heat_form <- switch(in_main_system, 1,1,1,1,1,2,2,2,1,2,3,3,1,2,2)

  in_pumps_yearly <- 0  # This is now estimated in HVAC module

  # Assign energy for cooking
  in_cooking_system <-
    ifelse((in_main_system %in% c(6,7,8,10,11,12,14,15)) &
             (in_dhw %in% c(8,9,10,11)), 1, 2)
  in_cooking_elec_yr <-
    ifelse(in_cooking_system == 1, (1.22 + 0.24 * in_occ) * 10^6/3600,
           (0.5 + 0.1 * in_occ) * 10^6/3600 * in_eui)
  in_cooking_gas_yr <-
    ifelse(in_cooking_system == 2, (0.87 + 0.18 * in_occ) * 10^6/3600 * in_eui, 0)

  # Summarise yearly demand
  d_demand_yearly <-
    data.frame(variable=c("lighting", "appliances", "pumps_fans",
                          "cooking_electric", "cooking_gas"),
               yearly=c(in_light_yearly, appliances_yearly, in_pumps_yearly,
                        in_cooking_elec_yr, in_cooking_gas_yr))

  d_demand_yearly <- d_demand_yearly %>%
    dplyr::mutate(yearly = ifelse(is.na(yearly), 0, yearly))

  return(d_demand_yearly)
}

assign_loads <- function(d_app, d_sch, l_sup = l_idf_blocks){
  #' @title Assigns loads to IDF objects based on schedule data
  #'
  #' @description This function assigns loads from an appliance data frame to IDF objects based
  #' on occupancy schedules and fuel types.
  #'
  #' @param d_app Data frame containing appliance data.
  #' @param d_sch Data frame containing occupancy schedules.
  #' @param l_sup List of IDF block objects (default: l_idf_blocks).
  #'
  #' @return List of modified IDF block objects.


  # Helpers to assign internal loads
  assign_people <- function(varTo, tblRef, lblTo){
    # @title Assigns people to IDF blocks
    #
    # @description This function creates IDF blocks for people based on given parameters.
    #
    # @param varTo Target variable for assignment.
    # @param tblRef Reference table for variable mapping.
    # @param lblTo Label mapping table.
    #
    # @return A character vector containing the generated IDF blocks.

    # Extract information
    ip='People'
    k_zone <- gsub("^[[:alpha:]]*_[0-9]*__","", varTo)
    k_lbl <- grep(varTo, tblRef, value = TRUE)

    # Generate IDF block parameters
    l_par_outputs <- list(
      name = paste0('p__',varTo),
      zone = k_zone,
      n_ppl_sch = varTo,
      n_ppl_meth = 'People',
      n_ppl = 1,
      ppl_area = '',
      zone_ppl = '',
      frac_rad = 0.45,
      sens_frac = 'autocalculate',
      act_sch = 'schedule:base-activity',
      co2_rate = 0.0000000381,
      comfort_warn = 'No',
      rad_calc = 'EnclosureAveraged',
      srf_fact = '',
      wrk_sch = 'schedule:work-eff',
      clo_meth = 'DynamicClothingModelASHRAE55',
      clo_sch = '',
      clo_lbl = 'schedule:clothing',
      air_lbl = 'schedule:air-velo',
      comf_xtype = 'FANGER')

    # Combine and process data
    l_com <- lblTo[[ip]]
    l_ins <- c(ip, l_par_outputs)
    l_obj <- iterate_on_list(l_ins, l_com)
    txt_res <- c(unlist(l_obj))

    return(txt_res)
  }

  # Helper to assign lighting and appliances
  assign_lights <- function(varTo, tblTo, ubir, lblTo){
    # @title Assigns lights to IDF blocks
    #
    # @description This function creates IDF blocks for lights based on given parameters.
    #
    # @param varTo Target variable for assignment.
    # @param tblTo Table containing light data.
    # @param ubir Ubiquity ratio for people.
    # @param lblTo Label mapping table.
    #
    # @return A character vector containing the generated IDF blocks.

    # Extract information
    ip='Lights'
    varZn <- tblTo %>% dplyr::filter(id==varTo) %>% dplyr::select(zone) %>%
      unlist() %>% as.character()
    varPw <- as.numeric(
      subset(tblTo, id==varTo,
             select = c(power__W, power__W__m2, power__W__ppl)))
    varPw <- varPw / c(1,1,ubir)
    varGr <- tblTo %>% dplyr::filter(id==varTo) %>% dplyr::select(type) %>%
      unlist() %>% as.character()
    varSp <- tblTo %>% dplyr::filter(id==varTo) %>% dplyr::select(supervised) %>%
      unlist() %>% as.logical()
    varSp <- ifelse(varSp==TRUE, 'Watts/Person', 'LightingLevel')
    varSh <- gsub('^x+','x', paste0('x',varTo))

    # Generate IDF block parameters
    l_par_outputs <- list(
      name = paste0('l__',varTo),
      zone = varZn,
      sch = varSh,
      des_meth = varSp,
      lit_lev = round(varPw[1],3),
      watt_area = round(varPw[2],3),
      watt_ppl = round(varPw[3],3),
      air_frac = 0,
      frac_rad = 0.42,
      frac_vis = 0.18,
      frac_rep = 1,
      end_use = varGr,
      plenum = 'No')

    # Combine and process data
    l_com <- lblTo[[ip]]
    l_ins <- c(ip, l_par_outputs)
    l_obj <- iterate_on_list(l_ins, l_com)
    txt_res <- c(unlist(l_obj))

    return(txt_res)
  }

  # Helper to assign appliances
  assign_apps <- function(varTo, tblTo, ubir, lblTo){
    # @title Assigns appliances to IDF blocks
    #
    # @description This function creates IDF blocks for appliances based on given parameters.
    #
    # @param varTo Target variable for assignment.
    # @param tblTo Table containing appliance data.
    # @param ubir Ubiquity ratio for people.
    # @param lblTo Label mapping table.
    #
    # @return A character vector containing the generated IDF blocks.

    # Extract information
    varZn <- tblTo %>% dplyr::filter(id==varTo) %>% dplyr::select(zone) %>%
      unlist() %>% as.character()
    varPw <- as.numeric(
      subset(tblTo, id==varTo,
             select=c(power__W, power__W__m2, power__W__ppl)))
    varPw <- varPw / c(1,1,ubir)
    varGr <- tblTo %>% dplyr::filter(id==varTo) %>% dplyr::select(type) %>%
      unlist() %>% as.character()
    varSp <- as.logical(subset(tblTo, id==varTo, select = supervised))
    varSp <- ifelse(varSp==TRUE, 'Watts/Person', 'EquipmentLevel')
    varSh <- gsub('^x+','x', paste0('x',varTo))

    varFl <- tblTo %>% dplyr::filter(id==varTo) %>% dplyr::select(mainf) %>%
      unlist() %>% as.character()
    ip <- varFe <- switch (varFl,
                           'ele' = 'ElectricEquipment', 'gas' = 'GasEquipment',
                           'cal' = 'OtherEquipment', 'oil' = 'OtherEquipment',
                           'bio' = 'OtherEquipment', 'dst' = 'OtherEquipment')

    # Generate IDF block parameters
    l_par_outputs <- list(
      name = paste0('e__',varTo),
      zone = varZn,
      sch = varSh,
      des_meth = varSp,
      lit_lev = round(varPw[1],3),
      watt_area = round(varPw[2],3),
      watt_ppl = round(varPw[3],3),
      air_frac = 0.0005,
      frac_rad = 0.51,
      frac_vis = 0,
      end_use = varGr)


    # Adjust for other fuels
    if(varFe=='GasEquipment'){
      l_par_outputs <- c(l_par_outputs[1:10],'',l_par_outputs[11])
    }

    if(varFe=='OtherEquipment'){
      varFl <- switch(varFl, 'cal' = 'Coal', 'oil' = 'FuelOilNo1',
                      'bio' = 'OtherFuel1', 'dst' = 'DistrictHeating')
      l_par_outputs <- c(l_par_outputs[1:10])
    }

    # Combine and process data
    l_com <- lblTo[[ip]]
    l_ins <- c(ip, l_par_outputs)
    l_obj <- iterate_on_list(l_ins, l_com)
    txt_res <- c(unlist(l_obj))

    return(txt_res)
  }

  # Helper to assign extra equipment
  assign_extra <- function(varTo, tblTo, lblTo){
    # @title Assigns extra equipment to IDF blocks
    #
    # @description This function creates IDF blocks for extra equipment based on given parameters.
    #
    # @param varTo Target variable for assignment.
    # @param tblTo Table containing extra equipment data.
    # @param lblTo Label mapping table.
    #
    # @return A character vector containing the generated IDF blocks.

    # Extract information
    ip='Exterior:FuelEquipment'
    varPw <- as.numeric(
      subset(tblTo, id==varTo,
             select = c(power__W, power__W__m2, power__W__ppl)))

    varGr <- tblTo %>% 
      dplyr::filter(id==varTo) %>% 
      dplyr::select(type) %>%
      unlist() %>% as.character()

    varFl <- tblTo %>% 
      dplyr::filter(id==varTo) %>% 
      dplyr::select(mainf) %>%
      unlist() %>% as.character()

    varFl <- switch(varFl, 
      'cal' = 'Coal', 
      'oil' = 'FuelOilNo1',
      'bio' = 'OtherFuel1', 
      'dst' = 'DistrictHeating')

    varSh <- gsub('^x+','x', paste0('x',varTo))

    # Generate IDF block parameters
    l_par_outputs <- list(
      name = paste0('x__',varTo),
      fuel = varFl,
      sch = varSh,
      level = round(varPw[1],3),
      end_use = varGr)

    # Combine and process data
    l_com <- lblTo[[ip]]
    l_ins <- c(ip, l_par_outputs)
    l_obj <- iterate_on_list(l_ins, l_com)
    txt_res <- c(unlist(l_obj))

    return(txt_res)
  }


  # Identify non-occupancy columns
  in_occ <- d_sch[grep("^x",d_sch, invert = T)]

  # Separate occupancy zone names and profiles
  varZns <- gsub("status_","",in_occ[grep("^status", in_occ)])
  varPrf <- in_occ[grep("(^Hour|^status)", in_occ, invert=T, ignore.case=T)]

  # Extract occupancy schedules
  in_occ <- in_occ[grep("\\-\\d+\\-\\d+",in_occ)]
  in_occ <- gsub(paste0("__",varZns[1],"$"),"",in_occ)

  # Calculate ubiquity ratio (profiles per schedule)
  ubiquity_ratio <- ceiling(length(varPrf) / length(in_occ))

  # Lighting appliances
  d_lights <- subset(d_app, type=='Lighting')

  # Non-electric appliances
  d_non <- subset(d_app, mainf %in% c('oil','cal','dst','bio'))

  # Basecase appliances (excluding lighting)
  d_bsc <- subset(d_app, mainf!='FALSE' & type!='Lighting')

  # Prepare IDF Objects (placeholder)
  idfAth <- ""


  # Generate idf objects

  #.. object: people
  l_ppl_body <- unlist(
    lapply(varPrf, assign_people, d_sch, l_sup))
  l_ppl_head <- make_idf_block_title('People')
  idfPpl <- c("", l_ppl_head, "", l_ppl_body)

  #.. object: lights
  d_lights <- d_lights %>% dplyr::filter(!is.na(usage__kwh___yr))
  if(dim(d_lights)[1]>0){
    l_lit <- unlist(
      lapply(d_lights$id, assign_lights, d_lights, ubiquity_ratio, l_sup))
  }else{
    l_lit <- ''
  }
  l_lit_head <- make_idf_block_title('Lights')
  idfLit <- c("", l_lit_head, "", l_lit)

  #.. object: appliances
  d_bsc <- d_bsc %>% dplyr::filter(!is.na(usage__kwh___yr))
  if(dim(d_bsc)[1]>0){
    l_apps <- unlist(
      lapply(d_bsc$id, assign_apps, d_bsc, ubiquity_ratio, l_sup))
  }else{
    l_apps <- ''
  }
  l_apps_head <- make_idf_block_title('Equipment')
  idfApp <- c("", l_apps_head, "", l_apps)

  #.. object: extra appliances and external
  if(dim(d_non)[1]>0){
    l_aty <- unlist(
      lapply(d_non$id, assign_apps, d_non, l_sup))
    l_ext <- unlist(
      lapply(d_non$id, assign_extra, d_non, l_sup))
    l_ext_head <- make_idf_block_title('OtherEquipment')
    idfAth <- c("", l_ext_head, "", l_aty, l_ext)
  }

  # Assemble objects
  idfLoads <- c(idfPpl, idfLit, idfApp, idfAth)

  return(idfLoads)
}
