
populate_idf_header <- function(id, l_sum,
                                l_ids=the$core_index,
                                ehs_data=s_ehs_2011_ext,
                                ons_data=s_ons,
                                tus_data=s_tus,
                                chm_data=s_chm){
  #' @title Populates the IDF Header with Building Information
  #' 
  #' @description This function populates the IDF header with 
  #' detailed building information, including characteristics,
  #' energy systems, and construction details. It merges data 
  #' from various sources and updates the building characteristics
  #' based on predefined defaults.
  #'
  #' @param id The ID of the building.
  #' @param l_sum Summary data for the building.
  #' @param l_ids A list of building IDs (default is `the$core_index`).
  #' @param ehs_data A list of EHS data sets (default is `s_ehs_2011_ext`).
  #' @param ons_data A list of ONS data sets (default is `s_ons`).
  #' @param tus_data A list of TUS data sets (default is `s_tus`).
  #' @param chm_data A list of CHM reference data (default is `s_chm`).
  #' @return A list containing the card and IDF data.


  # 1. Helpers ----

  # Helpers to extract building characteristics
  inc <- function(x){ln. <<- x + 1}
  yy <- paste
  sx <- as.character

  # Function to update building characteristics
  update_house_header <- function(l_p, l_def=the$GLOBAL_PAR){

    if('total_floor_area' %in% names(l_def)){
      v_ori <- l_p$floor_area$value
      v_ovr <- l_def$total_floor_area
      l_p$floor_area$value <- v_ovr
    }

    if('glazing_ratio' %in% names(l_def)){
      v_ori <- l_p$glazing_ratio
      v_ovr <- l_def$glazing_ratio
      l_p$glazing_ratio <- v_ovr
    }

    if('orientation_front' %in% names(l_def)){
      v_ori <- l_p$orientation
      v_ovr <- l_def$orientation_front
      l_p$orientation <- paste(as.character(v_ovr)," North")
    }

    if('heating_system_code' %in% names(l_def)){
      v_ori <- l_p$energy_system$code
      v_ovr <- l_def$heating_system_code
      l_heat_code <- decode_energy_system(v_ovr)
      l_p$energy_system$code <- v_ovr
      l_p$energy_system$space_heat <-
        l_energy_systems$space_heating[l_heat_code$msh]
      l_p$energy_system$hot_water <-
        l_energy_systems$hot_water[l_heat_code$dhw]
      l_p$energy_system$central_heating <- !l_heat_code$msh %in% c(7,8)
    }

    if('wall_type' %in% names(l_def)){
      v_ori <- l_p$constructions$wall$type
      v_ovr <- l_def$wall_type
      l_p$constructions$wall$type <- v_ovr
    }

    if('wall_thickness' %in% names(l_def)){
      v_ori <- l_p$constructions$wall$thickness
      v_ovr <- l_def$wall_thickness
      l_p$constructions$wall$thickness <- v_ovr
    }

    if('wall_transmittance' %in% names(l_def)){

      v_ovr <- l_def$wall_transmittance
      l_p$constructions$wall$transmittance <- v_ovr

      if('wall_type' %in% names(l_def)){
        v_ori <- l_p$constructions$wall$type
        v_ovr <- l_def$wall_type
        l_p$constructions$wall$type <- v_ovr
      }else{
        l_p$constructions$wall$type <- '<undefined>'
      }

      if('wall_thickness' %in% names(l_def)){
        v_ori <- l_p$constructions$wall$thickness
        v_ovr <- l_def$wall_thickness
        l_p$constructions$wall$thickness <- v_ovr
      }else{
        l_p$constructions$wall$thickness <- '<undefined>'
      }

    }

    return(l_p)
  }


  # 2. Load data ----

  # Read templates
  txt_map <- l_banners$maps
  txH <- l_banners$head

  # Obtain identifiers
  varID <- l_ids[id,]
  id <- get_aacode(varID$index)
  idf_uuid <-
    ifelse('uuid' %in% names(the$GLOBAL_PAR), the$GLOBAL_PAR$uuid, uuid())

  #.. collect some global data sets
  tblDwl <- subset(ehs_data$dwell, V001_HousingCode==id)
  tblGmt <- subset(ehs_data$geom, V001_HousingCode==id)
  tblVnt <- subset(ehs_data$vent, V001_HousingCode==id)
  tblLss <- subset(ehs_data$other, V001_HousingCode==id)
  tblMsh <- subset(ehs_data$space, V001_HousingCode==id)
  tblDhw <- subset(ehs_data$hot, V001_HousingCode==id)
  tblLel <- subset(ehs_data$low, V001_HousingCode==id)
  tblCom <- subset(ehs_data$comp, V001_HousingCode==id)
  tblSum <- subset(ehs_data$summ, V001_HousingCode==id)

  varMsh <- chm_data$CHM_MainHeatingSystemType
  varDhw <- chm_data$CHM_HotDHWSystem
  varWall <- chm_data$CHMtbl_EHS_CHM_wallType
    varWall <- varWall[1:16,c(2,6)]; colnames(varWall) <- c('type','info')

  tblScd <- gsub("[[:blank:]]*","", tus_data$essentials$name)

  # Extract model parameters
  tblOcc <- l_sum$schedules$occupants
  varN <- l_sum$context$orientation
  varLps <- l_sum$location$sim_lapse


  # 3. Summarise data ----

  # Make summary list
  l_sys <- l_hid <- l_run <- list()
  l_sys[['reference']] <- 'EnergyPlus v24.1 / R 4.4.1'
  l_sys[['timestep']] <- list(
    value=paste(60, '/', l_sum$context$time_step), units='minutes')
  l_sys[['simulation_period']] <- list(
    from=list(month=varLps$month_i,day=varLps$day_i),
    to=list(month=varLps$month_e,day=varLps$day_e))
  l_run[['energyplus']] <- the$.enhub.config$eplus$dir
  l_hid[['cuboid']] <- sx(l_sum$envelope$cuboid$cuboid)
  l_hid[['thermal_zones']] <- list(
    summary = l_sum$envelope$zones$thermal_zones,
    map = l_sum$envelope$zones$map_zones$room_assignment,
    spaces = l_sum$envelope$zones$thermal_spaces)
  l_hid[['typology']] <- list(
    value=sx(tblCom$V583_CuboidType), code=sx(tblSum$.hubtyp))
  l_hid[['epoch']] <- list(
    value=sx(tblCom$V584_CuboidEpoch), code=sx(tblSum$.hubage))
  l_hid[['floor_area']] <- list(
    value=sx(tblCom$V534_TFAsurvey), units='m2')
  l_hid[['glazing']] <- list(
    ratio = tblCom$V563_GlazingRatio,
    surface = l_sum$envelope$zones$glazing)
  l_hid[['rurality']] <- sx(tblCom$V513_Rurality)
  l_hid[['sparsity']] <- sx(tblCom$V514_Sparsity)
  l_hid[['urbanity']] <- sx(tblCom$V573_Urbanity)
  l_hid[['street']] <- sx(tblCom$V572_Street)
  l_hid[['orientation']] <- paste(sx(varN)," North")
  l_hid[['energy_system']] <- list(
    space_heat = sx(varMsh$type[tblMsh$D082_MainHeatingSystemType]),
    hot_water = sx(varDhw$dhw[tblDhw$D097_DHWSystemType]),
    central_heating = sx(tblDhw$D096_DHWSystemWithCentralHeating),
    solar_heating = sx(tblDhw$D109_SolarDHWSystem),
    code = sx(tblSum$.heatid),
    heating_operation=l_sum$heat_loop$systems$main_regime,
    setpoint_heating=unique(l_sum$heat_loop$zones$htg_schedules),
    setpoint_cooling=unique(l_sum$heat_loop$zones$clg_schedules),
    diagram=l_sum$heat_loop$diagram)
  l_hid[['lzc']] <- l_sum$lzc
  l_hid[['constructions']] <- list(
    wall = list(type = varWall$type[tblGmt$D077_ExternalWallConstruction],
                thickness = sx(tblGmt$D030_WallThickness)),
    roof = list(type = sx(tblLss$D079_RoofConstruction)),
    floor = list(type = sx(tblLss$D075_GroundFloorConstructionType)),
    summary = l_sum$heat_loss)

  l_hid <- update_house_header(l_hid)

  l_simcard <- list(
    uuid = idf_uuid,
    ehs_base = id,
    timestamp = format(Sys.time(), "%b %d %Y %X"),
    environment=l_sys,
    run=l_run,
    house = l_hid,
    weather = l_sum$location$epw)


  # 4. Generaet idf list ----

  # Create a line-by-line list with archetype information
  ln. <- length(txH)

  txH[inc(ln.)] <- "! ====================================================================== "
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    ",l_simcard$environment$reference)
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Cuboid id-CHM : ",sx(varID$index))
  txH[inc(ln.)] <- yy("!    Cuboid id-EHS : ",sx(id))
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Time-step assigned : ", l_simcard$environment$timestep$value,l_simcard$environment$timestep$units)
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    [Context]")
  txH[inc(ln.)] <- yy("!    Built Epoch : ", l_simcard$house$epoch$value)
  txH[inc(ln.)] <- yy("!  Dwelling Type : ", l_simcard$house$typology$value)
  txH[inc(ln.)] <- yy("!       Rurality : ", l_simcard$house$rurality)
  txH[inc(ln.)] <- yy("!       Sparsity : ", l_simcard$house$sparsity)
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    [Climate]")
  txH[inc(ln.)] <- yy("!     Reference :", sx(l_sum$location$epw$use_weather_file))
  txH[inc(ln.)] <- yy("!        Region :", sx(l_sum$location$epw$region_in_survey))
  txH[inc(ln.)] <- yy("!     Longitude :", sx(l_sum$location$epw$info$lon))
  txH[inc(ln.)] <- yy("!      Latitude :", sx(l_sum$location$epw$info$lat))
  txH[inc(ln.)] <- yy("!      Location :", sx(l_sum$location$epw$info$place))
  txH[inc(ln.)] <- yy("!   Orientation :", l_simcard$house$orientation)
  txH[inc(ln.)] <- yy("!         (epw) :", sx(l_sum$location$epw$epw))
  txH[inc(ln.)] <- "!"
  txH <- c(txH, txt_map)
  txH[ln. <- ln. + length(txt_map) + 1] <- "!"
  txH[inc(ln.)] <- paste("!    Simulation Period : ",paste(as.character(unlist(l_simcard$environment$simulation_period)),collapse = '-'))
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Schedules :  occupancy + appliances + energy systems + infiltration & ventilation")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Appliances : ", yy(tblScd[1:4], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[5:8], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[9:12], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[13:16], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[17:20], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[21:24], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[25:28], collapse = " / "))
  txH[inc(ln.)] <- yy("!         ", yy(tblScd[29:32], collapse = " / "))
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- "! ----------------------------------------------------------------------"
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    [Typology]")
  txH[inc(ln.)] <- yy("!             Cuboid Id :", l_simcard$house$cuboid)
  txH[inc(ln.)] <- yy("!    Dwelling Type (5x) :", l_simcard$house$typology$code)
  txH[inc(ln.)] <- yy("!      Built Epoch (5x) :", l_simcard$house$epoch$code)
  txH[inc(ln.)] <- yy("!        Heating System :", l_simcard$house$energy_system$code)
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    [Geometry]")
  txH[inc(ln.)] <- yy("!             Storeys :", sx(tblCom$V574_CuboidFloors),"(excluding attic and basement)")
  txH[inc(ln.)] <- yy("!        Room-in-roof :", ifelse(tblCom$V576_CuboidAttic==T,"yes","no"))
  txH[inc(ln.)] <- yy("!     Basement/Cellar :", ifelse(tblCom$V577_CuboidBasement==T,"yes","no"))
  txH[inc(ln.)] <- yy("!      Attached sides :", sx(tblCom$V582_CuboidAttachments))
  txH[inc(ln.)] <- yy("!    Total Floor Area :", paste(l_simcard$house$floor_area$value, l_simcard$house$floor_area$units))
  txH[inc(ln.)] <- yy("!       Fa\u00E7ade Height :", sx(tblCom$V543_AverageHeight * tblCom$V574_CuboidFloors), "m")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!  Infiltration (ach) : 65% + 35% zones")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Roof obstruction : ", sx(tblCom$V568_RoofObstruction))
  txH[inc(ln.)] <- yy("!      Window shading : ", sx(tblCom$V567_WindowShading))
  txH[inc(ln.)] <- yy("!       Glazing ratio : ", sx(l_simcard$house$glazing_ratio))
  txH[inc(ln.)] <- yy("!        Window types : ", sx(tblLss$D068_Windows1Type), " & ", sx(tblLss$D070_Windows2Type))
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!      Main wall type : ", sx(l_simcard$house$constructions$wall$type))
  txH[inc(ln.)] <- yy("!      Wall thickness : ", sx(l_simcard$house$constructions$wall$thickness))
  txH[inc(ln.)] <- yy("!           Roof type : ", sx(l_simcard$house$constructions$roof$type))
  txH[inc(ln.)] <- yy("!   Ground Floor type : ", sx(l_simcard$house$constructions$floor$type))
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Appliances : ")
  txH[inc(ln.)] <- yy("!    -- calibrated with ECUK-tbl-EnergyUsage")
  txH[inc(ln.)] <- yy("!       split for gains and external meter readings")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!      Occupants :", sx(tblOcc$.occupant_adult),"adults &",sx(tblOcc$.occupant_child),"children")
  txH[inc(ln.)] <- yy("!    (BREDEM eq) :", sx(tblOcc$occupancy_BREDEM), "if non-existent, is calculated as described in BREDEM")
  txH[inc(ln.)] <- yy("!    (EUSILC eq) :", sx(tblOcc$occupancy_EUSILC), "assumed contributions: 50% adult and 30% children")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    [Heating Systems]")
  txH[inc(ln.)] <- yy("!    -- Implemented using `OpenStudio` ")
  txH[inc(ln.)] <- yy("!       see Documentation for additional considerations")
  txH[inc(ln.)] <- yy("!    Main Space Heating (MSH) :",l_simcard$house$energy_system$space_heat)
  txH[inc(ln.)] <- yy("!    Domestic Hot Water (DHW) :",l_simcard$house$energy_system$hot_water)
  txH[inc(ln.)] <- yy("!             Central Heating :",l_simcard$house$energy_system$central_heating)
  txH[inc(ln.)] <- yy("!                   Solar DHW :",l_simcard$house$energy_system$solar_heating)
  txH[inc(ln.)] <- yy("!      Heating Operation Mode :",l_simcard$house$energy_system$heating_operation)
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    Water Net : ", round(tblOcc$occupancy_EUSILC * 125 * 0.001, 6) ," m3/s (default value)")
  txH[inc(ln.)] <- yy("!    -- This value is adjusted according to the implemented templates")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    [Information]")
  txH[inc(ln.)] <- yy("!           Conservatory :", sx(tblCom$V505_Conservatory))
  txH[inc(ln.)] <- yy("!           Solar panels :", sx(tblCom$V509_SolarPanels))
  txH[inc(ln.)] <- yy("!    Solar photovoltaics :", sx(tblCom$V510_SolarPhotovoltaics))
  txH[inc(ln.)] <- yy("!        Front extension :", sx(tblCom$V501_FrontExtension))
  txH[inc(ln.)] <- yy("!         Rear extension :", sx(tblCom$V503_RearExtension))
  txH[inc(ln.)] <- yy("!                   [ Bal: balcony; ByM: bay window (multiple); ByS: bay window (single);")
  txH[inc(ln.)] <- yy("!                     Con: conservatory; RfX: roof extension; Drm: dormer; Prh: porch ]")
  txH[inc(ln.)] <- yy("!             Exposition :", sx(tblCom$V570_Exposition))
  txH[inc(ln.)] <- yy("!               Urbanity :", l_simcard$house$urbanity)
  txH[inc(ln.)] <- yy("!         Rows of Houses :", sx(tblCom$V571_Rows))
  txH[inc(ln.)] <- yy("!           Street mode  :", l_simcard$house$street)
  txH[inc(ln.)] <- "!"

  # if(l_simcard$house, grepl('Archetype')){}

  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- "! ---EHS household reference-----------------------------------------"
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    weight-dwelling : ", sx(tblDwl$V002_DwellingWeight))
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!    household income (join) :", sx(tblCom$V525_IncomeCombined),"\u00A3/yr")
  txH[inc(ln.)] <- yy("!           fuel expenditure :", sx(tblCom$V528_FuelExpenditure),"\u00A3/yr")
  txH[inc(ln.)] <- yy("!         ..on water heating :", sx(tblCom$V530_DHWCost),"\u00A3/yr")
  txH[inc(ln.)] <- yy("!         ..on space heating :", sx(tblCom$V529_SpaceHeatCost),"\u00A3/yr")
  txH[inc(ln.)] <- yy("!              ..on lighting :", sx(tblCom$V531_LightingCost),"\u00A3/yr")
  txH[inc(ln.)] <- "!"

	# Append timestamp and uuid
  txH[inc(ln.)] <- "! ====================================================================== "
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- yy("!  [ File generated on ", l_simcard$timestamp, " ]")
  txH[inc(ln.)] <- yy("!  [ UUID : ", l_simcard$uuid, " ]")
  txH[inc(ln.)] <- "!"
  txH[inc(ln.)] <- " "


  # 5. Generate module card ----

  # Summarize information and create IDF object
  l_head <- list(card = jsonlite::toJSON(l_simcard, pretty=T, auto_unbox=T),
                 idf = txH)

  invisible(l_head)
}
