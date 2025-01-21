
# Data objects ----

estimate_solar_parameters <- function(idc, l_EPW,
                                      l_EHS = s_ehs_2011_ext,
                                      l_ONS = s_ons,
                                      l_SAP = s_sap,
                                      l_BREDEM = s_bredem,
                                      l_NOABL = s_noabl,
                                      l_MetO = s_metoffice){
  #' @title Estimates solar parameters for a specific dwelling.
  #'
  #' @description This function calculates various solar parameters based on provided data sets for a dwelling.
  #'
  #' @param idc Character string representing the dwelling code.
  #' @param l_EPW A list containing weather data (assumed to include latitude information).
  #' @param l_EHS A list containing dwelling and household information (assumed to include dwelling information and complementary data).
  #' @param l_ONS (Optional) A list containing additional data (not used in this function).
  #' @param l_SAP A list containing solar access pattern data.
  #' @param l_BREDEM A list containing horizontal radiation data.
  #' @param l_NOABL A list containing wind data (assumed to include regional data).
  #' @param l_MetO A list containing average external temperature data (assumed to include regional data).
  #'
  #' @return A list containing various solar parameter data frames.

  # Helper function to estimate vertical radiation
  estimate_vertical_rad <- function(i_win_ori, df_lat, df_rad_vert) {

    i_win_ori <- ifelse("south" %in% i_win_ori, 180,
      ifelse("southeast/southwest" %in% i_win_ori, 135,
        ifelse("east/west" %in% i_win_ori, 90,
          ifelse("northeast/northwest" %in% i_win_ori, 45, 0)
        )
      )
    ) * pi / 180

    tblRad <- (0.702 - 0.0119 * df_lat + 0.000204 * (df_lat)^2 +
      ((-0.107 + 0.0081 * df_lat - 0.000218 * (df_lat)^2) *
        cos(i_win_ori)) + ((0.117 - 0.0098 * df_lat + 0.000143 *
        (df_lat)^2) * cos(2 * i_win_ori))) * df_rad_vert

    return(tblRad)
  }

  # Extract relevant data from EHS data sets
  df_Inf <- subset(l_EHS[['dwelling.and.household.information']],
                    V001_HousingCode==idc)
  df_Cmp <- subset(l_EHS[['complementary']],
                    V001_HousingCode==idc)
  df_Lss <- subset(l_EHS[['other.heat.loss.elements']],
                    V001_HousingCode==idc)

  # Process variables
  varN <- df_Cmp$V585_CuboidOrientation
  varN <- ifelse(is.na(varN), 0, varN) #.. assume 0 if unknown
  varLoc <- as.character(df_Inf$D003_Region)
  varLat <- l_EPW$info$lat

  # Calculate latitude adjusted solar declination
  df_lat_adj <- l_SAP$SolarDeclination$declination
  df_lat_adj <- varLat - df_lat_adj

  # Extract window orientations and find region index
  varOri <- c(df_Lss$H042_Windows1Orientation,
              df_Lss$H047_Windows1Orientation,
              df_Lss$H051_RoofWindowOrientation)
  i_region <- grep(varLoc,l_SAP$RegionLatitudes$region,ignore.case = T)

  # Define orientation labels and calculate vertical radiation for each
  lbl_orientations <-
    c('south','southeast/southwest','east/west', 'northeast/northwest','north')
  df_hrz_srf <-
    as.numeric(unlist(l_BREDEM$HorizontalRadiation[,i_region+1]))
  df_vrt_rad <-
    lapply(lbl_orientations, estimate_vertical_rad, df_lat_adj, df_hrz_srf)
  names(df_vrt_rad) <- lbl_orientations
  df_vrt_rad <- as.data.frame(df_vrt_rad)

  # Calculate average monthly values for horizontal and vertical radiation
  df_vrt_avg <- df_vrt_rad * the$l_days$perMonth$Days * 24 / 1000
  df_hrz_avg <- df_hrz_srf * the$l_days$perMonth$Days * 24 / 1000

  # Calculate solar collector efficiency for various tilts
  df_solar_collector <-
    t(t(s_sap$CollectorFactor_Adjusted[,2:6]) * colSums(df_vrt_avg))
  df_solar_collector[1,] <- sum(df_hrz_avg)
  df_solar_collector <- as.data.frame(df_solar_collector)
  rownames(df_solar_collector) <- s_sap$CollectorFactor_Adjusted$tilt

  # Find optimal collector tilt and orientation
  i_max <-
    which(df_solar_collector == max(df_solar_collector), arr.ind = TRUE)
  i_max <-
    c(as.character(s_sap$CollectorFactor_Adjusted$tilt[i_max[1]]),
      as.character(lbl_orientations[i_max[2]]))

  # Re-format table
  df_solar_par <-
    data.frame(Month = unlist(l_SAP$SolarDeclination$month),
               Temperature = unlist(l_MetO$AvgExtTem[,i_region+1]),
               Wind = unlist(l_NOABL$AvgWnd[,i_region+1]))
  df_solar_par$Horizontal <- df_hrz_srf
  df_solar_par <- cbind(df_solar_par,df_vrt_rad)
  rownames(df_solar_par) <- NULL

  # Make list of tables
  df_out <- list(
    collector_parameters = df_solar_collector,
    collector_optimal = i_max,
    window_orientation = varOri,
    summary = df_solar_par)

  return(df_out)
}

encode_renewable <- function(id_ren){
  #' @title Encodes renewable energy information.
  #'
  #' @description Converts a numeric or character string representing renewable energy types into a logical vector.
  #'
  #' @param id_ren A numeric or character string representing renewable energy types.
  #'   If numeric, it's interpreted as a binary code. If character, it's a string of binary digits.
  #' @return A logical vector indicating the presence of different renewable energy types:
  #'   w.biogas, x.turbine, y.water, z.solarpv.

  if (length(id_ren) > 1) {
    # Numeric input: convert to binary string
    t_out <- as.integer(id_ren)
    t_out <- paste(t_out, collapse = "")
  } else {
    # Character input: convert to logical vector
    t_out <- unlist(strsplit(id_ren, ""))
    t_out <- as.logical(as.integer(t_out))
    names(t_out) <- c("w.biogas", "x.turbine", "y.water", "z.solarpv")
  }
  return(t_out)
}

obtain_daily_lzc <- function(idc, s_epw, s_ehs_ref = s_ehs_2011_ext,
                              s_sap_ref = s_sap, s_bredem_ref = s_bredem){
  #' @title Calculates daily solar energy potential for a specific dwelling.
  #'
  #' @description This function calculates the daily solar energy potential for a dwelling based on weather data and building geometry.
  #'
  #' @param idc Character string representing the dwelling code.
  #' @param s_epw A list containing weather data (assumed to include latitude information).
  #' @param s_ehs_2011_ext A list containing dwelling and household information (assumed to include geometry and complementary data).
  #' @param s_sap A list containing solar access pattern data (assumed to include solar declination data).
  #' @param s_bredem A list containing BRE DEM data (assumed to include flux constants and global irradiance data).
  #'
  #' @return A numeric value representing the daily solar energy potential.


  # Extract relevant data from EHS data sets
  df_Rgn <- s_epw$info
  df_Gmt <- s_ehs_ref$geometry %>% dplyr::filter(V001_HousingCode == idc)
  df_Cmp <- s_ehs_ref$complementary %>% dplyr::filter(V001_HousingCode == idc)

  # Extract relevant data from SAP and BRE DEM data sets
  df_SAP_den <- s_sap_ref[['SolarDeclination']]
  df_BRE_flx <- s_bredem_ref[['FluxConstants']]
  df_BRE_ird <- s_bredem_ref[['GlobalIrradiance']]

  # Calculate latitude in radians and pitch factor
  k_latitude <- df_Rgn$lat * 0.0174533
  k_pitch <- 40 * 0.0174533
  k_pitch_factor <- sin((pi/180)*(k_pitch/2))

  # Extract and process cuboid orientation
  k_ori <- df_Cmp$V585_CuboidOrientation
  k_ori <- ifelse(is.na(k_ori), 0, k_ori)
  k_ori <- ifelse(k_ori==0, 'north',
            ifelse(k_ori==45 | k_ori==315, 'ne_nw',
            ifelse(k_ori==90 | k_ori==270, 'e_w',
            ifelse(k_ori==135 | k_ori==225, 'se_sw', 'south'))))

  # Calculate pitch-dependent coefficients for solar radiation
  k_pitch_a <- df_BRE_flx[1,k_ori] * k_pitch_factor^3 +
    df_BRE_flx[2,k_ori] * k_pitch_factor^2 +
    df_BRE_flx[3,k_ori] * k_pitch_factor
  k_pitch_b <- df_BRE_flx[4,k_ori] * k_pitch_factor^3 +
    df_BRE_flx[5,k_ori] * k_pitch_factor^2 +
    df_BRE_flx[6,k_ori] * k_pitch_factor
  k_pitch_c <- df_BRE_flx[7,k_ori] * k_pitch_factor^3 +
    df_BRE_flx[8,k_ori] * k_pitch_factor^2 +
    df_BRE_flx[9,k_ori] * k_pitch_factor

  # Calculate solar height for each month based on solar declination
  k_solar_height <-
    lapply(df_SAP_den$declination, function(x) cos((pi/180)*(k_latitude*x)))
  k_solar_height <- plyr::ldply(k_solar_height, data.frame)
  colnames(k_solar_height) <- 'SolarHeight'

  # Calculate ratio of solar flux on the pitched roof compared to horizontal
  k_solar_height$RatioFlux <-
    as.numeric(k_pitch_a)*k_solar_height$SolarHeight^2 +
    as.numeric(k_pitch_b)*k_solar_height$SolarHeight + as.numeric(k_pitch_c)

  # Calculate monthly horizontal solar flux from irradiance data
  k_solar_height$HorizontalFlux <-
    as.vector(k_solar_height$RatioFlux) *
    as.numeric(df_BRE_ird[df_BRE_ird$region=='UK_average',2:13])

  # Calculate monthly incident solar radiation on the pitched roof
  k_solar_incident <- lapply(1:12, function(x)
    k_solar_height$HorizontalFlux[x] * 0.024 * days_in_month(x))
  k_solar_incident <- plyr::ldply(k_solar_incident, data.frame)
  colnames(k_solar_incident) <- 'Incidence'
  k_solar_height$Incidence <- k_solar_incident$Incidence
  k_solar_incident_total <- sum(k_solar_height$Incidence)

  # Calculate pitched area
  k_pitch_area <- df_Gmt$D061_RoofArea
  k_pitch_area <- ifelse(is.na(k_pitch_area) | k_pitch_area==0, 30, k_pitch_area)
  k_pitch_area <- k_pitch_area * 0.50

  # Adjust pitched area
  res <- k_solar_incident_total * k_pitch_area

  return(res)
}


# IDF objects ----

define_hosting_pv_surface <- function(l_dims){
  #' @title Defines hosting PV surfaces.
  #'
  #' @description This function identifies suitable roof zones for hosting PV panels based on provided dimensions and map data.
  #' It extracts relevant roof information, calculates slopes and dimensions, and determines candidate surfaces for PV installation.
  #' Finally, it calculates the location of potential PV surfaces.
  #'
  #' @param l_dims A list containing building dimensions, zone information, and surface details.
  #' @return A list of data frames, each representing the coordinates of a potential PV surface.
  #'

  # Helper to extract zone dictionary
  extract_roof_zone_origins <- function(l_origin){
    #' @title Extracts roof zone origins from a list of origins.
    #'
    #' @description This function takes a list of origins as input and extracts the origin points
    #' for each roof zone. The extracted points are then converted into a data frame
    #' with additional information about the corresponding room.
    #'
    #' @param l_origin A list of origins.
    #' @return A data frame containing the x, y, z coordinates of the roof zone origins
    #'   and the corresponding room label as the 'zone' column.
    #'

    #' Extract roof zone information from the origin list.
    l_roof_orig <- l_origin[[k_roof_zone]]

    #' Extract origin points and convert to a data frame.
    d_points <- lapply(l_roof_orig, '[[', "origin") %>% data.frame() %>% t()

    #' Extract room labels.
    k_labels <- lapply(l_roof_orig, '[[', "room") %>% unlist()

    #' Set column names for the points data frame.
    colnames(d_points) <- c('x', 'y', 'z')

    #' Convert points to a tibble and add zone information.
    d_points <- d_points %>% data.frame() %>% tibble::tibble() %>%
      dplyr::mutate(zone = k_labels) %>%
      dplyr::relocate(zone)

    #' Return the resulting data frame.
    return(d_points)
  }

  # Helper to extract surface vertices
  extract_roof_vertices <- function(k, l_roof){
    #' @title Extracts roof vertices from a list of roofs.
    #'
    #' @description This function extracts the vertex coordinates for a specific roof from a list of roofs.
    #' The extracted vertices are converted into a data frame with columns for x, y, and z coordinates.
    #'
    #' @param k The index of the roof in the list.
    #' @param l_roof A list of roof data.
    #' @return A data frame containing the x, y, z coordinates of the roof vertices.
    #'

    #' Extract vertex data and convert to a data frame.
    d_vert <- l_roof[[k]]$vertices %>% data.frame() %>% t() %>% data.frame()

    #' Set column names for the vertices data frame.
    colnames(d_vert) <- c('x', 'y', 'z')

    #' Return the resulting data frame.
    return(d_vert)
  }

  # Helper to obtain slope and dimensions
  obtain_slope_and_ranges <- function(l_vertx){
    #' @title Calculates slope and dimensions of a roof.
    #'
    #' @description This function determines the slope, dimensions, and area of a roof
    #' based on its vertex coordinates.
    #'
    #' @param l_vertx A data frame containing the x, y, and z coordinates of the roof vertices.
    #' @return A list containing the original vertex data, slope angle (alpha),
    #'   x-dimension (x_delta), y-dimension (y_delta), z-dimension (z_delta), and area.
    #'

    #' Calculate dimensions of the roof.
    x_delta <- max(l_vertx$x) - min(l_vertx$x)
    y_delta <- max(l_vertx$y) - min(l_vertx$y)
    z_delta <- max(l_vertx$z) - min(l_vertx$z)

    #' Calculate slope and area.
    c <- sqrt(z_delta^2 + y_delta^2)
    alpha <- atan(z_delta / y_delta) * 180 / pi
    area <- c * x_delta

    #' Create a list to hold results.
    l_res <- list(vertices = l_vertx,
                  dimensions = list(alpha = alpha,
                                    x_delta = x_delta,
                                    y_delta = y_delta,
                                    z_delta = z_delta,
                                    area = area))

    #' Return the results.
    return(l_res)
  }

  # Helper to define vertices of surfaces
  locate_pv_surfaces <- function(k, l_surface, d_ori){
    #' @title Locates photovoltaic (PV) surfaces.
    #'
    #' @description This function determines the coordinates of a PV surface based on its dimensions,
    #' orientation, and position.
    #'
    #' @param k The index of the PV surface in the list of surfaces.
    #' @param l_surface A list containing surface data, including dimensions and vertices.
    #' @param d_ori A data frame with orientation data for each zone.
    #' @return A data frame containing the x, y, and z coordinates of the PV surface vertices.
    #'

    #' Extract surface data.
    l_sub <- l_surface[[k]]

    #' Filter orientation data for the current zone.
    d_sub <- d_ori %>% dplyr::filter(zone == k)

    #' Extract dimensions and vertices.
    l_dim <- l_sub$dimensions
    d_vrt <- l_sub$vertices %>% tibble::tibble()

    #' Calculate x and y coordinates of the PV surface.
    x_par <- c(l_dim$x_delta * 0.1, l_dim$x_delta * 0.99) + d_sub$x
    y_par <- c(l_dim$y_delta * 0.1, l_dim$y_delta * 0.99) + d_sub$y

    #' Calculate z coordinates of the PV surface.
    z_up <- (max(y_par) - min(y_par)) * tan(l_dim$alpha * pi / 180) + d_sub$z
    z_bt <- (min(y_par)) * tan(l_dim$alpha * pi / 180) + d_sub$z

    #' Round x, y, and z coordinates to six decimal places.
    x_lf <- min(x_par) %>% round(6)
    x_rg <- max(x_par) %>% round(6)
    y_fr <- min(y_par) %>% round(6)
    y_re <- max(y_par) %>% round(6)
    z_up <- z_up %>% round(6)
    z_bt <- z_bt %>% round(6)

    #' Create a data frame with the PV surface vertices.
    d_srf <- data.frame(x = c(x_lf, x_rg, x_rg, x_lf),
                        y = c(y_fr, y_fr, y_re, y_re),
                        z = c(z_bt, z_bt, z_up, z_up))

    #' Return the PV surface data.
    return(d_srf)
  }


  #' Extract roof zone information.
  k_zones <- l_dims$zones$thermal_zones$storey
  k_roof_zone <- levels(k_zones)[length(levels(k_zones))] %>% gsub('storey_', '', .)
  l_roof_zone <- lapply(l_dims$zones$surface_detail$opaque[[k_roof_zone]], '[[', "f_top")

  #' Extract and process roof zone origins and vertices.
  d_origins <- extract_roof_zone_origins(l_dims$zones$origins)
  l_vert <- lapply(names(l_roof_zone), extract_roof_vertices, l_roof_zone)
  names(l_vert) <- names(l_roof_zone)
  l_slope <- lapply(l_vert, obtain_slope_and_ranges)

  #' Determine candidate surfaces based on map information.
  m_map <- l_dims$zones$map_zones$room_location[[k_roof_zone]]
  k_panel_fraction <- 'half'

  if (k_panel_fraction == 'full') {
    k_candidate <- as.character(m_map)
  } else if (k_panel_fraction == 'half') {
    k_candidate <- as.character(m_map[dim(m_map)[1], ])
  } else {
    k_candidate <- as.character(m_map[dim(m_map)[1], dim(m_map)[2]])
  }

  #' Filter slopes and calculate PV surface locations.
  l_slope <- l_slope[k_candidate]
  d_location <- lapply(names(l_slope), locate_pv_surfaces, l_slope, d_origins)
  names(d_location) <- names(l_slope)

  #' Return the calculated PV surface locations.
  return(d_location)
}

generate_lzc_hosting_pv_surface <- function(l_iter, l_sup = l_idf_blocks){
  #' @title Create IDF block for Shading:Building based on surface data
  #'
  #' @description This function generates an IDF block representing a detailed
  #' shading building using surface data (`l_iter`). It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(k_in, i_act, l_in, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Extract table of coordinates
    d_coords <- l_in[[k_in]]

    # Define loop parameter list
    l_values <- list(
      namez = paste0(k_in, '--solar-collector'),
      trnsn = '',
      nmbov = 4.0,
      vr1xm = d_coords$x[1],
      vr1ym = d_coords$y[1],
      vr1zm = d_coords$z[1],
      vr2xm = d_coords$x[2],
      vr2ym = d_coords$y[2],
      vr2zm = d_coords$z[2],
      vr3xm = d_coords$x[3],
      vr3ym = d_coords$y[3],
      vr3zm = d_coords$z[3],
      vr4xm = d_coords$x[4],
      vr4ym = d_coords$y[4],
      vr4zm = d_coords$z[4])

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
  i_active <- "Shading:Building:Detailed"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(names(l_iter), make_idf_block, i_active, l_iter) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_pv_generator <- function(l_iter, l_sup = l_idf_blocks){
  #' @title Create IDF block for Generator:Photovoltaic
  #'
  #' @description This function generates an IDF block representing a
  #' photovoltaic generator using surface data (`l_iter`). It also uses
  #' supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(k_in, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namez = paste0(k_in, '--solar-collector'),
      srfcn = paste0(k_in, '--solar-collector'),
      phpot = 'PhotovoltaicPerformance:Simple',
      mdlpn = 'PV Constant Efficiency : rated',
      httim = 'Decoupled',
      nossipd = 1.0,
      nomisd = 1.0)

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
  i_active <- "Generator:Photovoltaic"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(names(l_iter), make_idf_block, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_pv_performance <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for PhotovoltaicPerformance:Simple
  #'
  #' @description This function generates an IDF block representing a
  #' photovoltaic performance object, It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){
    
    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'PV Constant Efficiency : rated',
      fosawascd = 1,
      cneim = 'Fixed',
      vfceif = 0.175)

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
  i_active <- "PhotovoltaicPerformance:Simple"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_pv_inverter <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Inverter:Simple
  #'
  #' @description This function generates an IDF block representing a
  #' photovoltaic inverter object, It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){
    
    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'pv-inverter-simple',
      avlsn = 'Always On',
      zonnm = '',
      rdtvf = 0.3,
      invre = 0.95)

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
  i_active <- "ElectricLoadCenter:Inverter:Simple"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_pv_battery <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Storage:Simple
  #'
  #' @description This function generates an IDF block representing a
  #' photovoltaic storage object, It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'pv--battery',
      avlsn = 'Always On',
      zonnm = '',
      rffzhg = 0.0,
      neefc = 0.7,
      nmdee = 0.7,
      mxscj = 2.88E+7,
      mpfdw = 25000,
      mpfcw = 50000,
      isocj = 2.2E+6)

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
  i_active <- "ElectricLoadCenter:Storage:Simple"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_pv_load_centre_generators <- function(l_iter, i_pwr, l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Generators
  #'
  #' @description This function generates an IDF block representing a
  #' load centre generator using surface data (`l_iter`) and estimated power
  #' capacity (`i_pwr`). It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing loop properties (e.g., name, temperature).
  #' @param i_pwr: A value indicating power capacity
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(k_in, i_pw, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    l_objs <- list()
    for(j in k_in){
      l_objs <- c(l_objs, list(
        gnrn = paste0(j, '--solar-collector'),
        gnot = 'Generator:Photovoltaic',
        grepow = i_pw,
        gasn = 'Always On',
        grttepr = ''))
    }

    # Define loop parameter list
    l_values <- list(name = 'inverter-generator-list', l_objs)

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
  i_active <- "ElectricLoadCenter:Generators"

  # Obtain active zones
  k_zones <- names(l_iter)

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(k_zones, i_pwr, i_active) %>%
    unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_pv_load_centre_distribution <- function(i_battery, l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Distribution
  #'
  #' @description This function generates an IDF block representing a
  #' load centre distribution object considering the presence of a battery
  #' (`i_battery`). It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param i_battery: A value indicating the presence of a battery
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(k_st, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Adjust presence of a battery
    k_elcbt <- ifelse(k_st, 'DirectCurrentWithInverter', '')
    k_invrn <- ifelse(k_st, 'pv-inverter-simple', '')

    # Define loop parameter list
    l_values <- list(
      name = 'Inverter',
      gnrln = 'inverter-generator-list',
      gnost = 'Baseload',
      gdlspedlw = 0.0,
      gtsnssn = '',
      gtmsmn = '',
      elcbt = k_elcbt,
      invrn = k_invrn)

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
  i_active <- "ElectricLoadCenter:Distribution"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_battery, i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_wind_turbine <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for Generator:WindTurbine
  #'
  #' @description This function generates an IDF block representing a
  #' wind turbine generator object, It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'wind-turbine--generator',
      avlsn = 'Always On',
      rtrty = 'HorizontalAxisWindTurbine',
      pwrcn = 'FixedSpeedFixedPitch',
      rtrsr = 59,
      rtrdm = 5.6,
      ovrhm = 9,
      nmbob = 3,
      rtdpw = 850,
      rtwsm = 5.5,
      ciwsm = 2.5,
      cowsm = 15,
      frcse = 0.535,
      mxtsr = 5,
      mxmpc = 0.2,
      alawsm = 6.4,
      hflawsm = 20,
      bcam2 = '',
      blddc = '',
      bldlc = '',
      pwcc1 = '',
      pwcc2 = '',
      pwcc3 = '',
      pwcc4 = '',
      pwcc5 = '',
      pwcc6 = '')

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
  i_active <- "Generator:WindTurbine"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_wind_battery <- function(l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Storage:Simple
  #'
  #' @description This function generates an IDF block representing a
  #' photovoltaic storage object, It also uses supplementary
  #' data (`l_sup`) for comments.
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'wind-turbine--battery',
      avlsn = 'Always On',
      zonnm = '',
      rffzhg = 0.0,
      neefc = 0.7,
      nmdee = 0.7,
      mxscj = 2.88E+7,
      mpfdw = 25000,
      mpfcw = 50000,
      isocj = 2.2E+6)

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
  i_active <- "ElectricLoadCenter:Storage:Simple"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_wind_load_centre_generators <- function(i_pwr, l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Generators
  #'
  #' @description This function generates an IDF block representing a
  #' load centre generator using estimated power capacity (`i_pwr`). It also
  #' uses supplementary data (`l_sup`) for comments.
  #'
  #' @param i_pwr: A value indicating power capacity
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(i_pw, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      name = 'wind-inverter-generator-list',
      gnrn = 'wind-turbine--generator',
      gnot = 'Generator:WindTurbine',
      grepow = i_pw,
      gasn = 'Always On',
      grttepr = '')

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
  i_active <- "ElectricLoadCenter:Generators"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_pwr, i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

generate_lzc_wind_load_centre_distribution <- function(i_battery, l_sup = l_idf_blocks){
  #' @title Create IDF block for ElectricLoadCenter:Distribution
  #'
  #' @description This function generates an IDF block representing a
  #' load centre distribution object considering the presence of a battery
  #' (`i_battery`). It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param i_battery: A value indicating the presence of a battery
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(k_st, i_act, l_com=l_sup){

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Adjust presence of a battery
    k_elcbt <- ifelse(k_st, 'DirectCurrentWithInverter', '')
    k_invrn <- ifelse(k_st, 'wind-inverter-simple', '')

    # Define loop parameter list
    l_values <- list(
      name = 'Inverter-wind',
      gnrln = 'wind-inverter-generator-list',
      gnost = 'Baseload',
      gdlspedlw = 0.0,
      gtsnssn = '',
      gtmsmn = '',
      elcbt = k_elcbt,
      invrn = k_invrn)

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
  i_active <- "ElectricLoadCenter:Distribution"

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(i_battery, i_active) %>% unlist() %>% as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c('', l_head, '', l_body)

  # Return the complete IDF block content
  return(txt_idf)
}


# IDF workflows ----

generate_pv_solar_objects <- function(houseIds, buildingGeometry, params, weatherData, includeBattery){
  #' @title Generates PV solar objects for a given set of houses.
  #'
  #' @description This function creates IDF blocks for PV components based on provided parameters.
  #'
  #' @param houseIds A vector of house IDs.
  #' @param buildingGeometry A data frame containing building geometry information.
  #' @param params A list containing logical flags for various parameters.
  #' @param weatherData A data frame containing weather data.
  #' @param includeBattery A logical flag indicating whether to include a battery.
  #' @return A list of IDF blocks representing PV components.

  if(params['z.solarpv'] == TRUE){

    # Enable renewable sources
    dailyLoad <- obtain_daily_lzc(houseIds, weatherData)
    totalLoad <- round(abs(dailyLoad * 10))

    # Determine coordinates of PV panels
    pvSurface <- define_hosting_pv_surface(buildingGeometry)

    # Make idf blocks for surfaces hosting a PV
    idfSolar <- generate_lzc_hosting_pv_surface(pvSurface)
    idfGenerator <- generate_lzc_pv_generator(pvSurface)
    idfPerformance <- generate_lzc_pv_performance()
    idfInverter <- generate_lzc_pv_inverter()
    idfLoadCentreGen <- generate_lzc_pv_load_centre_generators(pvSurface, totalLoad)
    idfLoadCentreDis <- generate_lzc_pv_load_centre_distribution(includeBattery)

    # Add battery if indicated
    if(includeBattery == TRUE){
      idfBattery <- generate_lzc_pv_battery()
    }else{
      idfBattery <- ''
    }

    # Collect PV objects
    idfRes <- c(idfSolar, idfGenerator, idfPerformance, idfInverter,
                idfBattery, idfLoadCentreGen, idfLoadCentreDis, '!')

  }else{

    # Return empty object
    txtHead <- make_idf_block_header("NO PV PANELS ASSIGNED")
    idfBody <- "!"
    idfRes <- c(txtHead, idfBody)

  }

  return(idfRes)
}

generate_wind_turbine_objects <- function(houseIds, params, weatherData, includeBattery){
  #' @title Generates wind turbine objects for a given set of houses.
  #'
  #' @description This function creates IDF blocks for wind turbine components based on provided parameters.
  #'
  #' @param houseIds A vector of house IDs.
  #' @param params A list containing logical flags for various parameters.
  #' @param weatherData A data frame containing weather data.
  #' @param includeBattery A logical flag indicating whether to include a battery.
  #' @return A list of IDF blocks representing wind turbine components.

  if(params['x.turbine'] == TRUE){

    # Enable renewable sources
    dailyLoad <- obtain_daily_lzc(houseIds, weatherData)
    totalLoad <- round(abs(dailyLoad * 10))

    # Load additional templates
    idfWindTurbine <- generate_lzc_wind_turbine()
    idfBatteryWindTurbine <- generate_lzc_wind_battery()
    idfGenWindTurbine <- generate_lzc_wind_load_centre_generators(totalLoad)
    idfDisWindTurbine <- generate_lzc_wind_load_centre_distribution(FALSE)

    # Combine idf objects
    idfBody <- c(idfWindTurbine, idfBatteryWindTurbine,
                 idfGenWindTurbine,
                 idfDisWindTurbine)

    idfRes <- c(idfBody, '!')

  }else{

    # Return empty object
    txtHead <- make_idf_block_header("NO WIND TURBINES ASSIGNED")
    idfBody <- "!"
    idfRes <- c(txtHead, idfBody)

  }

  return(idfRes)
}

generate_water_solar_objects <- function(params){
  #' @title Generates solar water heating objects.
  #'
  #' @description This function creates IDF blocks for solar water heating components based on provided parameters.
  #'
  #' @param params A list containing logical flags for various parameters.
  #' @return A list of IDF blocks representing solar water heating components.

  if(params['y.water'] == TRUE){

    # Load additional templates
    idf_body <- l_idf_lzc$solar_collector
    idf_res <- c(idf_body, '!')

  }else{

    # Return empty object
    txt_head <- make_idf_block_header("NO SOLAR PANELS FOR DHW ASSIGNED")
    idf_body <- "!"
    idf_res <- c(txt_head, idf_body)

  }

  return(idf_res)
}

generate_biomass_store_objects <- function(params){
  #' @title Generates biomass storage objects.
  #'
  #' @description This function creates IDF blocks for biomass storage components based on provided parameters.
  #'
  #' @param params A list containing logical flags for various parameters.
  #' @return A list of IDF blocks representing biomass storage components.

  if(params['w.biogas'] == TRUE){

    # Load additional templates
    idf_body <- l_idf_lzc$biomass
    idf_res <- c(idf_body, '!')

  }else{

    # Return empty object
    txt_head <- make_idf_block_header("NO BIOMASS/BIOGAS ASSIGNED")
    idf_body <- "!"
    idf_res <- c(txt_head, idf_body)

  }

  return(idf_res)
}
