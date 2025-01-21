
# envelope parameters ---------------------------------------------------------

obtain_archetype_parameters <- function(housing_code, reference_data) {
  #' @title Obtains archetype parameters from reference data.
  #'
  #' @description This function extracts relevant parameters from the provided
  #' reference data based on the housing code. Parse attachment conditions:
  #' \enumerate{
  #'   \item{two attachments    ☐ ☐ ☐}
  #'   \item{left attachments    ☐ ☐ -}
  #'   \item{right attachments    - ☐ ☐}
  #'   \item{right attachments   - ☐ - }
  #' }
  #'
  #' @param housing_code Housing code.
  #' @param reference_data Reference data containing complementary and
  #' summarized information.
  #'
  #' @return A data frame with extracted archetype parameters.
  #'

  # Extract relevant data from reference data
  df_com <- subset(reference_data[["complementary"]], V001_HousingCode == housing_code)
  df_sum <- subset(reference_data[["summarised"]], V001_HousingCode == housing_code)

  # Determine building type characteristics
  vCub <- df_sum$.cubdid
  vFlr <- df_com$V574_CuboidFloors
  vRff <- ifelse(df_com$V576_CuboidAttic == TRUE, "W", "N")
  vBsm <- ifelse(df_com$V577_CuboidBasement == TRUE, "W", "N")

  # Determine attachment type
  vAtL <- ifelse(df_com$V580_CuboidAttachmentLeft == TRUE | df_com$V579_CuboidAttachmentRear == TRUE, TRUE, FALSE)
  vAtR <- ifelse(df_com$V578_CuboidAttachmentFront == TRUE | df_com$V581_CuboidAttachmentRight == TRUE, TRUE, FALSE)
  vAtt <- ifelse(vAtL == TRUE & vAtR == TRUE, "B",
           ifelse(vAtL == TRUE & vAtR == FALSE, "L",
           ifelse(vAtL == FALSE & vAtR == TRUE, "R", "N")))

  # Determine building type (flat or house)
  vFlt <- as.character(df_com$V583_CuboidType)
  vFlt <- grepl("flat", vFlt)
  vFlt <- ifelse(vFlt == TRUE, "flat", "house")

  # Calculate average floor area
  vHgt <- df_com$V543_AverageHeight
  vTFA <- df_com$V534_TFAsurvey
  vAre <- vTFA / (vFlr + ifelse(vRff == "W", 1, 0) + ifelse(vBsm == "W", 1, 0))
  vAre <- ifelse(vAre < 30, vTFA, vAre)

  # Create output data frame
  dtaPar <- data.frame(vFlr, vRff, vBsm, vAtt, vFlt, vAre, vHgt, vTFA, vCub)

  return(dtaPar)
}

obtain_archetype_dimensions <- function(idc, df_ref) {
  #' @title Obtain archetype dimensions
  #' @description This function extracts archetype dimensions from
  #' a reference data frame.
  #'
  #' @param idc A character vector specifying the archetype code.
  #' @param df_ref A data frame containing complementary and summarised data.
  #'
  #' @return A data frame containing archetype dimensions.

  # Subset complementary and summarised data
  df_com <- subset(df_ref[["complementary"]], V001_HousingCode == idc)
  df_sum <- subset(df_ref[["summarised"]], V001_HousingCode == idc)

  # Extract basic variables
  varCub <- df_sum$.cubdid
  varFlr <- df_com$V574_CuboidFloors
  varRff <- ifelse(df_com$V576_CuboidAttic == T, "W", "N")
  varBsm <- ifelse(df_com$V577_CuboidBasement == T, "W", "N")
  varTFA <- df_com$V534_TFAsurvey
  varFlt <- as.character(df_com$V583_CuboidType)
  varFlt <- grepl("flat", varFlt)
  varFlt <- ifelse(varFlt == TRUE, "flat", "house")
  tblFlP <- df_com$V541_FlatFloor

  # Calculate storey variables based on flat/house type
  varSty <- ifelse(varFlt == "flat", tblFlP,
    varFlr + ifelse(varRff == "W", 1, 0) + ifelse(varBsm == "W", 1, 0)
  )
  varMty <- ifelse(varFlt == "flat", tblFlP,
    varSty - ifelse(varRff == "W", 1, 0) - 1
  )

  # Extract dimensional data
  df_dim <-
    as.data.frame(subset(df_com, V001_HousingCode == idc,
      select = c(
        V543_AverageHeight, V544_BasementWidth,
        V545_BasementDepth, V546_BasementHeight,
        V547_GroundFloorWidth, V548_GroundFloorDepth,
        V549_GroundFloorHeight, V550_FirstFloorWidth,
        V551_FirstFloorDepth, V552_FirstFloorHeight,
        V553_SecondFloorWidth, V554_SecondFloorDepth,
        V555_SecondFloorHeight, V556_HigherFloorWidth,
        V557_HigherFloorDepth, V558_HigherFloorHeight,
        V559_AtticRoomWidth, V560_AtticRoomDepth,
        V561_AtticRoomHeight
      )
    ))
  colnames(df_dim) <-
    c(
      "AvH", "BBw", "BBd", "BBh", "GGw", "GGd", "GGh", "FFw", "FFd", "FFh",
      "SFw", "SFd", "SFh", "HFw", "HFd", "HFh", "ATw", "ATd", "ATh"
    )

  # Calculate width-to-depth ratio
  w_to_d_ratio <- ifelse(df_dim$GGw > 0 & df_dim$GGd > 0, df_dim$GGw / df_dim$GGd, 1.25)

  # Subset and reformat dimensional data
  df_sub <- data.frame(value = t(df_dim))
  df_sub$variable <- rownames(df_sub)
  df_sub <- df_sub[df_sub$value > 0, ]
  rownames(df_sub) <- NULL

  # Calculate average wall and depth dimensions
  df_dwd <- mean(df_sub$value[grep("\\w{2}w", df_sub$variable)], na.rm = T)
  df_ddh <- mean(df_sub$value[grep("\\w{2}d", df_sub$variable)], na.rm = T)
  df_dim$AvW <- df_dwd
  df_dim$AvD <- df_ddh

  # Perform safety check (for missing data and incompatibilities)
  if (varBsm == "W") {
    df_dim$BBw <- ifelse(df_dim$BBw == 0, df_dim$AvW, df_dim$BBw)
    df_dim$BBd <- ifelse(df_dim$BBd == 0, df_dim$AvD, df_dim$BBd)
    df_dim$BBh <- ifelse(df_dim$BBh == 0, df_dim$AvH, df_dim$BBh)
  }

  # Obtain glazing parameters
  tblGlz <-
    as.data.frame(subset(df_com, V001_HousingCode == idc,
      select = c(
        V562_SurfaceWallArea, V563_GlazingRatio,
        V564_DoubleGlazingRatio, V565_Doors, V566_DoorType
      )
    ))
  colnames(tblGlz) <- c("SfA", "GzR", "GzD", "DrN", "DrT")
  tblGlz$GzA <- tblGlz$SfA * tblGlz$GzR

  # Extract values related to geometric parameters
  SFA <- varTFA / varSty
  CuD <- sqrt(SFA / w_to_d_ratio) # : cuboid block width
  CuW <- CuD * w_to_d_ratio # : cuboid block depth

  BsH <- ifelse(df_dim$BBh < 1, 0, -df_dim$BBh) # : basement height
  GfH <- df_dim$GGh # : ground floor height
  AtH <- df_dim$ATh # : room-in-roof height
  GzA <- tblGlz$GzA # : glazing area
  GzR <- tblGlz$GzR # : glazing ratio
  DrA <- 1.81 * tblGlz$DrN # : door area
  XfH <- df_dim[, grep("\\wFh", colnames(df_dim))] # : upper floors checking
  XfH[XfH == 0] <- NA # : upper floors validation
  XfH <- rowMeans(XfH, na.rm = T) # : upper floors heights
  Nfl <- varSty # : number of storeys
  Ufl <- varMty # : number of upper floors

  # Assemble dimension parameters
  dtaPar <- data.frame(
    varCub, varTFA, varFlt, Nfl, Ufl,
    CuW, CuD, BsH, GfH, XfH, AtH, GzA, GzR, DrA
  )
  # Handle missing or incompatible values
  dtaPar[is.na(dtaPar)] <- 0

  return(dtaPar)
}

obtain_archetype_level <- function(num_floors, basement_presence, attic_presence, reference_data) {
  #' @title Obtain archetype level information
  #'
  #' @description This function determines the effective storey levels
  #' for an archetype based on the presence of a basement, attic,
  #' and number of floors.
  #'
  #' @param num_floors An integer specifying the number of floors in the archetype.
  #' @param basement_presence A character indicating the presence of a basement
  #' @param attic_presence A character indicating the presence of an attic
  #' @param reference_data A data frame containing a reference floor position variable
  #'
  #' @return A list containing information about the effective storey levels.

  # Define storey labels
  lbl_sto <- c("GF", "1F", "2F", "3F", "A", "B")

  # Adjust labels based on presence of basement and attic
  lbl_sto[6] <- ifelse(basement_presence == "W", lbl_sto[6], NA)
  lbl_sto[5] <- ifelse(attic_presence == "W", lbl_sto[5], NA)

  # Adjust labels based on number of floors
  lbl_sto[4] <- ifelse(num_floors >= 4, lbl_sto[4], NA)
  lbl_sto[3] <- ifelse(num_floors >= 3, lbl_sto[3], NA)
  lbl_sto[2] <- ifelse(num_floors >= 2, lbl_sto[2], NA)
  lbl_sto[1] <- ifelse(num_floors >= 1, lbl_sto[1], NA)

  # Remove missing labels
  lbl_sto <- lbl_sto[!is.na(lbl_sto)]

  # Reorder basement if present
  if ("B" %in% lbl_sto) lbl_sto <- c("B", lbl_sto[!(lbl_sto %in% "B")])

  # Determine number of storeys
  n_sto <- length(lbl_sto)

  # Assign floor position based on reference data
  varFflr <- as.character(reference_data$V542_FloorPosition)
  varLoc <- switch(varFflr,
    "House/Bungalow" = "GF",
    "Top Floor Flat" = lbl_sto[n_sto],
    "Mid Floor Flat" = lbl_sto[n_sto - 1],
    "Ground floor flat" = lbl_sto[1],
    "Basement Flat" = "B",
    "Unknown" = "GF"
  )
  varLoc <- ifelse(length(varLoc) == 1 & varLoc == "B", "GF", varLoc)

  # Separate main and secondary storeys
  varMain <- varLoc
  varSec <- lbl_sto[!(lbl_sto %in% varMain)]
  varSec <- varSec[!is.na(varSec)]
  varLoc <- c(varMain, varSec)

  # Create list of results
  l_res <- list(
    total = lbl_sto,
    effective = varLoc,
    main = varMain,
    secondary = varSec, size = n_sto
  )

  return(l_res)
}

obtain_list_zone_summary <- function(archetype_info, reference_data, zone_data) {
  #' @title Obtain a list of zone summaries
  #'
  #' @description This function processes information about an archetype
  #' and its zones, creating a list containing various zone-related data.
  #'
  #' @param archetype_info A data frame containing archetype
  #' information (vFlr, vBsm, vRff, vFlt).
  #' @param reference_data A reference data frame used by obtain_archetype_level.
  #' @param zone_data A data frame containing zone
  #' information (zone, room_function, area__m2, volume__m3, glazed).
  #'
  #' @return A list containing:
  #'   * all_zones: All zones in the data frame.
  #'   * gather: Zones classified as living or dining rooms.
  #'   * rest: Zones classified as bedrooms.
  #'   * cooking: Zones classified as kitchens.
  #'   * hygiene: Zones classified as bathrooms or water-related spaces.
  #'   * other: Zones not classified in the previous categories.
  #'   * size: Total number of zones.

  # Determine archetype level
  l_res <- obtain_archetype_level(
    as.integer(archetype_info$vFlr),
    as.character(archetype_info$vBsm),
    as.character(archetype_info$vRff),
    reference_data
  )

  # Handle flats with no secondary zones
  if (as.character(archetype_info$vFlt) == "flat") {
    l_res$secondary <- character(0)
  }

  # Extract zone information
  k_all <- zone_data %>%
    dplyr::select(zone) %>%
    unlist() %>%
    as.character()

  # Identify zone categories
  k_gather <- zone_data %>%
    dplyr::filter(grepl("^Living Room|^Dining Room", room_function)) %>%
    dplyr::select(zone) %>%
    unlist() %>%
    as.character()

  k_rest <- zone_data %>%
    dplyr::filter(grepl("^Bed", room_function)) %>%
    dplyr::select(zone) %>%
    unlist() %>%
    as.character()

  k_cook <- zone_data %>%
    dplyr::filter(grepl("^Kit", room_function)) %>%
    dplyr::select(zone) %>%
    unlist() %>%
    as.character()

  k_bath <- zone_data %>%
    dplyr::filter(grepl("^Bath|^Water|^Boud", room_function)) %>%
    dplyr::select(zone) %>%
    unlist() %>%
    as.character()

  # Other zones
  k_secs <- k_all[!k_all %in% c(k_gather, k_rest, k_cook, k_bath)]

  # Create zone summaries
  l_res$all_zones <- zone_data %>% dplyr::select(-area__m2, -volume__m3, -glazed)
  l_res$gather <- zone_data %>%
    dplyr::select(-area__m2, -volume__m3, -glazed) %>%
    dplyr::filter(zone %in% k_gather)
  l_res$rest <- zone_data %>%
    dplyr::select(-area__m2, -volume__m3, -glazed) %>%
    dplyr::filter(zone %in% k_rest)
  l_res$cooking <- zone_data %>%
    dplyr::select(-area__m2, -volume__m3, -glazed) %>%
    dplyr::filter(zone %in% k_cook)
  l_res$hygiene <- zone_data %>%
    dplyr::select(-area__m2, -volume__m3, -glazed) %>%
    dplyr::filter(zone %in% k_bath)
  l_res$other <- zone_data %>%
    dplyr::select(-area__m2, -volume__m3, -glazed) %>%
    dplyr::filter(zone %in% k_secs)

  # Add total number of zones
  l_res$size <- length(k_all)

  return(l_res)
}


# heat loss parameters --------------------------------------------------------

obtain_hlp_floors_bbgf <- function(idc, l_inp) {
  #' @title Obtain U-values for basement and ground floors
  #'
  #' @description This function calculates the U-values (thermal transmittance)
  #' for basement and ground floors in a building based on various construction
  #' parameters and reference data. Based on SAP S5.4 U-values of floors
  #' next to the ground
  #'
  #' @param idc A character string specifying the archetype code.
  #' @param l_inp A list containing three data frames:
  #'   * The first data frame contains information about the
  #'   building geometry (D030_WallThickness, D059_BasementFloorHeatLossArea,
  #'   D113_BasementFloorExposedPerimeter, D060_GroundFloorHeatLossArea,
  #'   D114_GroundFloorExposedPerimeter).
  #'   * The second data frame contains information about the
  #'   floor construction (D074_BasementFloorConstructionType,
  #'   D075_GroundFloorConstructionType).
  #'   * The third data frame contains information about the
  #'   reference region (D003_Region).
  #'
  #' @return A data frame containing U-values for basement and ground floors
  #' (`BB` and `GF` columns) along with other calculated parameters.

  # Reference data for U-values and insulation
  t_upar <- s_chm_reference$FloorUParameters
  t_insl <- s_chm_reference$BasementGroundFloorInsulation

  # Format U-value parameter table
  rownames(t_upar) <- t_upar$name
  t_upar$name <- t_upar$units <- NULL
  rownames(t_upar) <- gsub("[[:punct:]]", "", rownames(t_upar))
  rownames(t_upar) <- gsub("[[:space:]]", ".", rownames(t_upar))
  t_upar <- as.data.frame(t(t_upar))

  # Extract floor specific properties
  v_sg <- t_upar$Soil.type.clay
  v_rsi <- t_upar$Rsi
  v_rse <- t_upar$Rse
  v_k <- t_upar$Floor.thermal.conductivity
  v_tr <- t_upar$Thermal.resistance.of.floor.deck
  v_h <- t_upar$Height.above.external.ground.level
  v_v <- t_upar$Average.wind.speed.at.10m.height
  v_fw <- t_upar$Wind.shielding.factor
  v_e <- t_upar$Ventilation.openings.per.m.exposed.perimeter
  v_Uw <- t_upar$Uvalue.of.walls.to.underfloor.space

  # Extract construction data from input list
  t_Gmt <- l_inp[[1]][l_inp[[1]]$V001_HousingCode == idc, ]
  t_Lss <- l_inp[[2]][l_inp[[2]]$V001_HousingCode == idc, ]
  t_Inf <- l_inp[[3]][l_inp[[3]]$V001_HousingCode == idc, ]

  # Reference region
  t_reg <- t_Inf$D003_Region

  # Basement and ground floor construction types and exposed perimeters
  v_bcs <- t_Lss$D074_BasementFloorConstructionType
  v_bhl <- t_Gmt$D059_BasementFloorHeatLossArea
  v_bxp <- t_Gmt$D113_BasementFloorExposedPerimeter

  v_gcs <- t_Lss$D075_GroundFloorConstructionType
  v_gxp <- t_Gmt$D114_GroundFloorExposedPerimeter
  v_ghl <- t_Gmt$D060_GroundFloorHeatLossArea

  # Correction for missing data (assuming solid floor)
  if ((v_gcs == 0 & v_bcs == 0) | (is.na(v_gcs) & is.na(v_bcs))) {
    v_gcs <- "solid"
    v_ghl <- 86.41208
    v_gxp <- 36.80228
  }

  # Calculate basement wall depth and thermal bridging factor
  v_bwd <- ifelse(v_bhl == 0, 0, t_Gmt$D030_WallThickness)
  v_bsb <- ifelse(v_bwd == 0, 0, 2 * v_bhl / v_bxp)

  v_gwd <- ifelse(v_ghl == 0, 0, t_Gmt$D030_WallThickness)
  v_gsb <- ifelse(v_gwd == 0, 0, ifelse(v_gxp == 0, v_ghl, 2 * v_ghl / v_gxp))

  # Insulation for basement and ground floor (considering reference region)
  v_bin <-
    ifelse(is.na(v_bcs) | v_bcs == "-", 0.0,
      ifelse(as.integer(t_reg) < 10,
        t_insl[as.integer(t_reg), 1], t_insl[as.integer(t_reg), 2]
      )
    )

  v_gin <-
    ifelse(is.na(v_gcs) | v_gcs == "-", 0.0,
      ifelse(as.integer(t_reg) < 10,
        t_insl[as.integer(t_reg), 1], t_insl[as.integer(t_reg), 2]
      )
    )

  # Prepare data frame for intermediate calculations
  df_par <- data.frame(names = c(
    "type", "netArea", "thickness", "B",
    "dins", "Rf", "dt", "Usolid", "dg", "Ug", "Ux",
    "Ususpended", "Utotal"
  ), BB = 0, GF = 0)
  rownames(df_par) <- df_par$names
  df_par$names <- NULL
  df_par <- as.data.frame(t(df_par))

  # Assign values to data frame columns
  df_par$type <- c(v_bcs, v_gcs)
  df_par$type <- ifelse(is.na(df_par$type), "-", df_par$type) # Replace NA with "-"
  df_par$netArea <- c(v_bhl, v_ghl)
  df_par$thickness <- c(v_bwd, v_gwd)
  df_par$B <- c(v_bsb, v_gsb)
  df_par$dins <- c(v_bin, v_gin)

  # Calculate thermal resistance (Rf) based on insulation thickness and conductivity
  df_par$Rf <- ifelse(df_par$thickness == 0, 0, 0.001 * df_par$dins / v_k)

  # Calculate total thermal resistance (dt) considering soil properties
  df_par$dt <- ifelse(df_par$thickness == 0, 0, df_par$thickness +
    v_sg * (v_rsi + df_par$Rf + v_rse))

  # Calculate U-value for solid construction
  df_par$Usolid <- ifelse(df_par$thickness == 0, 0,
    ifelse(df_par$dt < df_par$B, 2 * v_sg *
      log(pi * df_par$B / df_par$dt + 1) / (pi * df_par$B + df_par$dt),
    v_sg / (0.457 * df_par$B + df_par$dt)
    )
  )

  # Calculate thermal resistance excluding ground (dg)
  df_par$dg <- ifelse(df_par$thickness == 0, 0,
    df_par$thickness + v_sg *
      (v_rsi + v_rse)
  )

  # Calculate U-value for ground heat transfer (Ug)
  df_par$Ug <- ifelse(df_par$thickness == 0, 0,
    2 * v_sg * log(pi * df_par$B / df_par$dg + 1) /
      (pi * df_par$B + df_par$dg)
  )

  # Calculate U-value for ground heat thermal transmittance (Ux) general
  df_par$Ux <- ifelse(df_par$thickness == 0, 0,
    2 * v_h * v_Uw / df_par$B + 1450 * v_e *
      v_v * v_fw / df_par$B
  )

  # Calculate U-value for ground heat thermal transmittance (Usus) for suspended floors
  df_par$Ususpended <- ifelse(df_par$thickness == 0, 0,
    1 / (2 * v_rsi + v_tr + 0.2 + 1 / (df_par$Ug + df_par$Ux))
  )

  # Calculate U-value for ground heat thermal transmittance (Ubs) for basements
  v_u_bs <- ifelse(is.na(v_bcs) | v_bcs == "-", 0.0,
    ifelse(v_bcs == "solid", df_par$Usolid[rownames(df_par) == "BB"],
      df_par$Ususpended[rownames(df_par) == "BB"]
    )
  )

  # Calculate U-value for ground heat thermal transmittance (Ugf)
  v_u_gf <- ifelse(is.na(v_gcs) | v_gcs == "-", 0.0,
    ifelse(v_gcs == "solid", df_par$Usolid[rownames(df_par) == "GF"],
      df_par$Ususpended[rownames(df_par) == "GF"]
    )
  )

  # Calculate total equivalent U-value for ground (Utotal)
  df_par$Utotal <- c(v_u_bs, v_u_gf)

  return(df_par)
}

obtain_hlp_openings <- function(idc, l_inp) {
  #' @title Obtain U-values and areas for building openings

  #' @description This function calculates the U-values and net areas for
  #' various building openings (windows, doors, and roof windows) based on
  #' construction details and reference data.

  #' @param idc A character string specifying the archetype code.
  #' @param l_inp A list containing three data frames:
  #'   * The first data frame contains information about the building geometry
  #'     (D057_DoorArea, D067_Windows1Area, D069_Windows2Area, D049_RoofWindowArea).
  #'   * The second data frame contains information about the opening types
  #'     (D068_Windows1Type, D070_Windows2Type, D058_DoorUValue,
  #'     D071_Windows1FrameType, D072_Windows2FrameType).
  #'   * The third data frame contains information about the glazing ratio
  #'     (V564_DoubleGlazingRatio) and number of doors (V565_Doors).

  #' @return A data frame containing U-values and net areas for doors,
  #'   windows 1 and 2, and roof windows (`Door`, `WinA`, `WinB`, `WinRoof`
  #'   columns) along with opening type and frame type.

  # Reference data for window properties
  t_type <- s_chm_reference$Window
  t_frame <- s_chm_reference$WindowFrameType
  t_orien <- s_chm_reference$WindowOrientation
  t_overs <- s_chm_reference$WindowOvershading
  t_trans <- s_chm_reference$WindowTransmittance

  # Extract building geometry data from input list
  tblGmt <- l_inp[[1]][l_inp[[1]]$V001_HousingCode == idc, ]
  tblLss <- l_inp[[2]][l_inp[[2]]$V001_HousingCode == idc, ]
  tblCmp <- l_inp[[3]][l_inp[[3]]$V001_HousingCode == idc, ]

  # Exterior and interior window data
  v_typA <- tblLss$D068_Windows1Type
  v_typB <- tblLss$D070_Windows2Type
  v_frmA <- tblLss$D071_Windows1FrameType
  v_frmB <- tblLss$D072_Windows2FrameType
  v_dblR <- tblCmp$V564_DoubleGlazingRatio
  v_srfA <- tblGmt$D067_Windows1Area
  v_srfB <- tblGmt$D069_Windows2Area

  # Roof window data
  v_Drno <- tblCmp$V565_Doors
  v_Drarea <- tblGmt$D057_DoorArea
  v_DrU <- ifelse(v_Drarea == 0 | is.na(v_Drarea), 0, tblLss$D058_DoorUValue)

  # .. roof window
  v_roof <- tblLss$H048_RoofWindowType
  v_frrf <- tblLss$H050_RoofWindowFrame
  v_srfR <- tblLss$H049_RoofWindowArea

  # Process reference catalogue data
  t_pars <-
    t_type[1:8, c("Type", "Uv_Wood_Eff", "Uv_Metal_Eff", "Uv_uPVC_Eff")]
  t_pars$Type <- tolower(gsub(" \\(air filled\\)", "", t_pars$Type))

  t_trns <- t_trans[1:8, ]
  t_trns$Window.Transmittance <-
    tolower(gsub(" \\(air filled\\)", "", t_trns$Window.Transmittance))

  t_Rfpars <-
    t_type[1:8, c(
      "Type", "Roof_Uv_Wood_Eff",
      "Roof_Uv_Metal_Eff", "Roof_Uv_uPVC_Eff"
    )]
  t_Rfpars$Type <- tolower(gsub(" \\(air filled\\)", "", t_pars$Type))

  # Map frame material codes to frame material indices
  v_fA <- ifelse(grepl("upvc", tolower(v_frmA)), 3,
    ifelse(grepl("metal", tolower(v_frmA)), 2, 1)
  )
  v_fB <- ifelse(grepl("upvc", tolower(v_frmB)), 3,
    ifelse(grepl("metal", tolower(v_frmB)), 2, 1)
  )
  v_fR <- ifelse(grepl("upvc", tolower(v_frrf)), 3,
    ifelse(grepl("metal", tolower(v_frrf)), 2, 1)
  )

  # Map window type codes to glazing levels (single, double, triple)
  v_aU <- ifelse(grepl("triple", v_typA), 6,
    ifelse(grepl("double", v_typA), 2, 1)
  )
  v_bU <- ifelse(grepl("triple", v_typB), 6,
    ifelse(grepl("double", v_typB), 2, 1)
  )
  v_rU <- ifelse(grepl("triple", v_roof), 6,
    ifelse(grepl("double", v_roof), 2, 1)
  )

  # Look up U-values based on frame material and glazing level
  v_aU <- t_pars[v_aU, v_fA + 1]
  v_bU <- t_pars[v_bU, v_fB + 1]
  v_rU <- t_Rfpars[v_rU, v_fR + 1]

  # Generate table
  df_par <- data.frame(
    names = c("type", "frame", "netArea", "U", "AxU"),
    Door = 0, WinA = 0, WinB = 0, WinRoof = 0
  )
  rownames(df_par) <- df_par$names
  df_par$names <- NULL
  df_par <- as.data.frame(t(df_par))

  # Correct type and frame
  df_par$type <- c("conventional", v_typA, v_typB, v_roof)
  df_par$frame <- c("-", v_frmA, v_frmB, v_frrf)

  # Estimate glazing area (A)
  df_par$netArea <-
    c(
      ifelse(is.na(v_Drarea) | v_Drarea < 0.1, 0, v_Drarea),
      ifelse(is.na(v_srfA) | v_srfA < 0.1, 0, v_srfA),
      ifelse(is.na(v_srfB) | v_srfB < 0.1, 0, v_srfB),
      ifelse(is.na(v_srfR) | v_srfR < 0.1, 0, v_srfR)
    )

  # Estimate thermal transmitance (U)
  df_par$U <-
    c(
      ifelse(is.na(v_Drarea) | v_Drarea < 0.1, 0, v_DrU),
      ifelse(is.na(v_srfA) | v_srfA < 0.1, 0, v_aU),
      ifelse(is.na(v_srfB) | v_srfB < 0.1, 0, v_bU),
      ifelse(is.na(v_srfR) | v_srfR < 0.1, 0, v_rU)
    )

  # Estimate thermal conductance (UxA)
  df_par$AxU <- as.numeric(df_par$netArea) * as.numeric(df_par$U)

  return(df_par)
}

obtain_hlp_opaques <- function(idc, l_inp) {
  #' @title Obtain heat loss parameters for opaque building elements
  #'
  #' @description This function calculates heat loss areas, U-values,
  #' thermal conductivities, and area-weighted thermal transmittance
  #' and conductance for various opaque building elements
  #' (basement floor, ground floor, exposed floor, basement wall,
  #' external wall, semi-exposed wall, roof, and room-in-roof) based on
  #' construction details and reference data.
  #'
  #' @param idc A character string specifying the archetype code.
  #' @param l_inp A list containing five data frames:
  #'   * The first data frame contains information about the building geometry
  #'     (D036_GroundFloorArea, D040_RoomInRoofArea, D059_BasementFloorHeatLossArea,
  #'     D061_RoofArea, D064_BasementWallArea, D065_ExternalWallArea,
  #'     D121_SemiExposedArea).
  #'   * The second data frame contains information about the construction types
  #'     of specific elements (D074_BasementFloorConstructionType,
  #'     D075_GroundFloorConstructionType, D076_BasementWallConstruction,
  #'     D077_ExternalWallConstruction, D079_RoofConstruction,
  #'     D080_LoftInsulationThickness, D081_RoomInRoofConstruction,
  #'     H058_ExposedFloorConstruction, H059_ExposedFloorHeatLossArea).
  #'   * The third data frame contains information about user-defined U-values
  #'     (Utotal).
  #'   * The fourth data frame contains information on the dwelling age
  #'     (D001_DwellingAge).
  #'   * The fifth data frame is (potentially) empty.
  #'
  #' @return A data frame containing heat loss areas, U-values, thermal
  #'   conductivities, area-weighted thermal transmittance (AxU), and conductance
  #'   (AxK) for various opaque building elements (`BaseFloor`, `GrndFloor`,
  #'   `ExpdFloor`, `BaseWall`, `ExtrWall`, `SemiWall`, `Roof`, `RoomInRoof`
  #'   columns) along with construction type and data source.


  # Extract data from input list for each element
  tblGmt <- l_inp[[1]] %>% dplyr::filter(V001_HousingCode == idc)
  tblLss <- l_inp[[2]] %>% dplyr::filter(V001_HousingCode == idc)
  tblCmp <- l_inp[[3]] %>% dplyr::filter(V001_HousingCode == idc)
  tblInf <- l_inp[[4]] %>% dplyr::filter(V001_HousingCode == idc)
  tblUsr <- l_inp[[5]]

  # Load reference data for materials and constructions
  df_ref <- s_chm_reference

  # Extract material properties from reference data
  t_materials <- df_ref[["_MatList"]] %>%
    dplyr::mutate(Name = tolower(gsub(" ", "_", Name)))
  t_floor.type <- df_ref[["GroundFloorConstruction"]]
  t_exposedfloor.type <- df_ref[["ExposedFloorConstruction"]]
  t_exposedfloor.u <- df_ref[["ExposedFloorUvalue"]]
  t_roof.ins <- df_ref[["Roof"]]
  t_roof.u <- df_ref[["RoofU"]]
  t_wall.type <- df_ref[["WallType"]]

  t_wall.u <-
    t_wall.type[, grep("wllUv_Wm2K_EngWal", colnames(t_wall.type))]
  t_semi.u <-
    t_wall.type[, grep("wllUvSem_Wm2K_EngWal", colnames(t_wall.type))]
  t_wall.k <-
    t_wall.type[, grep(
      "(Wall.Construction|Wall.K.value..kJ.m2K)",
      colnames(t_wall.type)
    )]


  # Basement floor
  v_bcs <- tblLss$D074_BasementFloorConstructionType
  v_bcs <- ifelse(is.na(v_bcs), "-", v_bcs)
  v_bcx <- "switch"
  v_bhl <- tblGmt$D059_BasementFloorHeatLossArea
  v_buv <- tblUsr["BB", "Utotal"]
  bcs_values <- c(timber = 1, suspended = 2, solid = 3, "-" = 4)
  v_bcn <- bcs_values[v_bcs]
  v_bkv <- if (is.na(v_bhl) | v_bhl < 0.01) {
    0
  } else {
    t_floor.type$Basement.Ground.Floor.K.Value..kJ.m2K.[v_bcn]
  }

  # Consider ground floor type
  v_gcs <- tblLss$D075_GroundFloorConstructionType
  v_gcs <- ifelse(is.na(v_gcs), "-", v_gcs)
  v_gcx <- "switch"
  v_ghl <- tblGmt$D036_GroundFloorArea
  v_guv <- tblUsr["GF", "Utotal"]
  gcs_values <- c(timber = 1, suspended = 2, solid = 3, "-" = 4)
  v_gcn <- gcs_values[v_gcs]
  v_gkv <- if (is.na(v_ghl) | v_ghl < 0.01) {
    0
  } else {
    t_floor.type$Basement.Ground.Floor.K.Value..kJ.m2K.[v_gcn]
  }

  # Consider exposed  floor type
  v_exs <- tblLss$H058_ExposedFloorConstruction
  v_exs <- ifelse(is.na(v_exs), "-", v_exs)
  v_exx <- "tables"
  exs_values <- c("as-built" = 1, "retro-fit" = 2, "-" = 3)
  v_exn <- exs_values[v_exs]

  # Consider exposed  floor area
  v_exl <- tblLss$H059_ExposedFloorHeatLossArea
  v_exa <- tblInf$D001_DwellingAge
  v_exu <- ifelse(v_exn > 2, 0, t_exposedfloor.u[v_exa + 1, v_exn - 1])
  v_exk <-
    ifelse(v_exn > 2, 0,
      t_exposedfloor.type$Exposed.Floor.K.value..kJ.m2K.[v_exn]
    )

  # Consider basement floor type
  v_bwn <- tblGmt$D076_BasementWallConstruction
  v_bwn <- ifelse(is.na(v_bwn), "-", v_bwn)
  v_bwx <- "tables"
  v_bws <-
    ifelse(v_bwn == 16, "-",
      as.character(t_wall.type$Wall.Construction.Type[v_bwn])
    )
  v_bwl <- tblGmt$D064_BasementWallArea
  v_bwu <- t_wall.u[v_bwn, v_exa]
  v_bwk <- t_wall.k[v_bwn, 2]

  # Consider external wall type
  v_ewn <- tblGmt$D077_ExternalWallConstruction
  v_ewn <- ifelse(is.na(v_ewn), "-", v_ewn)
  v_ewx <- "tables"
  v_ews <-
    ifelse(v_ewn == 16, "-",
      as.character(t_wall.type$Wall.Construction.Type[v_ewn])
    )
  v_ewl <- tblGmt$D065_ExternalWallArea
  v_ewu <- t_wall.u[v_ewn, v_exa]
  v_ewk <- t_wall.k[v_ewn, 2]

  # Consider semi-external wall type
  v_swn <- tblGmt$D122_SemiExposedConstruction
  v_swn <- ifelse(is.na(v_swn), "-", v_swn)
  v_swx <- "tables"
  v_sws <-
    ifelse(v_swn == 16, "-",
      as.character(t_wall.type$Wall.Construction.Type[v_swn])
    )
  v_swl <- tblGmt$D121_SemiExposedArea
  v_swu <- t_wall.u[v_swn, v_exa]
  v_swk <- t_wall.k[v_swn, 2]

  # Consider room-in-roof wall type
  v_rrs <- tblLss$D081_RoomInRoofConstruction
  v_rrs <- ifelse(is.na(v_rrs), "-", v_rrs)
  v_rrx <- "switch"
  v_rrl <- tblGmt$D040_RoomInRoofArea
  v_rrn <-
    ifelse(v_rrs == "-", "-",
      ifelse(grepl("flat", v_rrs), 3,
        ifelse(grepl("thatch", v_rrs), 2, 1)
      )
    )

  # Consider roof wall type
  v_rfs <- tblLss$D079_RoofConstruction
  v_rfs <- ifelse(is.na(v_rfs), "-", v_rfs)
  v_rfx <- "switch"
  v_rfl <- tblGmt$D061_RoofArea
  v_rfn <-
    ifelse(v_rfs == "-", "-",
      ifelse(grepl("flat", v_rfs), 3,
        ifelse(grepl("thatch", v_rfs), 2, 1)
      )
    )

  # Consider loft insulation
  v_rfi <- tblLss$D080_LoftInsulationThickness
  v_rfi <- ifelse(is.na(v_rfi), "-", v_rfi)
  v_rfin <-
    ifelse(is.na(v_rfi) | v_rfi < 0, NA,
      ifelse(v_rfi > 300, 11,
        grep(v_rfi, as.character(t_roof.ins$RoofValue))
      )
    )

  # Estimate thermal properties
  if (v_rfn == "-") {
    v_rfu <- 0
  } else {
    if (v_rfin > 0 & v_rfin < 11) {
      v_rf.row <- v_rfin
      v_rf.col <- v_rfn
    } else {
      v_rf.row <- v_exa
      v_rf.col <- v_rfn + 2
    }
    v_rfu <- t_roof.u[v_rf.row, v_rf.col]
  }

  if (v_rrn == "-") {
    v_rru <- 0
  } else {
    if (v_rfin > 0 & v_rfin < 11) {
      v_rf.row <- v_rfin
      v_rf.col <- v_rrn
    } else {
      v_rf.row <- v_exa
      v_rf.col <- v_rrn + 5
    }
    v_rru <- t_roof.u[v_rf.row, v_rf.col]
  }

  v_rfk <- ifelse(v_rfn == "-" & v_rfu == "-", 0, 9)
  v_rrk <- ifelse(v_rrn == "-" & v_rru == "-", 0, 9)

  v_rfl <- ifelse(is.na(v_rfl) & !is.na(v_rrl), v_rrl, v_rfl)
  v_rfl <- ifelse((v_rfl == 0) & (v_rrl == 0) & (v_ghl > 0), v_ghl, v_rfl)


  # Generate template table
  df_par <- data.frame(
    names = c("type", "netArea", "U", "AxU", "K", "AxK"),
    BaseFloor = 0, GrndFloor = 0, ExpdFloor = 0, BaseWall = 0,
    ExtrWall = 0, SemiWall = 0, Roof = 0, RoomInRoof = 0
  )
  rownames(df_par) <- df_par$names
  df_par$names <- NULL
  df_par <- as.data.frame(t(df_par))

  # Fill summary table
  df_par$type <- c(v_bcs, v_gcs, v_exs, v_bws, v_ews, v_sws, v_rfs, v_rrs)
  df_par$source <- c(v_bcx, v_gcx, v_exx, v_bwx, v_ewx, v_swx, v_rfx, v_rrx)
  df_par$netArea <- c(v_bhl, v_ghl, v_exl, v_bwl, v_ewl, v_swl, v_rfl, v_rrl)
  df_par$U <- c(v_buv, v_guv, v_exu, v_bwu, v_ewu, v_swu, v_rfu, v_rru)
  df_par$AxU <- as.numeric(df_par$netArea) * as.numeric(df_par$U)
  df_par$K <- c(v_bkv, v_gkv, v_exk, v_bwk, v_ewk, v_swk, v_rfk, v_rrk)
  df_par$AxK <- as.numeric(df_par$netArea) * as.numeric(df_par$K)

  return(df_par)
}

obtain_hlp_internals <- function(idc, l_inp) {
  #' @title Obtain internal material properties
  #'
  #' @description This function retrieves internal material properties
  #' from various data sources based on a housing code.
  #'
  #' @param idc Housing code
  #' @param l_inp List containing data tables:
  #'   - Element 1: Gmt table
  #'   - Element 2: Lss table
  #'   - Element 3: Cmp table (unused)
  #'   - Element 4: Inf table
  #'   - Element 5: Usr table (unused)
  #'
  #' @return Data frame with internal material properties for party wall, party
  #' floor, party ceiling, internal wall, internal floor, and internal ceiling.

  # Access data from reference table
  df_ref <- s_chm_reference
  t_materials <- df_ref[["_MatList"]]
  t_partywall.ku <- df_ref[["PartyWallConstruction"]]
  t_partyfloor.ku <- df_ref[["PartyFloor"]]
  t_partyceiling.ku <- df_ref[["PartyCeiling"]]
  t_internalwall.ku <- df_ref[["InternalConstructionK"]]

  # Filter data tables by housing code
  tblGmt <- l_inp[[1]][l_inp[[1]]$V001_HousingCode == idc, ]
  tblLss <- l_inp[[2]][l_inp[[2]]$V001_HousingCode == idc, ]
  tblCmp <- l_inp[[3]][l_inp[[3]]$V001_HousingCode == idc, ]
  tblInf <- l_inp[[4]][l_inp[[4]]$V001_HousingCode == idc, ]
  tblUsr <- l_inp[[5]]

  # Extract party wall properties
  v_pwt <- tblLss$H072_PartyWallConstruction
  v_pwn <- ifelse(grepl("^(D|d)ense", v_pwt), 1,
    ifelse(grepl("^(S|s)ingle", v_pwt), 2,
      ifelse(grepl("^(P|p)laster ", v_pwt), 3,
        ifelse(grepl("^(P|p)lasterboard", v_pwt), 4,
          ifelse(grepl("^(D|d)ouble", v_pwt), 5,
            ifelse(grepl("^(S|s)teel", v_pwt), 6, 7)
          )
        )
      )
    )
  )
  v_pwl <- tblGmt$D073_PartyWallArea
  v_pwu <- ifelse(is.na(v_pwl) | v_pwl < 0.01,
    0, t_partywall.ku$U.value[v_pwn]
  )
  v_pwa <- ifelse(tblInf$D001_DwellingAge > 9,
    v_pwl * v_pwu, v_pwl * v_pwu * 0.25
  )
  v_pwk <- ifelse(is.na(v_pwl) | v_pwl < 0.01,
    0, t_partywall.ku$K.value[v_pwn]
  )

  # Extract party floor properties
  v_pft <- tblLss$H074_PartyFloorConstruction
  v_pfn <- ifelse(grepl("(P|p)lanks", v_pft), 1,
    ifelse(grepl("^(C|c)oncrete", v_pft), 2,
      ifelse(grepl("(I|i)nsulation", v_pft), 3,
        ifelse(grepl("(R|r)ubber", v_pft), 4,
          ifelse(grepl("(P|p)rofiled", v_pft), 5,
            ifelse(grepl("^(T|t)imber", v_pft), 6, 7)
          )
        )
      )
    )
  )

  # Parse party floor area
  v_pfl <- tblGmt$D062_PartyFloorArea
  v_pfk <- ifelse(is.na(v_pfl) | v_pfl < 0.01,
    0, t_partyfloor.ku$value[v_pfn]
  )

  # Parse party ceiling construction
  v_pct <- tblLss$H076_PartyCeilingConstruction
  v_pcn <- ifelse(grepl("(P|p)lasterboard", v_pct), 1, 2)
  v_pcl <- tblGmt$D063_PartyCeilingArea
  v_pck <- ifelse(is.na(v_pcl) | v_pcl < 0.01,
    0, t_partyceiling.ku$value[v_pcn]
  )

  # Parse internal wall construction
  v_iwt <- tblLss$H078_InternalWallConstruction
  v_iwn <- ifelse(grepl("^(P|p)lasterboard", v_iwt), 1,
    ifelse(grepl("(P|p)laster", v_iwt), 2,
      ifelse(grepl("(D|d)abs", v_iwt), 3, 4)
    )
  )

  # Parse internal wall area
  v_iwl <- tblLss$H079_InternalWallArea
  v_iwk <- ifelse(is.na(v_iwl) | v_iwl < 0.01,
    0, t_internalwall.ku$K.value[v_iwn]
  )

  # Parse internal floor construction
  v_ift <- tblLss$H080_InternalFloorConstruction
  v_ifn <- ifelse(grepl("(P|p)lanks", v_ift), 1,
    ifelse(grepl("^(C|c)oncrete", v_ift), 2,
      ifelse(grepl("(I|i)nsulation", v_ift), 3,
        ifelse(grepl("(R|r)ubber", v_ift), 4,
          ifelse(grepl("(P|p)rofiled", v_ift), 5,
            ifelse(grepl("^(T|t)imber", v_ift), 6, 7)
          )
        )
      )
    )
  )

  # Parse internal floor area
  v_ifl <- tblLss$H081_InternalFloorArea
  v_ifk <- ifelse(is.na(v_ifl) | v_ifl < 0.01,
    0, t_partyfloor.ku$value[v_ifn]
  )

  # Parse internal ceiling construction
  v_ict <- tblLss$H082_InternalCeilingConstruction
  v_icn <- ifelse(grepl("(P|p)lasterboard", v_ict), 1, 2)
  v_icl <- tblLss$H083_InternalCeilingArea
  v_ick <- ifelse(is.na(v_icl) | v_icl < 0.01,
    0, t_partyceiling.ku$value[v_icn]
  )

  # Pre-define empty data frame
  df_par <- data.frame(
    names = c("type", "netArea", "U", "AxU", "K", "AxK"),
    PartyWall = 0, PartyFloor = 0, PartyCeiling = 0,
    IntWall = 0, IntFloor = 0, IntCeiling = 0
  )
  rownames(df_par) <- df_par$names
  df_par$names <- NULL
  df_par <- as.data.frame(t(df_par))

  # Fill summary table
  df_par$type <- c(v_pwt, v_pft, v_pct, v_iwt, v_ift, v_ict)
  df_par$netArea <- c(v_pwl, v_pfl, v_pcl, v_iwl, v_ifl, v_icl)
  df_par$U <- c(v_pwu, rep(NA, 5))
  df_par$AxU <- c(v_pwa, rep(NA, 5))
  df_par$K <- c(v_pwk, v_pfk, v_pck, v_iwk, v_ifk, v_ick)
  df_par$AxK <- as.numeric(df_par$netArea) * as.numeric(df_par$K)

  return(df_par)
}

obtain_hlp_building <- function(tblFlrs, tblOpns, tblOpqs, tblInts, varAge, varTFA) {
  #' @title Obtain building thermal properties
  #' @description This function calculates various thermal properties
  #' of a building based on data tables and additional variables.
  #'
  #' @param tblFlrs Data table containing floor information (potentially unused)
  #' @param tblOpns Data table containing information about openings (windows etc.)
  #' @param tblOpqs Data table containing information about opaque elements (walls etc.)
  #' @param tblInts Data table containing information about internal elements
  #' @param varAge Numeric variable representing building age
  #' @param varTFA Numeric variable representing the total floor area
  #'
  #' @return Data frame containing thermal properties: opening ratio,
  #' fabric heat loss, heat capacity, thermal mass, thermal bridge,
  #' and total heat loss.

  # Calculate opening ratio
  net_wall_area <- tblOpqs[c("ExtrWall"),"netArea"]
  opening_ratio <- tblOpns[c("WinA", "WinB", "WinRoof"),] %>%
    dplyr::summarise(opening_ratio = sum(netArea) / net_wall_area) %>%
    dplyr::pull(opening_ratio)

  # Calculate fabric heat loss
  fabric_heat_loss__W_K <- sum(tblOpns$AxU, tblOpqs$AxU, tblInts$AxU, na.rm = TRUE)

  # Calculate heat capacity and thermal mass
  heat_capacity__kJ_K <- sum(tblOpqs$AxK, tblInts$AxK, na.rm = TRUE)
  thermal_mass__kJ_m2K <- heat_capacity__kJ_K / varTFA

  # Calculate thermal bridge based on age
  varAreas <- sum(tblOpns$netArea, tblOpqs$netArea, na.rm = TRUE)
  thermal_bridge__W_K <- ifelse(varAge > 9, varAreas * 0.15, varAreas * 0.15 * 0.25)

  # Calculate total heat loss
  total_heat_loss__W_K <- fabric_heat_loss__W_K + thermal_bridge__W_K

  # Create the result data frame
  df_trm <- tibble::tibble(
    opening_ratio = opening_ratio,
    fabric_heat_loss__W_K = fabric_heat_loss__W_K,
    heat_capacity__kJ_K = heat_capacity__kJ_K,
    thermal_mass__kJ_m2K = thermal_mass__kJ_m2K,
    thermal_bridge__W_K = thermal_bridge__W_K,
    total_heat_loss__W_K = total_heat_loss__W_K
  )

  return(df_trm)
}

compile_envelope_heat_loss_data <- function(l_flr, l_glz, l_opq, l_int, l_bld) {
  #' @title Collect and clean building element data
  #' @description This function takes a list of data tables containing
  #' information about different building elements (floors, glazing,
  #' opaques, internals, and building) and performs basic cleaning.
  #'
  #' @param l_flr Data table containing floor information
  #' @param l_glz Data table containing glazing information
  #' @param l_opq Data table containing opaque element (wall etc.) information
  #' @param l_int Data table containing internal element information
  #' @param l_bld Data table containing building information
  #'
  #' @return List containing cleaned data tables for floors, openings,
  #' opaques, internals, and building.

  # Clean floor data
  l_flr <- l_flr %>%
    tibble::rownames_to_column("zone") %>%
    tibble::tibble() %>%
    dplyr::filter(!is.na(type))

  # Clean glazing data
  l_glz <- l_glz %>%
    tibble::rownames_to_column("element") %>%
    tibble::tibble() %>%
    dplyr::filter(!is.na(type))

  # Clean opaque element data
  l_opq <- l_opq %>%
    tibble::rownames_to_column("element") %>%
    tibble::tibble() %>%
    dplyr::filter(!is.na(type))

  # Clean internal element data
  l_int <- l_int %>%
    tibble::rownames_to_column("element") %>%
    tibble::tibble() %>%
    dplyr::filter(!is.na(type))

  # Convert building data to tibble (if necessary)
  l_bld <- l_bld %>% tibble::tibble()

  # Combine cleaned data tables into a list
  l_sum <- list(
    floor = l_flr, openings = l_glz, opaques = l_opq,
    internal = l_int, building = l_bld
  )

  return(l_sum)
}

estimate_heat_loss_parameter <- function(hid, l_param,
                                         d_ids = the$core_index,
                                         l_ehs = s_ehs_2011_ext){
  #' @title Estimates heat loss parameters for a given housing ID
  #'
  #' @description Calculates heat loss parameters for a specific
  #' housing ID based on provided data.
  #'
  #' @param hid Housing ID
  #' @param l_param List of parameters (unused in this function)
  #' @param d_ids Data frame containing ID information (default: the$core_index)
  #' @param l_ehs List of energy and housing survey data (default: s_ehs_2011_ext)
  #'
  #' @return List containing heat loss parameters for the given housing ID


  # Get ID parameters
  varID <- d_ids[hid, ]
  idEhs <- get_aacode(varID$index)

  # Get pivotal datasets
  tbl_Gmt <- l_ehs$geometry %>%
    dplyr::filter(V001_HousingCode == idEhs)
  tbl_Lss <- l_ehs$other.heat.loss.elements %>%
    dplyr::filter(V001_HousingCode == idEhs)
  tbl_Inf <- l_ehs$dwelling.and.household.information %>%
    dplyr::filter(V001_HousingCode == idEhs)
  tbl_Cmp <- l_ehs$complementary %>%
    dplyr::filter(V001_HousingCode == idEhs)

  # Get archetype parameters
  dtaID <- obtain_archetype_parameters(idEhs, l_ehs)
  var_epoch <- tbl_Inf$D001_DwellingAge
  var_tfa <- as.numeric(dtaID$vTFA)

  # Collect properties
  df_trm_floor.ex <- list(
    tbl_Gmt, tbl_Lss, tbl_Inf)
  df_trm_floor.ex <- obtain_hlp_floors_bbgf(idEhs, df_trm_floor.ex)

  df_trm_openings <- list(
    tbl_Gmt, tbl_Lss, tbl_Cmp)
  df_trm_openings <- obtain_hlp_openings(idEhs, df_trm_openings)

  df_trm_opaques <- list(
    tbl_Gmt, tbl_Lss, tbl_Cmp, tbl_Inf, df_trm_floor.ex)
  df_trm_opaques <- obtain_hlp_opaques(idEhs, df_trm_opaques)

  df_trm_internals <- list(
    tbl_Gmt, tbl_Lss, tbl_Cmp, tbl_Inf, df_trm_floor.ex)
  df_trm_internals <- obtain_hlp_internals(idEhs, df_trm_internals)

  df_trm_building <- obtain_hlp_building(
    df_trm_floor.ex, df_trm_openings, df_trm_opaques,
    df_trm_internals, var_epoch, var_tfa)

  # Compose list
  l_hlb <- compile_envelope_heat_loss_data(
    df_trm_floor.ex,
    df_trm_openings,
    df_trm_opaques,
    df_trm_internals,
    df_trm_building
  )

  return(l_hlb)
}
