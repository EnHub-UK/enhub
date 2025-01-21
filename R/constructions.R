# Tables ----

obtain_materials <- function(sel, d_prop_act, lbl_idf = l_idf_blocks) {
  #' @title Obtains materials based on selection and properties.
  #'
  #' @description This function subsets material properties based on the provided selection,
  #' processes the data, and returns a data frame with material and label information.
  #'
  #' @param sel Character string specifying the material selection.
  #' @param d_prop_act Data frame containing material properties.
  #' @param lbl_idf List of IDF block labels (default: l_idf_blocks).
  #'
  #' @return A data frame with columns `material` and `label`.

  # Subset material properties
  d_sel <- data.frame(t(subset(d_prop_act, name == sel)))
  colnames(d_sel) <- "material"
  d_sel$label <- rownames(d_sel)
  rownames(d_sel) <- NULL

  # Obtain type of IDF object
  id_sel_block <- d_sel %>%
    dplyr::filter(label == "type") %>%
    dplyr::select(material) %>%
    as.character()

  # Obtain comments (description) from global `l_idf_blocks`
  d_block <- lbl_idf[[id_sel_block]]
  d_block <- c("Type", d_block)
  d_block_normal <- data.frame(
    label = d_block,
    normal = tolower(gsub("[[:punct:]]|[[:space:]]", "_", d_block))
  )

  # Process subset of material properties
  d_sel <- d_sel %>%
    dplyr::mutate(
      material = as.character(material),
      label = as.character(label)
    ) %>%
    dplyr::filter(label != "code" & label != "source")

  d_sel <- d_sel %>% dplyr::filter(label %in% d_block_normal$normal)
  colnames(d_sel) <- c("value", "normal")
  d_sel <- d_sel %>%
    plyr::join(d_block_normal, by = "normal") %>%
    dplyr::select(-normal)
  colnames(d_sel) <- c("material", "label")

  d_sel <- d_sel %>%
    dplyr::mutate(label = factor(label, ordered = TRUE, levels = d_block_normal$label))
  d_sel <- d_sel[order(d_sel$label), ]
  d_sel <- d_sel %>% dplyr::mutate(material = gsub("-", "", material))

  return(d_sel)
}

obtain_hlp_u_value <- function(sel, d_layers) {
  #' @title Calculates thermal properties from layer data
  #'
  #' @description This function calculates the overall thermal resistance (R-value) and
  #' thermal transmittance (U-value) based on layer properties in `d_layers`.
  #' Expected columns in `d_layers`:
  #' - name: Character vector of layer names.
  #' - thickness__m: Numeric vector of layer thickness (meters).
  #' - conductivity__w_m_k: Numeric vector of thermal conductivity (W/m*K).
  #' Additional columns for other thermal properties (optional)
  #'
  #' @param sel A character vector specifying layers to include.
  #' @param d_layers A data frame containing layer properties.
  #'
  #' @return A list containing:
  #'   - layers: A data frame of selected layer properties.
  #'   - R__resistance__m2K_w: The total thermal resistance (m^2*K/W).
  #'   - U__transmittance__W_m2K: The overall thermal transmittance (W/m^2*K).

  # Select relevant layers from d_layers
  d_properties <- d_layers[match(sel, d_layers$name), ]

  # Clean column names
  rownames(d_properties) <- NULL
  colnames(d_properties) <- gsub("\\_+$", "", colnames(d_properties))

  # Replace hyphens with empty strings
  d_properties[d_properties == "-"] <- ""

  # Convert specific columns to numeric/integer
  d_properties <- d_properties %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^thic|^cond|^ther|^sola|^visi"), as.numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^dens|^spec"), as.integer))

  # Calculate thermal resistance for missing values
  d_r <- d_properties %>%
    dplyr::filter(is.na(thermal_resistance__m2_k_w)) %>%
    dplyr::mutate(thermal_resistance__m2_k_w = thickness__m / conductivity__w_m_k)

  # Filter and combine data with calculated thermal resistance
  d_properties <- d_properties %>%
    dplyr::filter(!is.na(thermal_resistance__m2_k_w)) %>%
    dplyr::bind_rows(d_r)

  # Calculate overall thermal properties
  l_thermal <- list()
  l_thermal[["layers"]] <- d_properties
  l_thermal[["R__resistance__m2K_w"]] <-
    sum(l_thermal[["layers"]]$thermal_resistance__m2_k_w)
  l_thermal[["U__transmittance__W_m2K"]] <-
    1 / l_thermal[["R__resistance__m2K_w"]]

  return(l_thermal)
}

obtain_construction <- function(i_sel, i_typ, lbl_prefix = "XX", l_hlpc = l_HLP, k_width = "-", k_u_value = "-") {
  #' @title Retrieves and formats construction information
  #'
  #' @description This function retrieves information about a construction and its materials
  #' based on selection index (`i_sel`) and construction type (`i_typ`). It also
  #' formats the data and estimates thermal transmittance (if applicable).
  #'
  #' @param i_sel Integer index of the construction to retrieve.
  #' @param i_typ Character string specifying construction type ("opaque" or "translucent").
  #' @param lbl_prefix Character string to prefix construction/material names (default "XX").
  #' @param l_hlpc A list containing HLP construction and material data (assumed to exist).
  #' @param k_width Numeric value to adjust material width (default "-").
  #' @param k_u_value Numeric value to adjust material U-value (default "-").
  #'
  #' @return A list containing:
  #'   - table: A data frame with estimated thermal properties (opaque constructions only).
  #'             - OR - "Not yet implemented" for translucent constructions.
  #'   - idf: A character vector containing formatted identification text for the construction
  #'         and its materials.


  # Helper function to add prefix
  add_prefix <- function(k, lbl_pre = lbl_prefix) {
    k <- paste0(lbl_pre, "__", gsub(" ", "_", k))
    k <- gsub(paste0(lbl_pre, "__-$"), "-", k, perl = TRUE)
    return(k)
  }

  # Load construction data based on type
  if (i_typ == "opaque") {
    df_const <- l_hlpc$HLP_constructionsOpaque
    df_mater <- l_hlpc$HLP_materialsOpaque
  } else {
    df_const <- l_hlpc$HLP_constructionsTranslucent
    df_mater <- l_hlpc$HLP_materialsTranslucent
  }

  # Add unique prefix to construction and material names
  df_mater$name <- add_prefix(df_mater$name, lbl_prefix)
  df_const.sub <- as.data.frame(
    apply(subset(df_const,
      select = grep("(name|layer)", colnames(df_const))
    ), 2, add_prefix)
  )
  df_const[, names(df_const.sub)] <- df_const.sub

  # Parse construction data and layers
  lblCons <- as.character(df_const$name[i_sel])
  i_sel <- data.frame(t(df_const[i_sel, ]))
  colnames(i_sel) <- "object"
  i_sel$object <- as.character(i_sel$object)
  i_sel$label <- rownames(i_sel)
  rownames(i_sel) <- NULL
  i_sel <- rbind(c("Construction", "Type"), i_sel)
  i_sel <- subset(i_sel, object != "-" & label != "code" & label != "source")
  varLyr <- i_sel$object[3:dim(i_sel)[1]]

  # Amend surface thickness if requested
  df_mater$thickness__m_ <- as.character(df_mater$thickness__m_)
  if (k_width != "-") {
    tblWdt <- df_mater[match(varLyr, df_mater$name), ]
    tblWdt$thickness__m_ <- suppressWarnings(edit_surface_width(
      as.numeric(tblWdt$thickness__m_), k_width
    ))
    tblWdt <- tblWdt[!duplicated(tblWdt), ]
    tblWdt$thickness__m_ <- as.character(tblWdt$thickness__m_)
    df_mater[match(unique(varLyr), df_mater$name), ] <- tblWdt
  }

  # Amend transmittance if requested
  if (k_u_value != "-") {
    tblWdt <- df_mater[match(varLyr, df_mater$name), ]
    tblWdt <- suppressWarnings(edit_thermal_properties(tblWdt, k_u_value))
    df_mater[match(unique(varLyr), df_mater$name), ] <- tblWdt %>% unique()
  }

  # Obtain materials
  l_materials <- lapply(unique(varLyr), obtain_materials, df_mater)
  names(l_materials) <-
    lapply(l_materials, function(x) x$material[1]) %>%
    as.character() %>%
    gsub("\\:", "_", .)

  # Split objects
  i_sel$object <- gsub("-", "", i_sel$object)

  # Split type of constructions
  idf_opaque <- idf_glazing <- idf_gas <- idf_air_gap <- idf_no_mass <- NULL
  k_opaque <- grep("^Material$", names(l_materials))
  k_glazing <- grep("^Windowmaterial_Glazing$", names(l_materials))
  k_gas <- grep("^Windowmaterial_Gas$", names(l_materials))
  k_air_gap <- grep("^Material_AirGap$", names(l_materials))
  k_no_mass <- grep("^Material_NoMass$", names(l_materials))


  if (!identical(k_opaque, character(0))) {
    l_opaque <- l_materials[k_opaque]
    idf_opaque <- make_construction_materials(l_opaque)
  }

  if (!identical(k_glazing, character(0))) {
    l_glazing <- l_materials[k_glazing]
    idf_glazing <- make_construction_glazing(l_glazing)
  }

  if (!identical(k_gas, character(0))) {
    l_gas <- l_materials[k_gas]
    idf_gas <- make_construction_gas(l_gas)
  }

  if (!identical(k_air_gap, character(0))) {
    l_air_gap <- l_materials[k_air_gap]
    idf_air_gap <- make_construction_air_gap(l_air_gap)
  }

  if (!identical(k_no_mass, character(0))) {
    l_no_mass <- l_materials[k_no_mass]
    idf_no_mass <- make_construction_no_mass(l_no_mass)
  }

  # Generate idf blocks
  idf_materials <- c(idf_opaque, idf_glazing, idf_gas, idf_air_gap)
  idf_constructions <- make_construction_layers(i_sel)

  # Assign the IDFs
  idf_block <- c(idf_constructions, idf_materials)

  # Estimate thermal transmittance
  if (i_typ == "opaque") {
    df_thermal <- obtain_hlp_u_value(varLyr, df_mater)
  } else {
    df_thermal <- "Not yet implemented => BS EN 673"
  }

  # Return elements
  l_res <- list(table = df_thermal, idf = idf_block)

  return(l_res)
}


# Iterators ----

make_construction_materials <- function(l_iter, l_sup = l_idf_blocks) {
  #' @title Create IDF block for Material objects based on list of materials
  #'
  #' @description This function generates an IDF block representing material
  #' objects using an iterator (`l_iter`) defining list of materials. It
  #' also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing materials
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Prepend object type to parameter list
    l_values <- l_in$material %>% as.vector()
    l_values <- l_values[2:length(l_values)]
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- "Material"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", l_head, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_construction_glazing <- function(l_iter, l_sup = l_idf_blocks) {
  #' @title Create IDF block for Windowmaterial:Glazing objects based on
  #' list of materials
  #'
  #' @description This function generates an IDF block representing glazing
  #' objects using an iterator (`l_iter`) defining list of materials. It
  #' also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing materials
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Prepend object type to parameter list
    l_values <- l_in$material %>% as.vector()
    l_values <- l_values[2:length(l_values)]
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- "Windowmaterial:Glazing"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", l_head, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_construction_gas <- function(l_iter, l_sup = l_idf_blocks) {
  #' @title Create IDF block for Windowmaterial_Gas objects based on
  #' list of materials
  #'
  #' @description This function generates an IDF block representing gas
  #' objects using an iterator (`l_iter`) defining list of materials. It
  #' also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing materials
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Prepend object type to parameter list
    l_values <- l_in$material %>% as.vector()
    l_values <- l_values[2:length(l_values)]
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- "Windowmaterial:Gas"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", l_head, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_construction_air_gap <- function(l_iter, l_sup = l_idf_blocks) {
  #' @title Create IDF block for Windowmaterial:AirGap objects based on
  #' list of materials
  #'
  #' @description This function generates an IDF block representing air-gap
  #' objects using an iterator (`l_iter`) defining list of materials. It
  #' also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing materials
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Prepend object type to parameter list
    l_values <- l_in$material %>% as.vector()
    l_values <- l_values[2:length(l_values)]
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- "Material:AirGap"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", l_head, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_construction_no_mass <- function(l_iter, l_sup = l_idf_blocks) {
  #' @title Create IDF block for Windowmaterial:AirGap objects based on
  #' list of materials
  #'
  #' @description This function generates an IDF block representing air-gap
  #' objects using an iterator (`l_iter`) defining list of materials. It
  #' also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing materials
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'


  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Prepend object type to parameter list
    l_values <- l_in$material %>% as.vector()
    l_values <- l_values[2:length(l_values)]
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- "Material:NoMass"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(l_iter, make_idf_block, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", l_head, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

make_construction_layers <- function(l_iter, l_sup = l_idf_blocks) {
  #' @title Create IDF block for Construction objects based on list of layers
  #'
  #' @description This function generates an IDF block representing a
  #' construction object an iterator (`l_iter`) defining list of layers.
  #' It also uses supplementary data (`l_sup`) for comments.
  #'
  #' @param l_iter: List containing list of layers.
  #' @param l_sup: List with supplementary data (defaults to `l_idf_blocks`).
  #'
  #' @return A character vector representing the complete IDF block content.
  #'

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Prepend object type to parameter list
    l_values <- l_in$object %>% as.vector()
    l_values <- l_values[2:length(l_values)]
    l_block_set <- c(i_act, l_values)

    # Create block object with comments using helper function (assumed)
    l_obj <- iterate_on_list(l_block_set, l_comment)

    # Combine block with comment, empty lines
    txt_res <- c(unlist(l_obj))

    # Return the complete IDF block content
    return(txt_res)
  }

  # Active object for plant loop
  i_active <- "Construction"

  # Create block content for each loop in iterator, and combine
  l_body <- lapply(list(l_iter), make_idf_block, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", l_head, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}


# Walls ----

edit_surface_width <- function(d_source, sel) {
  #' @title Adjusts surface width based on target value
  #'
  #' @description This function adjusts the values in `d_source` proportionally to match the total `sel`.
  #'
  #' @param d_source A numeric vector representing the original surface widths.
  #' @param sel The target total width.
  #'
  #' @return A numeric vector of adjusted surface widths, rounded to four decimal places.

  # Calculate total original width
  total_width <- sum(d_source, na.rm = TRUE)

  # Calculate adjustment factor
  adjustment_factor <- sel / total_width

  # Adjust widths proportionally
  adjusted_widths <- d_source * adjustment_factor

  # Round to four decimal places
  result <- round(adjusted_widths, 4)

  return(result)
}

edit_thermal_properties <- function(d_thr, u_val) {
  #' @title Adjusts thermal properties to achieve target U-value
  #'
  #' @description This function modifies the thermal conductivity of materials in a dataset
  #' to achieve a specified U-value while maintaining the same thermal resistance.
  #'
  #' @param d_thr A data frame containing thermal properties.
  #'   Expected columns: `conductivity__w_m_k_`, `thickness__m_`.
  #' @param u_val The desired U-value (W/m^2*K).
  #'
  #' @return The modified data frame with adjusted thermal conductivity.

  # Extract thermal properties
  l_k <- as.numeric(d_thr$conductivity__w_m_k_)
  l_l <- as.numeric(d_thr$thickness__m_)
  l_R <- l_l / l_k

  # Calculate current R-value and U-value
  R_val <- sum(l_R, na.rm = T)
  U_val <- 1 / R_val

  # Calculate new R-value and distribute it proportionally
  R_new <- 1 / u_val
  l_R_new <- (l_R / R_val) * R_new
  l_k_new <- l_l / l_R_new

  # Update thermal conductivity in the data frame
  d_thr$conductivity__w_m_k_ <- as.character(round(l_k_new, 5))

  return(d_thr)
}

assign_void_boundary <- function(l_sup = l_idf_blocks) {
  #' @title Assigns a void air boundary construction
  #'
  #' @description Creates an IDF block for a void air boundary construction with specified parameters.
  #'
  #' @param l_sup A list of IDF blocks, used as a base for the new construction.
  #'   (Default: `l_idf_blocks`)
  #'
  #' @return A character vector representing the IDF block for the void air boundary.


  # Define parameters for the void air boundary construction
  l_par_outputs <- list(
    "Construction:AirBoundary" = list(
      name = "Void Air Boundary",
      mix_rate = "SimpleMixing",
      mix_sched = "0.5",
      sim_sched = ""
    )
  )

  # Create IDF block using the specified parameters and base list
  idf_res <- lapply(
    names(l_par_outputs), iterate_on_output_list, l_par_outputs, l_sup
  ) %>%
    unlist()

  # Add empty line at the beginning
  idf_res <- c("", idf_res)

  return(idf_res)
}

assign_external_surface <- function(dfa, df_hlc) {
  #' @title Assigns external surface properties
  #'
  #' @description Creates IDF blocks for external surface properties based on provided data.
  #'
  #' @param dfa A data frame containing surface parameters (e.g., construction type, width).
  #' @param df_hlc A data frame containing construction and material data (assumed to exist).
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the external surface.
  #'   - summary: A data frame summarizing the surface properties.

  # Use pre-defined indentation
  indent <- the$.indent

  # Extract surface parameters
  i_typ <- as.integer(dfa$D077_ExternalWallConstruction)
  i_width <- as.numeric(dfa$D030_WallThickness)
  lbl_grp <- "ES"
  lbl_srf <- "Exterior Wall"

  # Parse custom U-value (if provided)
  i_u_val <- ifelse(is.null(the$GLOBAL_PAR$wall_transmittance),
    "-", the$GLOBAL_PAR$wall_transmittance
  )

  # Create IDF block header and object label
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # Obtain construction and summary data
  l_cons <- obtain_construction(
    i_typ, "opaque", lbl_grp,
    k_width = i_width, k_u_value = i_u_val
  )
  l_summ <- l_cons$table
  l_idft <- l_cons$idf

  # Process parameters and format output
  l_summ$object <- lbl_srf
  lbl_cons <- grep("^Construction,", l_idft)
  l_idft[lbl_cons] <- "Construction,"
  l_idft[lbl_cons + 1] <- lbl_obj

  # Return results
  l_res <- list(idf = c("", l_idft), summary = l_summ)

  return(l_res)
}

assign_internal_surface <- function(dfa, df_hlc, i_width = 0.145) {
  #' @title Assigns internal surface properties
  #'
  #' @description Creates IDF blocks for internal surface properties based on provided data.
  #'
  #' @param dfa A data frame containing surface parameters (e.g., construction type).
  #' @param df_hlc A data frame containing construction and material data (assumed to exist).
  #' @param i_width The default wall thickness (default: 0.145).
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the internal surface.
  #'   - summary: A data frame summarizing the surface properties.

  # Use pre-defined indentation
  indent <- the$.indent

  # Assign surface parameters (using a fixed construction type for now). There
  # is only one construction type reported in the EHS: surface #19 (heavy partition)
  i_typ <- dfa$H078_InternalWallConstruction
  i_typ <- 19
  lbl_grp <- "IS"
  lbl_srf <- "Interior Wall"

  # Create IDF block header and object label
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # Obtain construction and summary data
  l_cons <- obtain_construction(i_typ, "opaque", lbl_grp, k_width = i_width)
  l_summ <- l_cons$table
  l_idft <- l_cons$idf

  # Process parameters and format output
  l_summ$object <- lbl_srf
  lbl_cons <- grep("^Construction,", l_idft)
  l_idft[lbl_cons] <- "Construction,"
  l_idft[lbl_cons + 1] <- lbl_obj

  # Return results
  l_res <- list(idf = c("", l_idft), summary = l_summ)

  return(l_res)
}



# Floors ----

assume_heated_underfloor <- function(hc) {
  #' @title Checks if heated underfloor system is assumed
  #'
  #' @description Determines if a heated underfloor system is assumed based on the provided energy system code.
  #'
  #' @param hc A character string representing the energy system code.
  #'
  #' @return A logical value indicating whether a heated underfloor system is assumed (TRUE) or not (FALSE).

  en_sys <- c("MSH14DHW3", "MSH14DHW8", "MSH15DHW3", "MSH15DHW8")
  return(ifelse(hc %in% en_sys, TRUE, FALSE))
}

assign_underfloor_heat <- function(idf_block, l_sup = l_idf_blocks) {
  #' @title Assigns underfloor heating parameters
  #'
  #' @description Inserts IDF blocks for underfloor heating parameters if specified.
  #'
  #' @param idf_block A character vector representing the existing IDF content.
  #'
  #' @return A character vector representing the updated IDF content with underfloor heating parameters (if applicable).

  # Helper function to create an IDF block with comments
  make_idf_block <- function(l_in, i_act, l_com = l_sup) {

    # Extract comment from supplementary data (assuming l_sup has comments)
    l_comment <- l_com[[i_act]]

    # Define loop parameter list
    l_values <- list(
      namec = paste0(l_in, "--source"),
      namec = l_in,
      trhmno = 4,
      thrmme = 4,
      ctfdim = 1,
      tubesp = 0.1524,
      twodim = 0.0
    )

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
  i_active <- "ConstructionProperty:InternalHeatSource"

  # Extract construction name
  k_lbl <- idf_block[grep("Construction\\,", idf_block) + 1] %>%
    gsub("\\,.*$", "", .) %>%
    gsub("^\\s+", "", .)

  # Create block content for each loop in iterator, and combine
  l_body <- make_idf_block(k_lbl, i_active) %>%
    unlist() %>%
    as.character()

  # Create block title
  # l_head <- make_idf_block_title(i_active)

  # Combine title, empty lines, and block content
  txt_idf <- c("", idf_block, "", l_body)

  # Return the complete IDF block content
  return(txt_idf)
}

assign_ground_floor <- function(df_hlc, l_hlpc = l_HLP) {
  #' @title Assigns ground floor properties
  #'
  #' @description Creates IDF blocks for ground floor properties based on provided data and construction library.
  #'
  #' @param df_hlc A data frame containing ground floor type information (e.g., timber, solid).
  #' @param l_hlpc A list containing construction library data (assumed to exist).
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the ground floor.
  #'   - summary: A data frame summarizing the ground floor properties.

  # Use pre-defined indentation
  indent <- the$.indent

  # Extract ground floor type from data frame
  lbl_grpc <- df_hlc %>% dplyr::filter(type != '-') %>%
    tail(1) %>% dplyr::pull(type)

  # Handle missing values and translate type labels
  lbl_grpc <- ifelse(is.na(lbl_grpc), "-", lbl_grpc)
  lbl_grpc <- ifelse(lbl_grpc == "timber", "Timber_Flooring",
    ifelse(lbl_grpc == "solid", "Heavy_Floor", "Light_Floor")
  )

  # Find ground floor type index in construction library
  lbl_grpc <- grep(lbl_grpc, l_hlpc$HLP_constructionsOpaque$name)

  # Define group and surface labels
  lbl_grp <- "EF"
  lbl_srf <- "Exterior Floor"

  # Parse custom U-value (if provided)
  i_u_val <- ifelse(is.null(the$GLOBAL_PAR$floor_transmittance),
    sum(df_hlc$Utotal, na.rm = TRUE), the$GLOBAL_PAR$floor_transmittance
  )

  # Create IDF block header and object label
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # Obtain construction data
  l_cons <- obtain_construction(lbl_grpc, "opaque", lbl_grp, k_u_value = i_u_val)

  # Extract summary and IDF text from construction data
  l_summ <- l_cons[[1]]
  l_idft <- l_cons[[2]]

  # Update object name in summary and IDF text
  l_summ$object <- lbl_srf
  lbl_cons <- grep("^Construction,", l_idft)
  l_idft[lbl_cons] <- "Construction,"
  l_idft[lbl_cons + 1] <- lbl_obj

  # Format returned data
  l_res <- list(idf = c("", l_idft), summary = l_summ)

  return(l_res)
}

assign_internal_floor <- function(idfTo, idf.label = "Interior Floor") {
  #' @title Assigns internal floor properties
  #'
  #' @description Inserts an "Interior Floor" definition within existing IDF content.
  #'
  #' @param idfTo A character vector representing the existing IDF content.
  #' @param idf.label A character string specifying the label for the internal floor (default: "Interior Floor").
  #'
  #' @return A character vector representing the updated IDF content with an internal floor definition.

  # Use pre-defined indentation
  indent <- the$.indent

  # Create label for the internal floor
  var.label <- paste0(indent, idf.label, add_idf_comment_custom("Name", 9))

  # Find the starting line of the existing floor construction
  ln.ma <- grep("Construction,", idfTo)[1]

  # Identify lines belonging to the existing floor construction
  ln.mb <- data.frame(grep = regexpr(";", idfTo[ln.ma:length(idfTo)]))

  # Calculate the number of lines in the existing floor construction
  ln.mb$flag <- ifelse(ln.mb$grep < 0, 0, rownames(ln.mb))
  ln.mb <- as.integer(ln.mb$flag[ln.mb$flag > 0])[1]

  # Extract existing IDF content excluding the floor construction
  idfTo <- c(idfTo[c(ln.ma:(ln.ma + ln.mb - 1))])
  idfRev <- idfTo[3:length(idfTo)]

  # Assemble the updated IDF content
  idfTo <- c(idfTo[1], var.label, rev(idfRev))

  # Replace semicolons with commas (except for the last line)
  idfTo <- gsub(";", ",", idfTo)
  idfTo[length(idfTo)] <- gsub(",", ";", idfTo[length(idfTo)])

  return(idfTo)
}



# Windows ----

assign_window_type <- function(dfa, dfb, df_hlc, l_sup = l_idf_blocks) {
  #' @title Assigns window type and properties
  #'
  #' @description Creates IDF blocks for window types, including exterior and interior windows,
  #' based on provided data.
  #'
  #' @param dfa A data frame containing window type and frame information.
  #' @param dfb A data frame containing additional window parameters.
  #' @param df_hlc A data frame containing construction and material data.
  #' @param l_sup A list of IDF blocks, used as a base for the new constructions.
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the windows.
  #'   - summary: A data frame summarizing the window properties.


  # Use pre-defined indentation
  indent <- the$.indent

  # Collect window parameters
  i_type_a <- dfa$D068_Windows1Type
  i_type_b <- dfa$D070_Windows2Type
  i_frame_a <- dfa$D071_Windows1FrameType
  i_frame_b <- dfa$D072_Windows2FrameType
  i_glz_ratio <- dfb$V564_DoubleGlazingRatio

  # Determine window type and frame based on input data
  if (grepl("triple", i_type_a) | grepl("triple", i_type_b)) {
    i_locs <- c(195, 35)
  } else if (grepl("double", i_type_a) | grepl("double", i_type_b)) {
    i_locs <- c(22, 19)
  } else {
    i_locs <- c(161, 3)
  }
  i_frms <- ifelse(grepl("upvc", i_frame_a) | grepl("upvc", i_frame_b), 3,
    ifelse(grepl("metal", i_frame_a) | grepl("metal", i_frame_b), 2, 1)
  )

  # Extract window parameters
  lbl_grpc <- i_locs[1]
  i_width <- i_locs[2] / 1000

  # Parse custom U-value (if provided)
  i_u_val <- ifelse(
    is.null(the$GLOBAL_PAR$window_transmittance), "-",
    the$GLOBAL_PAR$window_transmittance
  )


  # Create IDF block header and object labels
  lbl_grp <- "EW"
  lbl_srf <- "Exterior Window"
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # .... get window parameters
  l_cons_ew <- obtain_construction(lbl_grpc, "glazing", lbl_grp,
    k_width = i_width, k_u_value = i_u_val
  )

  # Adjust construction label for exterior window
  lbl_cons <- grep("^Construction,", l_cons_ew$idf)
  l_cons_ew$idf[lbl_cons + 1] <- lbl_obj


  # Get frame parameters
  v_frame_u <- c(upvc = 2.3, metal = 3.8, wood = 2.6)
  i_frms <- as.numeric(v_frame_u[i_frms])

  # Generate IDF block for window frame
  l_par_outputs <- list(
    "WindowProperty:FrameAndDivider" = list(
      name = "WindowFrame",
      frmwm = 0.02,
      fropm = 0.02,
      fripm = 0.02,
      fcw2k = i_frms,
      rofgctcc = 1.2,
      frmsa = 0.8,
      frmva = 0.8,
      frthe = 0.9,
      dvdrt = "DividedLite",
      dvdwm = 0.0085,
      nmohd = 2,
      nmovd = 2,
      dvopm = 0.02,
      dvipm = 0.02,
      dcw2k = i_frms,
      rodgctcc = 1.2,
      dvdsa = 0.82,
      dvdva = 0.82,
      dvthe = 0.91,
      rvlab = 0.0,
      silld = 0.0,
      silla = 0.0,
      revdp = 0.0,
      revab = 0.0,
      nfrct = "CurtainWall"
    )
  )

  idf_frame <- lapply(
    names(l_par_outputs), iterate_on_output_list, l_par_outputs, l_sup
  ) %>%
    unlist()


  # Get interior window parameters
  lbl_grpc <- 161
  i_width <- 3 / 1000

  # Create IDF block header and object label for interior window
  lbl_grp <- "IW"
  lbl_srf <- "Interior Window"
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # Obtain interior window construction
  l_cons_iw <- obtain_construction(lbl_grpc, "glazing", lbl_grp,
    k_width = i_width
  )

  # Adjust construction label for interior window
  lbl_cons <- grep("^Construction,", l_cons_iw$idf)
  l_cons_iw$idf[lbl_cons + 1] <- lbl_obj


  # Combine IDF blocks and summary data
  idf_block <- c("", idf_frame, l_cons_ew$idf, l_cons_iw$idf)
  df_sum <- df_hlc %>% dplyr::filter(!grepl("[dD]oor", element))
  l_res <- list(idf = idf_block, summary = df_sum)

  return(l_res)
}


# Doors ----

assign_door_type <- function(dfa, dfb, df_hlc) {
  #' @title Assigns door types and properties
  #'
  #' @description Creates IDF blocks for exterior and interior doors based on provided data.
  #'
  #' @param dfa A data frame containing door U-value information.
  #' @param dfb A data frame containing door count information.
  #' @param df_hlc A data frame containing construction and material data.
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the doors.
  #'   - summary: A list containing data frames for exterior and interior door summaries.


  # Use pre-defined indentation
  indent <- the$.indent

  # Extract door parameters
  v.u.door <- dfa$D058_DoorUValue
  v.n.door <- dfb$V565_Doors

  # Assign exterior door

  # Define labels
  lbl_grp <- "ED"
  lbl_srf <- "Exterior Door"
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))
  i_sel <- 31
  i_width <- 0.033

  # Parse custom U-value (if provided)
  i_u_val <- ifelse(
    is.null(the$GLOBAL_PAR$door_transmittance), "-", the$GLOBAL_PAR$door_transmittance
  )

  # Obtain exterior door construction
  l_cons_ed <- obtain_construction(i_sel, "opaque", lbl_grp, k_width = i_width, k_u_value = i_u_val)

  # Adjust construction label for exterior door
  lbl_cons <- grep("^Construction,", l_cons_ed$idf)
  l_cons_ed$idf[lbl_cons + 1] <- lbl_obj


  # Assign interior door

  # Define labels
  lbl_grp <- "ID"
  lbl_srf <- "Interior Door"
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))
  i_sel <- 32
  i_width <- 0.025

  # Obtain interior door construction
  l_cons_id <- obtain_construction(i_sel, "opaque", lbl_grp, k_width = i_width)

  # Adjust construction label for exterior door
  lbl_cons <- grep("^Construction,", l_cons_id$idf)
  l_cons_id$idf[lbl_cons + 1] <- lbl_obj


  # Combine IDF blocks and summaries
  l_res <- list(
    idf = c("", l_cons_ed$idf, l_cons_id$idf),
    summary = list(ext = l_cons_ed$table, int = l_cons_id$table)
  )

  return(l_res)
}


# Roofs ----

assign_external_roof <- function(dfa, dfb, k_att, df_hlc) {
  #' @title Assigns external roof properties
  #'
  #' @description Creates IDF blocks and summarizes properties for an external roof based on provided data and construction library.
  #'
  #' @param dfa A data frame containing roof construction information.
  #' @param dfb A data frame containing dwelling age information.
  #' @param k_att A logical value indicating a pitched roof (TRUE) or flat roof (FALSE).
  #' @param df_hlc A data frame containing construction and material data (assumed to exist).
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the external roof.
  #'   - summary: A data frame summarizing the external roof properties.

  # Function to obtain roof properties
  obtain_roof_properties <- function(df_a, df_b, k_at, l_cop = l_HLP) {
    # Extract roof parameters
    i_lft <- df_a$D081_RoomInRoofConstruction
    i_ins <- df_a$D080_LoftInsulationThickness
    i_rfc <- df_a$D079_RoofConstruction
    i_age <- df_b$D001_DwellingAge

    # Reference data frames (assumed to exist)
    df_lf_uval <- s_chm$CHM_RoomInRoofUvalue
    df_lf_thkn <- s_chm$CHM_LoftThickness
    df_rf_uins <- s_chm$CHM_RoofUvalueInsulation
    df_rf_uval <- s_chm$CHM_RoofUvalue
    df_rf_type <- s_chm$CHM_RoomInRoofType

    # Process roof properties
    k_at <- ifelse(k_at == "N", FALSE, TRUE)
    i_lft <- ifelse(i_lft == "-", "man made slate / flat", i_lft)
    i_lft <- ifelse(is.na(i_lft), "-", i_lft)
    i_rfc <- ifelse(grepl("pitch", i_rfc), 1, ifelse(grepl("that", i_rfc), 2, 3))
    i_rfc <- ifelse(i_rfc > 2 & k_at == TRUE, 1, i_rfc)
    i_sel <- ifelse(grepl("pitch", i_rfc), "^Roof_Slate_Or_Tile",
      ifelse(grepl("that", i_rfc), "^Roof_Thatched",
        "^Roof_Concrete_Or_Built_up"
      )
    )
    i_sel <- grep(i_sel, l_cop$HLP_constructionsOpaque$name)

    # Determine U-value based on roof type and presence of loft
    if (k_at == FALSE) {
      if (is.na(i_ins) | i_rfc == 3) {
        df_rmt <- df_rf_uval[, c(2, 3, 1)]
      } else {
        df_rmt <- df_rf_uins
      }
      i_layer <- 0.15
    } else {
      i_ins <- ifelse(i_ins == ">300", 300, as.integer(i_ins))
      i_ins <- ifelse(is.na(i_ins), 0.191, i_ins / 1000)
      i_ins <- ifelse(i_ins <= 0, 0.0001, i_ins)
      df_rmt <- df_lf_uval
      i_layer <- i_ins
    }

    # Calculate roof properties
    val_l_tile <- 0.013 + i_layer # l,tile [m]
    val_l <- 0.110 # l, [m]
    val_U <- as.numeric(unlist(df_rmt[i_age, i_rfc])) # U, [W/m²K]
    val_R <- round(1 / val_U, 3) # R, [m²K/W], R=l/λ
    val_K <- val_l / val_R # λ, [W/mK]
    val_c <- 960 # c, [J/kgK]
    val_d <- 1280 # d, [kg/m3]

    l_res <- list(
      type = i_sel, roof = i_rfc, loft = i_lft,
      l = val_l, u = val_U, r = val_R, k = val_K, c = val_c, d = val_d
    )
    return(l_res)
  }

  # Use pre-defined indentation
  indent <- the$.indent

  # Obtain roof properties
  l_roof_prop <- obtain_roof_properties(dfa, dfb, k_att)

  # Assign surface parameters
  lbl_grp <- "ER"
  lbl_srf <- "Exterior Roof"
  i_typ <- l_roof_prop$type
  i_width <- l_roof_prop$l

  # Parse custom U-value (if provided)
  i_u_val <- ifelse(
    is.null(the$GLOBAL_PAR$roof_transmittance), l_roof_prop$u,
    the$GLOBAL_PAR$roof_transmittance
  )

  # Add general labels
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # Generate lists
  l_cons <- obtain_construction(
    i_typ, "opaque", lbl_grp,
    k_width = i_width, k_u_value = i_u_val
  )
  l_summ <- l_cons$table
  l_idft <- l_cons$idf

  # Process parameters
  l_summ$object <- lbl_srf
  lbl_cons <- grep("^Construction,", l_idft)
  l_idft[lbl_cons] <- "Construction,"
  l_idft[lbl_cons + 1] <- lbl_obj

  # Combine values and labels
  l_res <- list(idf = c("", l_idft), summary = l_summ)

  return(l_res)
}

assign_internal_roof <- function(dfa, df_hlc) {
  #' @title Assigns internal roof (ceiling) properties
  #'
  #' @description Creates IDF blocks for an internal roof (ceiling) based on provided data and construction library.
  #'
  #' @param dfa A data frame containing internal ceiling construction information.
  #' @param df_hlc A data frame containing construction and material data (assumed to exist).
  #'
  #' @return A list containing:
  #'   - idf: A character vector representing the IDF blocks for the internal roof.
  #'   - summary: A data frame summarizing the internal roof properties.

  # Use pre-defined indentation
  indent <- the$.indent

  # Extract internal ceiling construction type
  i_sel <- as.character(dfa$H082_InternalCeilingConstruction)

  # Map construction type to standard options
  i_sel <- ifelse(grepl("plasterboard", i_sel),
    "Timber_I_Joists", "Timber_Semi_Ceiling"
  )

  # Find construction index in library
  i_sel <- grep(i_sel, l_HLP$HLP_constructionsOpaque$name)

  # Define labels
  lbl_grp <- "IH"
  lbl_srf <- "Interior Ceiling"
  lbl_obj <- paste0(indent, lbl_srf, add_idf_comment_custom("Name", 5))

  # Obtain construction data
  l_cons <- obtain_construction(i_sel, "opaque", lbl_grp)

  # Adjust construction label
  lbl_cons <- grep("^Construction,", l_cons$idf)
  l_cons$idf[lbl_cons] <- "Construction,"
  l_cons$idf[lbl_cons + 1] <- lbl_obj

  # Format and return results
  l_res <- list(idf = c("", l_cons$idf), summary = l_cons$table)

  return(l_res)
}



# Lofts and over-roof ----

obtain_insulation_material <- function(varThk, lbl.mat = "LF_i_fibre_roll") {
  #' @title Obtains properties for a specified insulation material
  #'
  #' @description Extracts material properties and generates IDF content for a given insulation type.
  #'
  #' @param varThk A numeric value representing the insulation thickness in millimeters.
  #' @param lbl.mat A character value specifying the desired material name (default: "LF_i_fibre_roll").
  #'
  #' @return A list containing:
  #'   - table: A data frame containing material properties.
  #'   - idf: A character vector representing the IDF content for the material.

  # Use pre-defined indentation
  indent <- the$.indent

  # Retrieve material data reference
  tblMat <- s_chm_reference[["_MatList"]]

  # Filter material data
  tblMat <- subset(tblMat, Name == "glass fibre roll insulation")
  tblMat <- as.data.frame(tblMat)

  # Update material properties
  tblMat$Name <- lbl.mat
  tblMat$Thickness..m. <- varThk / 1000
  rownames(tblMat) <- NULL
  colnames(tblMat) <- tolower(colnames(tblMat))
  colnames(tblMat) <- gsub("\\.", "_", colnames(tblMat))
  colnames(tblMat) <- gsub("\\_+$", "", colnames(tblMat))

  # Construct IDF labels
  lblMaterials <- c("", colnames(tblMat))

  # Build material property string for IDF
  varProp <- "Material"
  for (i in 1:dim(tblMat)[2]) {
    varProp <- rbind(varProp, as.character(tblMat[1, i]))
  }
  varProp <-
    as.data.frame(cbind(varProp, c(rep(c(","), each = 6), c(";"))))
  varProp$V3 <- lblMaterials
  varProp$idf.gaps <- c("", rep(indent, dim(tblMat)[2]))
  colnames(varProp) <- c("idf.values", "idf_marks", "idf.labels", "idf.gaps")
  varProp$idf.values <-
    paste(varProp$idf.gaps, varProp$idf.values)
  varProp$idf.labels <-
    paste(indent, "!-", varProp$idf.labels)
  varProp <- as.character(apply(varProp, 1, paste, collapse = ""))

  # Return results
  return(list(table = tblMat, idf = varProp))
}

update_hlp_layers <- function(l_src, d_upd) {
  #' @title Updates layer properties in a surface object
  #'
  #' @description Replaces the first layer of a surface object with updated properties and recalculates thermal resistance and transmittance.
  #'
  #' @param l_src A list representing the surface object, containing layers and thermal properties.
  #' @param d_upd A data frame containing updated layer properties.
  #'
  #' @return An updated surface object with the replaced layer and recalculated properties.

  # Extract the first layer from the surface object
  l_src$layers <- l_src$layers %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      type = "Material",
      name = d_upd$name,
      roughness = d_upd$roughness,
      thickness__m = d_upd$thickness__m,
      conductivity__w_m_k = d_upd$conductivity__w_mk,
      density__kg_m3 = d_upd$density__kg_m3,
      specific_heat__j_kg_k = d_upd$specificheat__j_kgk
    ) %>%
    dplyr::mutate(thermal_resistance__m2_k_w = thickness__m / conductivity__w_m_k) %>%
    dplyr::bind_rows(l_src$layers)

  # Update surface object with new layers and recalculate thermal properties
  l_src$R__resistance__m2K_w <-
    sum(l_src[["layers"]]$thermal_resistance__m2_k_w)
  l_src$U__transmittance__W_m2K <- 1 / l_src$R__resistance__m2K_w

  return(l_src)
}

assign_loft_material <- function(idfTo, l_IR, varThk = 270) {
  #' @title Assigns properties for loft insulation material
  #'
  #' @description Updates a surface object (l_IR) with insulation properties and creates IDF content for the loft construction.
  #'
  #' @param idfTo A character vector representing the existing IDF content.
  #' @param l_IR A list representing the surface object for the loft ceiling.
  #' @param varThk A numeric value specifying the insulation thickness in millimeters (default: 270).
  #'
  #' @return A list containing:
  #'   - summary: An updated surface object (l_IR) with insulation properties.
  #'   - idf: A character vector representing the updated IDF content for the loft.
  #'

  # Use pre-defined indentation
  indent <- the$.indent

  # Define global parameters
  varMat <- "LF_i_fibre_roll"

  # Define layer labels
  lblMat <- paste0(indent, "Loft Ceiling", add_idf_comment_custom("Name", 19))
  varLyr <- paste0(indent, varMat, add_idf_comment_custom("Layer Insulation", 11, T))

  # Obtain insulation material properties
  varProp <- obtain_insulation_material(varThk)
  df_prop <- varProp$table
  varProp <- varProp$idf

  # Find construction definition start and end lines
  ln.ma <- grep("Construction,", idfTo)[1]

  # Extract construction definition lines
  ln.mb <- data.frame(grep = regexpr(";", idfTo[ln.ma:length(idfTo)]))
  ln.mb$flag <- ifelse(ln.mb$grep < 0, 0, rownames(ln.mb))
  ln.mb <- as.integer(ln.mb$flag[ln.mb$flag > 0])[1]
  idfTo <- c(idfTo[c(ln.ma:(ln.ma + ln.mb - 1))])
  idfTo[2] <- lblMat
  idfTo <- gsub(";", ",", idfTo)

  # Update surface object
  l_IR <- update_hlp_layers(l_IR, df_prop)

  # Update IDF content
  idfTo <- c("", varProp, "", idfTo, varLyr, "")

  return(list(summary = l_IR, idf = idfTo))
}

assign_top_block_insulation <- function(idfTo, l_ER, varThk = 270) {
  #' @title Assigns properties for top block insulation material
  #'
  #' @description Updates a surface object (l_ER) with insulation properties and creates IDF content for the top block construction.
  #'
  #' @param idfTo A character vector representing the existing IDF content.
  #' @param l_ER A list representing the surface object for the top block.
  #' @param varThk A numeric value specifying the insulation thickness in millimeters (default: 270).
  #'
  #' @return A list containing:
  #'   - summary: An updated surface object (l_ER) with insulation properties.
  #'   - idf: A character vector representing the updated IDF content for the top block.

  # Use pre-defined indentation
  indent <- the$.indent

  # Define global parameters
  varMat <- "TF_i_fibre_roll"
  varLyr <- paste0(indent, varMat, add_idf_comment_custom("Layer Insulation", 11, T))

  # Obtain insulation material properties
  varProp <- obtain_insulation_material(varThk, varMat)
  df_prop <- varProp$table
  varProp <- varProp$idf

  # Find construction definition start and end lines
  ln.ma <- grep("Construction,", idfTo)[1]
  ln.mb <- data.frame(grep = regexpr(";", idfTo[ln.ma:length(idfTo)]))
  ln.mb$flag <- ifelse(ln.mb$grep < 0, 0, rownames(ln.mb))
  ln.mb <- as.integer(ln.mb$flag[ln.mb$flag > 0])[1]

  # Extract construction definition lines (excluding header and footer)
  idf.origin <- c(idfTo[c(1:(ln.ma - 1), (ln.ma + ln.mb + 1):length(idfTo))])

  # Separate original and modified sections of IDF content
  idf.change <- c(idfTo[c(ln.ma:(ln.ma + ln.mb - 1))])
  idf.change <- gsub(";", ",", idf.change)
  idf.change <- c(idf.change, varLyr)

  # Update surface object
  l_ER <- update_hlp_layers(l_ER, df_prop)

  # Combine updated IDF sections
  idfTo <- c(idf.origin, varProp, "", idf.change, "")

  return(list(summary = l_ER, idf = idfTo))
}
