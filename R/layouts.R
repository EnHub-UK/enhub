# layout module ----------------------------------------------------------------

decode_cuboid_inputs <- function(lc, ld) {
  #' @title Parse defined cuboid information
  #'
  #' @param lc list of custom cuboid inputs
  #' @param ld list of default inputs
  #'
  #' @return list of parsed attributes

  # Update list of inputs
  lc <- modifyList(ld, lc)

  # Extract relevant data from the input list
  cc <- lc$cuboid
  fr <- ifelse("flat_roof" %in% names(lc), lc$flat_roof, FALSE)
  ac <- ifelse("access" %in% names(lc), lc$access, "front")
  ly <- lc$layouts
  pg <- lc$window_to_wall_per_side

  # Clean cuboid code
  cc <- sub("MOD-", "", cc)
  storey_ref <- c("GF", paste0(1:99, "F"))

  nf <- sub("(^\\d{,2})(\\w+)", "\\1", cc) %>% as.integer()
  wa <- ifelse(sub("(^\\d{,2}\\w)(\\w)(\\w+)", "\\2", cc) == "W", TRUE, FALSE)
  wb <- ifelse(sub("(^\\d{,2}\\w{3})(\\w)(\\w+)", "\\2", cc) == "W", TRUE, FALSE)
  pa <- sub("(^\\d{,2}\\w{5})(\\w)(\\w)", "\\2", cc)

  # Extract wall presence from codes
  battach <- switch(pa,
    "L" = "left",
    "R" = "right",
    "B" = "both",
    "N" = "none"
  )

  # Assign roof type as proxy
  rpitched <- TRUE

  # Assign presence of roof-in-roof window
  win_att <- FALSE

  # Limit number of storeys
  lstorey <- storey_ref[1:nf]

  # Adjust storeys based on additional features
  if (wb) {
    lstorey <- c("BB", lstorey)
  }

  if (wa) {
    lstorey <- c(lstorey, "AT")
    win_att <- TRUE
  } else {
    if (fr == TRUE) {
      lstorey <- c(lstorey, "TF")
      rpitched <- FALSE
    } else {
      lstorey <- c(lstorey, "PR")
    }
  }

  # Parse fraction of glazed faces
  if (any(grepl("^window_to_wall$", names(lc)))) {
    pg$front <- pg$rear <- pg$left <- pg$right <- lc$window_to_wall
  } else {
    pg$front <- ifelse("front" %in% names(pg), pg$front / 100, 0)
    pg$rear <- ifelse("rear" %in% names(pg), pg$rear / 100, 0)
    pg$left <- ifelse("left" %in% names(pg), pg$left / 100, 0)
    pg$right <- ifelse("right" %in% names(pg), pg$right / 100, 0)
  }
  pg$left <- ifelse(battach %in% c("right", "none"), pg$left, 0)
  pg$right <- ifelse(battach %in% c("left", "none"), pg$right, 0)

  # Make list of processed properties
  lp <- list(
    n_storeys = nf,
    storeys = lstorey,
    attachment = battach,
    pitched_roof = rpitched,
    pitched_roof_window = win_att,
    window_to_wall_per_side = pg,
    access = ac,
    layouts = ly
  )

  # Remove updated fields
  lc$window_to_wall <- NULL
  lc$window_to_wall_per_side <- NULL
  lc$access <- NULL
  lc$layouts <- NULL

  # Create edited list
  l_res <- modifyList(lc, lp)
  l_res <- l_res[order(names(l_res))]

  # Return a list of extracted information
  return(l_res)
}

obtain_matrix_size <- function(id, l) {
  #' @title Obtain dimensions of matrix
  #'
  #' @param id selected layout
  #' @param  l list of available layouts
  #'
  #' @return list of parsed attributes

  # Define layout dimensions
  l_act <- lapply(l[unique(id)], "[[", "size")

  # Check if all dimensions are the same
  allSame <- function(x) length(unique(x)) == 1

  # Assign dimensions if they are the same
  if (allSame(l_act)) {
    m_dim <- as.vector(l_act[[1]])
  } else {
    warning(paste(
      "layout dimensions must be the same ->",
      paste(l_act, collapse = " / ")
    ))
    m_dim <- NULL
  }

  # Return dimensions
  invisible(m_dim)
}

assign_room_layout <- function(idm, dim_mtx, k_layout, k_levels, k_attach, d_room = d_rooms, d_layout = t_layouts) {
  #' @title Iteratively assign room layout and matching components
  #'
  #' @param      idm selected storey number
  #' @param  dim_mtx matrix dimensions for internal layout
  #' @param k_layout reference of available storeys/floors in building
  #' @param k_levels reference of available storeys/floors in building
  #' @param k_attach reference of attachment condition of building
  #' @param   d_room reference for available room uses
  #' @param d_layout reference for available layouts templates
  #'
  #' @return a dictionary of room types per floor

  # Helper to obtain matching blocks
  obtain_matching_blocks <- function(dim_mtx) {
    # @title Obtain Matching Blocks
    # @description This function identifies and returns pairs of matching blocks based on the given dimensions matrix.
    #
    # @param dim_mtx List. A list containing the dimensions matrix with rows and columns.
    # @return A data frame containing the matching blocks with their respective row and column indices.

    # Extract the number of rows and columns from the dimensions matrix
    dim_r <- dim_mtx$cols
    dim_c <- dim_mtx$rows

    # Create a data frame with all possible combinations of row and column indices
    d <- expand.grid(r = 1:dim_r, c = 1:dim_c)
    d$id <- paste(d$r, d$c, sep = "-")

    # Generate all possible pairs of block IDs
    d <- data.frame(t(data.frame(combn(d$id, 2))))
    colnames(d) <- c("a", "b")

    # Initialize the 'valid' column to check if pairs are identical
    d$valid <- ifelse(d$a == d$b, TRUE, FALSE)

    # Split the block IDs into separate row and column indices
    d <- cbind(
      d, t(data.frame(strsplit(as.character(d$a), "-"))),
      t(data.frame(strsplit(as.character(d$b), "-")))
    )

    # Rename the columns for clarity
    colnames(d) <- c("a", "b", "valid", "a1", "a2", "b1", "b2")

    # Calculate the differences in row and column indices between pairs
    d$diff_1 <- abs(as.integer(d$a1) - as.integer(d$b1))
    d$diff_2 <- abs(as.integer(d$a2) - as.integer(d$b2))

    # Determine if pairs are adjacent and not diagonally aligned
    d$pass <- ifelse(d$diff_1 > 1 | d$diff_2 > 1, FALSE, TRUE)

    # Filter out invalid pairs
    d <- d[d$valid == FALSE & d$pass == TRUE & d$diff_1 != d$diff_2, ]

    # Select and rename the relevant columns
    d <- d[, c("a1", "a2", "b1", "b2")]
    colnames(d) <- c("row_1", "col_1", "row_2", "col_2")

    # Create a match identifier for each pair
    d$match <- paste0("(", d$row_1, ",", d$col_1, ")", "-", "(", d$row_2, ",", d$col_2, ")")

    # Order the data frame by row indices
    d <- d[order(d$row_1, d$row_2), ]
    rownames(d) <- NULL

    return(d)
  }

  # Helper to obtain room coordinates
  obtain_room_coordinate <- function(dim_mtx) {
    # @title Generate room map, based on layout dimensions
    # @param dim_mtx dimensions of layouts
    # @return a matrix indicating name of rooms

    dim_c <- dim_mtx$cols
    dim_r <- dim_mtx$rows

    d <- expand.grid(r = 1:dim_r, c = 1:dim_c)
    d$id <- paste0("(", d$r, "-", d$c, ")")

    room_mtx <- matrix(d$id, nrow = dim_r, ncol = dim_c)
    return(room_mtx)
  }

  # Helper to obtain outer boundaries
  obtain_outer_boundaries <- function(m_ref, n_ref, n_lbl) {
    # @title Generate room map, based on layout dimensions
    # @param m_ref map of room locations
    # @param n_ref indication of attached units
    # @param n_lbl name of active storey
    # @return matrix of outer boundaries

    m_att <- NULL

    if (n_ref == "left" | n_ref == "both") {
      m_att <- gsub("^\\(", paste0("(l", n_lbl, "-"), m_ref[, 1])
    }

    if (n_ref == "right" | n_ref == "both") {
      m_att <- gsub("^\\(", paste0("(r", n_lbl, "-"), m_ref[, dim(m_ref)[2]])
    }

    m_att_opp <- m_att %>%
      gsub("^\\(r", "(l", .) %>%
      gsub("\\-\\d+\\)$", "-1)", .)

    return(list(r = m_att, l = m_att_opp))
  }

  # Helper to obtain room sequence
  obtain_room_sequence <- function(nly, nlv, dly, drm) {
    # @title Generate room map, based on layout dimensions
    # @param nly selected layout
    # @param nlv selected storey
    # @param dly dictionary of layouts
    # @param drm table of rooms
    # @return vector of applicable rooms

    k_lbl <- k_cod <- NULL

    k_sequence <- dly[[nly]]$loc

    for (k in k_sequence) {
      k_lbl <- c(k_lbl, drm$name[drm$id == k])
      k_cod <- c(k_cod, drm$id[drm$id == k])
    }
    k_lbl <- k_lbl %>% gsub("<empty>", "-", .)
    k_bools <- !grepl("^-$", k_lbl)


    if (nlv %in% c("TF", "PR")) {
      k_sequence <- rep(30, length(k_sequence))

      k_lbl <- k_cod <- NULL
      for (k in k_sequence) {
        k_lbl <- c(k_lbl, drm$name[drm$id == k])
        k_cod <- c(k_cod, drm$id[drm$id == k])
      }

      k_lbl <- paste0(k_lbl, k_bools) %>%
        gsub(".*FALSE$", "", .) %>%
        gsub("TRUE$", "", .) %>%
        gsub("^$", "-", .)
      k_cod <- paste0(k_cod, k_bools) %>%
        gsub(".*FALSE$", "0", .) %>%
        gsub("TRUE$", "", .) %>%
        gsub("^$", "0", .) %>%
        as.integer()
    }

    return(list(names = k_lbl, codes = k_cod, bool = k_bools))
  }

  # Helper to obtain matching table
  obtain_matching_table <- function(d, r_mtc, r_loc, lbl_mtc) {
    # @title Generate room map, based on layout dimensions
    # @param       d table of rooms
    # @param   r_mtc table of room connections
    # @param   r_loc selected room
    # @param lbl_mtc selected storey
    # @return list of available rooms and internal boundaries

    n_row <- dim(r_loc)[1]
    n_col <- dim(r_loc)[2]

    r_mtc$room_a <- paste0("(", r_mtc$row_1, "-", r_mtc$col_1, ")")
    r_mtc$room_b <- paste0("(", r_mtc$row_2, "-", r_mtc$col_2, ")")

    r_mtc <- merge(x = r_mtc, y = d, by.x = "room_a", by.y = "loc")
    names(r_mtc)[names(r_mtc) == "type"] <- "use_a"
    names(r_mtc)[names(r_mtc) == "code"] <- "code_a"
    r_mtc <- merge(x = r_mtc, y = d, by.x = "room_b", by.y = "loc")
    names(r_mtc)[names(r_mtc) == "type"] <- "use_b"
    names(r_mtc)[names(r_mtc) == "code"] <- "code_b"
    r_mtc <- r_mtc[, c(
      "match", "room_a", "room_b", "use_a", "use_b", "code_a", "code_b"
    )]
    r_mtc$void <- ifelse(r_mtc$code_a == r_mtc$code_b, TRUE, FALSE)
    r_mtc$match <- NULL

    lbl_ref <- paste0("(", lbl_mtc, "-")
    r_base <- matrix(gsub("^\\(", lbl_ref, as.character(r_loc)),
      nrow = n_row, ncol = n_col
    )
    r_mtc$room_a <- gsub("^\\(", lbl_ref, r_mtc$room_a)
    r_mtc$room_b <- gsub("^\\(", lbl_ref, r_mtc$room_b)

    r_mtc <- r_mtc %>%
      dplyr::filter(use_b != "-") %>%
      dplyr::filter(use_a != "-")

    return(list(base = r_base, matches = r_mtc))
  }

  # Load object parameters
  n_layout <- k_layout[idm]
  n_level <- k_levels[idm]

  l <- list()

  room_mtc <- obtain_matching_blocks(dim_mtx)

  room_loc <- obtain_room_coordinate(dim_mtx)

  room_att <- obtain_outer_boundaries(room_loc, k_attach, n_level)

  l_roomseq <- obtain_room_sequence(n_layout, n_level, d_layout, d_room)

  n_row <- dim(room_loc)[1]
  n_col <- dim(room_loc)[2]

  if ((dim_mtx$cols * dim_mtx$rows) == length(l_roomseq$codes)) {
    room_mtx <- matrix(l_roomseq$names, nrow = n_row, ncol = n_col)
    room_bool <- matrix(l_roomseq$bool, nrow = n_row, ncol = n_col)

    d_loc <- data.frame(loc = as.vector(room_loc), type = l_roomseq$names, code = l_roomseq$codes)

    l_match <- obtain_matching_table(d_loc, room_mtc, room_loc, n_level)
    room_mtc <- l_match$matches
    room_base <- l_match$base

    k_bool <- paste0(room_base, room_bool) %>%
      grep(".*FALSE$", ., value = T) %>%
      gsub("FALSE$", "", .)

    # use attached elements

    # .. find opposite element for each floor
    if (match(n_level, k_levels) == 1) {
      lbl_ref <- "ground"

      lbl_to_assign <- gsub(".*", lbl_ref, as.character(room_loc))
      lbl_to_assign <- paste0(lbl_to_assign, l_roomseq$bool) %>%
        gsub(".*FALSE$", "", .) %>%
        gsub("TRUE$", "", .) %>%
        gsub("^$", "-", .)

      room_mtc_beneath <- matrix(lbl_to_assign, nrow = n_row, ncol = n_col)
    } else {
      lbl_ref <- k_levels[match(n_level, k_levels) - 1]
      lbl_ref <- paste0("(", lbl_ref, "-")

      lbl_to_assign <- gsub("^\\(", lbl_ref, as.character(room_loc))
      lbl_to_assign <- paste0(lbl_to_assign, l_roomseq$bool) %>%
        gsub(".*FALSE$", "", .) %>%
        gsub("TRUE$", "", .) %>%
        gsub("^$", "-", .)

      room_mtc_beneath <- matrix(lbl_to_assign, nrow = n_row, ncol = n_col)
    }

    # .. find opposite element for each ceiling/roof
    if (match(n_level, k_levels) == length(k_levels)) {
      lbl_ref <- "top"

      lbl_to_assign <- gsub(".*", lbl_ref, as.character(room_loc))
      lbl_to_assign <- paste0(lbl_to_assign, l_roomseq$bool) %>%
        gsub(".*FALSE$", "", .) %>%
        gsub("TRUE$", "", .) %>%
        gsub("^$", "-", .)

      room_mtc_above <- matrix(lbl_to_assign, nrow = n_row, ncol = n_col)
    } else {
      lbl_ref <- k_levels[match(n_level, k_levels) + 1]
      lbl_ref <- paste0("(", lbl_ref, "-")

      lbl_to_assign <- gsub("^\\(", lbl_ref, as.character(room_loc))
      lbl_to_assign <- paste0(lbl_to_assign, l_roomseq$bool) %>%
        gsub(".*FALSE$", "", .) %>%
        gsub("TRUE$", "", .) %>%
        gsub("^$", "-", .)

      room_mtc_above <- matrix(lbl_to_assign, nrow = n_row, ncol = n_col)
    }


    # .. find opposite element for each front face
    row_mtc <- NULL
    for (i in 1:n_row) {
      if (i < n_row) {
        lbl_ref <- paste0("(", n_level, "-")
        row_ref <- gsub("^\\(", lbl_ref, as.character(room_loc[i + 1, ]))
      } else {
        lbl_ref <- "exterior"
        row_ref <- gsub(".*", lbl_ref, as.character(room_loc[i, ]))
      }
      row_mtc <- c(row_mtc, row_ref)
    }
    room_mtc_front <- matrix(row_mtc, byrow = TRUE, nrow = n_row, ncol = n_col)


    # .. find opposite element for each rear face
    row_mtc <- NULL
    for (i in 1:n_row) {
      if (i > 1) {
        lbl_ref <- paste0("(", n_level, "-")
        row_ref <- gsub("^\\(", lbl_ref, as.character(room_loc[i - 1, ]))
      } else {
        lbl_ref <- "exterior"
        row_ref <- gsub(".*", lbl_ref, as.character(room_loc[i, ]))
      }
      row_mtc <- c(row_mtc, row_ref)
    }
    room_mtc_rear <- matrix(row_mtc, byrow = TRUE, nrow = n_row, ncol = n_col)


    # .. find opposite element for each right face
    col_mtc <- NULL
    for (i in 1:n_col) {
      if (i < n_col) {
        lbl_ref <- paste0("(", n_level, "-")
        col_ref <- gsub("^\\(", lbl_ref, as.character(room_loc[, i + 1]))
      } else {
        lbl_ref <- "exterior"
        col_ref <- gsub(".*", lbl_ref, as.character(room_loc[, i]))
      }
      col_mtc <- c(col_mtc, col_ref)
    }
    room_mtc_right <- matrix(col_mtc, byrow = FALSE, nrow = n_row, ncol = n_col)
    if (k_attach == "right" | k_attach == "both") {
      room_mtc_right[, dim(room_mtc_right)[2]] <- room_att$r
    }


    # .. find opposite element for each left face
    col_mtc <- NULL
    for (i in 1:n_col) {
      if (i > 1) {
        lbl_ref <- paste0("(", n_level, "-")
        col_ref <- gsub("^\\(", lbl_ref, as.character(room_loc[, i - 1]))
      } else {
        lbl_ref <- "exterior"
        col_ref <- gsub(".*", lbl_ref, as.character(room_loc[, i]))
      }
      col_mtc <- c(col_mtc, col_ref)
    }
    room_mtc_left <- matrix(col_mtc, byrow = FALSE, nrow = n_row, ncol = n_col)
    if (k_attach == "left" | k_attach == "both") {
      room_mtc_left[, 1] <- room_att$l
    }


    # confirm base map
    lbl_mtx_pass <- paste0(room_base, room_bool) %>%
      gsub(".*FALSE$", "", .) %>%
      gsub("TRUE$", "", .) %>%
      gsub("^$", "-", .)
    room_base <- matrix(lbl_mtx_pass, nrow = n_row, ncol = n_col)

    # confirm side facades
    for (k in k_bool) {
      room_mtc_front <-
        gsub(k, "exterior", room_mtc_front, fixed = T)
    }
    for (k in k_bool) {
      room_mtc_rear <-
        gsub(k, "exterior", room_mtc_rear, fixed = T)
    }
    for (k in k_bool) {
      room_mtc_right <-
        gsub(k, "exterior", room_mtc_right, fixed = T)
    }
    for (k in k_bool) {
      room_mtc_left <-
        gsub(k, "exterior", room_mtc_left, fixed = T)
    }

    # .. compose list for storey
    l <- list(
      room_location = room_base,
      room_assignment = room_mtx,
      conjoined_zones = room_mtc,
      room_floor_links_to = room_mtc_beneath,
      room_ceiling_links_to = room_mtc_above,
      room_front_links_to = room_mtc_front,
      room_rear_links_to = room_mtc_rear,
      room_right_links_to = room_mtc_right,
      room_left_links_to = room_mtc_left
    )
  } else {
    warning("size of layout and given layout do NOT match", call. = FALSE)
  }

  return(l)
}

generate_room_layout <- function(idr, l_lay, l_axial, l_template = t_layouts) {
  #' @title Generate loom layout dictionary
  #'
  #' @param         id label for selected story/floor
  #' @param      l_lay building layout info
  #' @param    l_axial list of axial dimensions
  #' @param l_template tupple of templates of layouts
  #'
  #' @return a dictionary of layouts

  # Helper to obtain bounding box coordinates
  get_surface_bbox <- function(w, d, h, c) {
    # Calculate the Bounding Box Coordinates for a 3D Surface
    #
    # This function calculates the bounding box coordinates for a 3D surface based on the given width, depth, height, and starting coordinate.
    #
    # @param w Numeric. The width of the bounding box.
    # @param d Numeric. The depth of the bounding box.
    # @param h Numeric. The height of the bounding box.
    # @param c Numeric vector of length 3. The starting coordinates (x, y, z).
    #
    # @return A list containing three faces of the bounding box:
    # \describe{
    #   \item{face_transverse}{A list of four points representing the transverse face.}
    #   \item{face_coronal}{A list of four points representing the coronal face.}
    #   \item{face_sagittal}{A list of four points representing the sagittal face.}
    # }
    # @export
    #
    # @examples
    # get_surface_bbox(2, 3, 4, c(0, 0, 0))

    # message('anticlockwise coordinates')

    p1 <- c(c[1], c[2], c[3])
    p2 <- c(c[1] + w, c[2], c[3])
    p3 <- c(c[1] + w, c[2] + d, c[3])
    p4 <- c(c[1], c[2] + d, c[3])

    p1a <- c(c[1], c[2], c[3])
    p2a <- c(c[1] + w, c[2], c[3])
    p3a <- c(c[1] + w, c[2], c[3] + h)
    p4a <- c(c[1], c[2], c[3] + h)

    p1b <- c(c[1], c[2], c[3])
    p2b <- c(c[1], c[2] + d, c[3])
    p3b <- c(c[1], c[2] + d, c[3] + h)
    p4b <- c(c[1], c[2], c[3] + h)

    lh <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
    la <- list(p1 = p1a, p2 = p2a, p3 = p3a, p4 = p4a)
    lb <- list(p1 = p1b, p2 = p2b, p3 = p3b, p4 = p4b)

    l <- list(face_transverse = lh, face_coronal = la, face_sagittal = lb)

    return(l)
  }

  # Helper to generate cuboid coordinates
  generate_cuboid <- function(storey, faces, ref_storey) {
    # @title Generate cuboid coordinates
    # @param     storey selected storey level
    # @param      faces list of faces dimensions
    # @param ref_storey vector of available storeys
    # @return dictionary of coordinates

    lbl_coord <- c("x", "y", "z")


    assign_rownames_to_ordered_list <- function(dr) {
      # @title Assign Row Names to Ordered List
      # @description This function takes a data frame and assigns its row names to an ordered list.
      #
      # @param dr A data frame whose row names will be assigned to an ordered list.
      # @return A named list where each element is a row from the data frame.

      # Split the data frame into a list of rows
      row_list <- split(dr, seq(nrow(dr)))

      # Assign row names to the list
      named_list <- setNames(row_list, rownames(dr))

      return(named_list)
    }

    reverse_coordinates <- function(l) {
      # @title Reverse Coordinates
      # @description This function reverses the order of a list and retains the original names.
      #
      # @param l A list whose order will be reversed.
      # @return A list with reversed order but original names.

      # Store the original names of the list
      lbl_right <- names(l)

      # Reverse the order of the list
      l <- rev(l)

      # Reassign the original names to the reversed list
      names(l) <- lbl_right

      return(l)
    }

    lvl <- match(storey, ref_storey)

    d_horiz <- t(as.data.frame(faces$face_transverse))
    colnames(d_horiz) <- lbl_coord
    d_horiz[, 3] <- d_horiz[, 3] + faces$face_sagittal$p3[3] * (lvl - 1)
    g_floor <- assign_rownames_to_ordered_list(d_horiz)

    d_horiz <- t(as.data.frame(faces$face_transverse))
    colnames(d_horiz) <- lbl_coord
    d_horiz[, 3] <- d_horiz[, 3] + faces$face_sagittal$p3[3] * (lvl)
    g_ceiling <- reverse_coordinates(assign_rownames_to_ordered_list(d_horiz))

    d_front <- t(as.data.frame(faces$face_coronal))
    colnames(d_front) <- lbl_coord
    d_front[, 3] <- d_front[, 3] + faces$face_sagittal$p4[3] * (lvl - 1)
    g_front <- assign_rownames_to_ordered_list(d_front)

    d_rear <- t(as.data.frame(faces$face_coronal))
    colnames(d_rear) <- lbl_coord
    d_rear[, 2] <- d_rear[, 2] + faces$face_sagittal$p2[2]
    d_rear[, 3] <- d_rear[, 3] + faces$face_sagittal$p4[3] * (lvl - 1)
    g_rear <- reverse_coordinates(assign_rownames_to_ordered_list(d_rear))

    d_right <- t(as.data.frame(faces$face_sagittal))
    colnames(d_right) <- lbl_coord
    d_right[, 3] <- d_right[, 3] + faces$face_sagittal$p4[3] * (lvl - 1)
    d_right[, 1] <- d_right[, 1] + faces$face_coronal$p2[1]
    g_right <- reverse_coordinates(assign_rownames_to_ordered_list(d_right))

    d_left <- t(as.data.frame(faces$face_sagittal))
    colnames(d_left) <- lbl_coord
    d_left[, 3] <- d_left[, 3] + faces$face_sagittal$p4[3] * (lvl - 1)
    g_left <- assign_rownames_to_ordered_list(d_left)

    # pass floor dimensions
    floor_w <- data.frame(g_floor)[1, ]
    floor_w <- max(floor_w) - min(floor_w)
    floor_d <- data.frame(g_floor)[2, ]
    floor_d <- max(floor_d) - min(floor_d)
    floor_h <- data.frame(g_front)[3, ]
    floor_h <- max(floor_h) - min(floor_h)

    l <- list(
      .w = floor_w,
      .d = floor_d,
      .h = floor_h,
      floor = g_floor,
      ceiling = g_ceiling,
      front = g_front,
      rear = g_rear,
      right = g_right,
      left = g_left
    )

    return(l)
  }

  # Helper to assign boundary elements
  assign_boundary_elements <- function(lb, id) {
    mtx <- dim(lb$room_location)

    # .. process exterior elements for top-floors
    if (id %in% c("TF", "AT", "PR")) {
      lb$room_ceiling_links_to <-
        matrix("exterior", nrow = mtx[1], ncol = mtx[2])
    }

    # .. process exterior elements for basement/cellars
    if (id %in% c("BB")) {
      lb$room_front_links_to[lb$room_front_links_to == "exterior"] <- "ground"
      lb$room_rear_links_to[lb$room_rear_links_to == "exterior"] <- "ground"
      lb$room_right_links_to[lb$room_right_links_to == "exterior"] <- "ground"
      lb$room_left_links_to[lb$room_left_links_to == "exterior"] <- "ground"
      lb$room_floor_links_to[lb$room_floor_links_to == "exterior"] <- "ground"
      lb$room_ceiling_links_to[lb$room_ceiling_links_to == "exterior"] <- "ground"
    }

    return(lb)
  }

  # Helper to assign floor origins
  assign_floor_origins <- function(id, l_lays, l_vert, dim_mtx, l_axi) {
    # Assign Floor Origins
    #
    # This function assigns the origins for each floor based on the given layout, vertical faces, dimensions, and axial blocks.
    #
    # @param id Character. The identifier for the floor (e.g., 'GF').
    # @param l_lays List. A list containing the storeys information.
    # @param l_vert List. A list containing the vertical faces information.
    # @param dim_mtx List. A list containing the dimensions matrix with rows and columns.
    # @param l_axi List. A list containing the axial block widths.
    #
    # @return A data frame containing the xyz origins for each segment of the floor.
    # @export
    #
    # @examples
    # assign_floor_origins('GF', l_lays, l_vert, dim_mtx, l_axi)

    floors <- l_lays$storeys

    # define xy dimensions
    y_seg <- dim_mtx$rows
    x_seg <- dim_mtx$cols

    # define x origin
    x_range <- data.frame(l_vert$face_transverse)[1, ]
    x_seg <- (max(x_range) - min(x_range)) / x_seg

    x_ini <- 0
    for (k in 1:(dim_mtx$cols - 1)) {
      x_ini <- c(x_ini, l_axi$w_block[k] + x_ini[k])
    }

    # define y origin
    y_range <- data.frame(l_vert$face_transverse)[2, ]
    y_seg <- (max(y_range) - min(y_range)) / dim_mtx$rows

    y_ini <- NULL
    for (k in 1:dim_mtx$rows) {
      y_ini <- c(y_ini, (k - 1) * y_seg)
    }

    # define z origin
    lvl <- match(id, floors) - match("GF", floors)
    zo <- max(l_vert$face_sagittal$p4) * lvl

    # define xyz origin
    l_ori <- l_orix <- list()
    for (yi in 1:dim_mtx$rows) {
      for (xi in 1:dim_mtx$cols) {
        l_orix[[xi]] <- c(x_ini[xi], y_ini[yi], zo)
      }
      l_ori[[yi]] <- l_orix
    }
    l_ori <- rev(l_ori)
    d_ori <- as.data.frame(do.call(rbind, l_ori))

    # reformat xyz origin
    colnames(d_ori) <- paste0("(", id, "-n-", 1:dim_mtx$cols, ")")
    rownames(d_ori) <- paste0("(", id, "-", 1:dim_mtx$rows, "-m)")

    return(d_ori)
  }


  # assign input variables
  idn <- match(idr, l_lay$storeys)
  dim_mtx <- obtain_matrix_size(l_lay$layouts, l_template)
  ref_level <- l_lay$storeys
  ref_attach <- l_lay$attachment
  ref_layout <- l_lay$layouts

  # include roof void dummy-layout
  if (ref_level[idn] %in% c("TF", "PR", "AT") & length(ref_layout) < length(ref_level)) {
    ref_layout <- c(ref_layout, ref_layout[length(ref_layout)])
  }

  # .. estimate axial dimensions
  l_faces <- get_surface_bbox(l_axial$w, l_axial$d, l_axial$h, l_axial$c)

  # .. assign vertices for each of the faces
  d_vertices <- list(generate_cuboid(ref_level[1], l_faces, ref_level))
  d_vertices <- rep(d_vertices, length(ref_level))
  names(d_vertices) <- ref_level

  # .. obtain matches for each of the faces
  l_blocks <- assign_room_layout(
    idn, dim_mtx, ref_layout, ref_level, ref_attach
  )

  # .. re-assign boundary elements for top and underground blocks
  l_blocks <- assign_boundary_elements(l_blocks, idr)

  # .. process vertices in function of storeys
  l_blocks[["faces"]] <- d_vertices[[idr]]

  # .. include origin matrices
  l_blocks[["origins"]] <-
    assign_floor_origins(idr, l_lay, l_faces, dim_mtx, l_axial)

  return(l_blocks)
}

assign_vertical_voids <- function(l_romly) {
  #' @title Assign rooms with vertical inter-connections or voids.
  #' Currently only 'Stairwell' zones are considered.
  #'
  #' @param l_romly list of layout parameters
  #'
  #' @return updated list of layout parameters

  # Helper to compare layout assignments
  compare_layout_assignment <- function(i_ly, l_rly) {
    # @title Compare Layout Assignment
    # @description This function compares the room assignments between two consecutive layouts and identifies matching zones.
    #
    # @param i_ly Index of the current layout.
    # @param l_rly List containing the layout details for each storey.
    # @return A data frame with matching room assignments between the two layouts.

    # Extract bottom and top layouts
    l_bot <- l_rly[[i_ly]]
    l_top <- l_rly[[i_ly + 1]]

    # Define the zone to compare
    zone_to <- "Stairwell"
    zone_use <- 9

    # Find matches in room assignments
    k_match_bot <- grep(zone_to, l_bot$room_assignment)
    k_match_top <- grep(zone_to, l_top$room_assignment)

    if (length(k_match_bot) > 0 & length(k_match_top) > 0) {
      k_matches <- k_match_bot[match(k_match_bot, k_match_top)]
      k_matches <- k_matches[!is.na(k_matches)]

      # Create data frame for matches
      df_match <- data.frame(
        room_a = l_bot$room_location[k_matches],
        room_b = l_top$room_location[k_matches],
        use_a = zone_to, use_b = zone_to,
        code_a = zone_use, code_b = zone_use,
        void = TRUE
      )

      # Reverse the match data frame
      df_match_rev <- df_match %>%
        dplyr::mutate(room_c = room_b) %>%
        dplyr::mutate(room_b = room_a) %>%
        dplyr::mutate(room_a = room_c) %>%
        dplyr::select(-room_c)

      # Add storey information
      df_match <- df_match %>% dplyr::mutate(storey = names(l_rly)[i_ly])
      df_match_rev <- df_match_rev %>% dplyr::mutate(storey = names(l_rly)[i_ly + 1])

      # Combine match data frames
      df_res <- df_match %>%
        dplyr::bind_rows(df_match_rev) %>%
        tibble::tibble()
    } else {
      df_res <- NULL
    }

    return(df_res)
  }

  update_layout_assignment <- function(i_ly, l_rly, d_mtc) {
    # @title Update Layout Assignment
    # @description This function updates the layout assignments by adding new conjoined zones.
    #
    # @param i_ly Index of the current layout.
    # @param l_rly List containing the layout details for each storey.
    # @param d_mtc Data frame containing the matching zones.
    # @return A list with updated conjoined zones.

    # Extract current conjoined zones
    df_to <- l_rly[[i_ly]]$conjoined_zones

    # Update conjoined zones if there are matches for the current storey
    if (i_ly %in% unique(d_mtc$storey)) {
      df_with <- d_mtc %>%
        dplyr::filter(grepl(i_ly, storey)) %>%
        dplyr::select(-storey)
      df_to <- df_to %>% dplyr::bind_rows(df_with)
    }

    return(list(conjoined_zones = df_to))
  }

  # Create iterator, using intersecting layers (ie. storeys - 1)
  k_it <- 1:(length(names(l_romly)) - 1)

  # Create table with vertical inter-connections
  d_matches <- lapply(k_it, compare_layout_assignment, l_romly)
  d_matches <- d_matches[lengths(d_matches) > 0L]
  d_matches <- plyr::ldply(d_matches, data.frame) %>% dplyr::distinct()

  # Create list of `conjoined_zones` tables
  l_matches <- lapply(names(l_romly), update_layout_assignment, l_romly, d_matches)
  names(l_matches) <- names(l_romly)

  # Update `conjoined_zones` table
  for (i in names(l_matches)) {
    l_romly[[i]]$conjoined_zones <- l_matches[[i]]$conjoined_zones
  }

  return(l_romly)
}

obtain_dimensions <- function(b_cub, rir_h = 1.125, rtv_h = 0.25, l_template = t_layouts) {
  #' @title Obtain detailed list of geometrical parameters of house
  #'
  #' @param       b_cub list of geometrical input-parameters
  #' @param       rir_h room-in-roof height
  #' @param       rtv_h top-roof/void height
  #' @param  l_template tuple of tupple of available layouts
  #'
  #' @return list of detailed geometrical parameters

  b_w <- b_cub$width
  b_d <- b_cub$depth
  b_sh <- b_cub$floor_height
  b_w2w <- b_cub$window_to_wall_per_side
  b_c <- b_cub$origin
  b_sto <- b_cub$storeys
  b_att <- b_cub$attachment

  # Helper to obtain matrix size
  assign_row_col_dimensions <- function(kcub, klay, kw, kd, kmtx, narrow = 0.4) {
    # @title Assign Row and Column Dimensions
    # @description This function assigns the dimensions (widths and depths) for rows and columns in a layout matrix.
    #
    # @param kcub A list containing the storeys and layouts.
    # @param klay A list containing the layout templates.
    # @param kw The total width to be distributed among columns.
    # @param kd The total depth to be distributed among rows.
    # @param kmtx A list containing the dimensions matrix with rows and columns.
    # @param narrow A numeric value indicating the narrowing factor for specific columns.
    # @return A list with the calculated widths and depths for the layout matrix.

    # Determine the position of the ground floor (GF) or default to the first storey
    if (any(grepl("GF", kcub$storeys))) {
      kpos <- match("GF", kcub$storeys)
    } else {
      kpos <- 1
    }

    # Get the active layout for the determined position
    kact <- kcub$layouts[kpos]
    klay[[kact]]

    # Create a matrix of room locations
    krom <- matrix(klay[[kact]]$loc, nrow = kmtx$rows, ncol = kmtx$cols)

    # Identify columns that need to be narrowed
    krob <- ifelse(krom == 7 | krom == 9, TRUE, FALSE)
    knarrow <- which.max(colSums(krob))

    # Calculate column widths
    kwidths <- rep(1, kmtx$cols)
    kwidths[knarrow] <- kwidths[knarrow] * narrow
    kwidths <- kw * (kwidths / sum(kwidths))

    # Calculate row depths
    kdepths <- kd * rep(1, kmtx$rows) / sum(rep(1, kmtx$rows))

    return(list(widths = kwidths, depths = kdepths))
  }

  dim_mtx <- obtain_matrix_size(b_cub$layouts, l_template)
  l_rc_dim <- assign_row_col_dimensions(b_cub, l_template, b_w, b_d, dim_mtx)
  b_w_block <- l_rc_dim$widths
  b_d_block <- l_rc_dim$depths

  h_sto <- !grepl("^B|^A|^T|^P", b_sto)
  h_att <- grepl("^A|^P", b_sto)
  h_tfv <- grepl("^T", b_sto)
  h_rir <- ifelse(b_sto[length(b_sto)] %in% c("AT", "PR"), rir_h, rtv_h)

  h_storeys_core <- sum(h_sto * b_sh)
  h_storeys <- sum(h_sto * b_sh + h_att * rir_h + h_tfv * rtv_h)

  f_cor <- 2
  f_sag <- switch(b_att,
    "left" = 1,
    "right" = 1,
    "both" = 0,
    "none" = 2
  )

  g_coronal <- b_w * f_cor
  g_sagittal <- b_d * f_sag
  g_exposed <- g_coronal + g_sagittal

  a_coronal <- h_storeys_core * g_coronal
  a_sagittal <- h_storeys_core * g_sagittal
  a_exposed <- a_coronal + a_sagittal

  a_glazed_coronal <- a_coronal / f_cor * b_w2w$front + a_coronal / f_cor * b_w2w$rear
  if (f_sag > 0) {
    a_glazed_sagittal <- a_sagittal / f_sag * b_w2w$left + a_sagittal / f_sag * b_w2w$right
  } else {
    a_glazed_sagittal <- 0
  }
  a_glazed <- a_glazed_coronal + a_glazed_sagittal

  r_w2d <- b_w / b_d
  r_h2w <- b_sh / b_w
  r_h2d <- b_sh / b_d
  r_ex_c2s <- a_glazed_coronal / a_glazed_sagittal

  l_dim <- list(
    w = b_w, d = b_d, h = b_sh, c = b_c, h_roomroof = h_rir,
    w_block = b_w_block, d_block = b_d_block,
    height_vertical = h_storeys,
    height_core = h_storeys_core,
    ratio_lateral_to_front = r_w2d,
    ratio_height_to_width = r_h2w,
    ratio_height_to_depth = r_h2d,
    area_vertical = a_exposed,
    area_vertical_glazed = a_glazed,
    area_vertical_glazed_sagittal = a_glazed_sagittal,
    area_vertical_glazed_coronal = a_glazed_coronal,
    ratio_exposed_coronal_to_sagittal = r_ex_c2s
  )

  return(l_dim)
}

obtain_glazing_areas <- function(l_lay, l_dmg, l_cbd) {
  #' @title Obtain glazing dimensions across the house
  #'
  #' @param l_lay list of existing room layouts
  #' @param l_dmg list of major dimensions
  #' @param l_cbd list of basic geometrical parameters
  #'
  #' @return list of glazing areas

  # Helper to obtain glazing canvas
  obtain_glazing_canvas <- function(l_b, l_c) {
    # @title Obtain Glazing Canvas
    # @description This function identifies the glazing canvas for a building layout by determining the exterior faces.
    #
    # @param l_b A list containing the layout details for a specific floor (e.g., ground floor).
    # @param l_c A list containing the building configuration details.
    # @return A list with the main and mirrored glazing canvas.

    # Identify room/space
    s_type <- l_b$room_location %>%
      as.character() %>%
      grep("^-$", ., invert = TRUE, value = TRUE)
    k_door_face <- l_c$access

    # Extract conditions for each face
    m_front <- l_b$room_front_links_to
    m_rear <- l_b$room_rear_links_to
    m_left <- l_b$room_left_links_to
    m_right <- l_b$room_right_links_to

    if (k_door_face %in% c("front", "rear")) {
      if (k_door_face == "front") {
        m_front <- m_front[dim(m_front)[1], ]
        if (grepl("^\\(GF", s_type[1])) m_front[1] <- "access"
        m_rear <- m_rear[1, ]
      } else {
        m_rear <- m_rear[1, ]
        if (grepl("^\\(GF", s_type[1])) m_rear[1] <- "access"
        m_front <- m_front[dim(m_front)[1], ]
      }
      m_left <- m_left[, 1]
      m_right <- m_right[, dim(m_right)[2]]
    }

    m_front <- m_front %in% "exterior"
    m_rear <- m_rear %in% "exterior"
    m_left <- m_left %in% "exterior"
    m_right <- m_right %in% "exterior"

    # Generate list of areas and building properties
    l_canvas <- list(front = m_front, rear = m_rear, right = m_right, left = m_left)
    l_mirror <- l_canvas

    if ((any(m_right) & !any(m_left)) | (!any(m_right) & any(m_left))) {
      l_mirror$right <- l_canvas$left
      l_mirror$left <- l_canvas$right
      l_mirror$front <- rev(l_canvas$front)
      l_mirror$rear <- rev(l_canvas$rear)
    }

    if ((any(m_front) & !any(m_rear)) | (!any(m_front) & any(m_rear))) {
      l_mirror$front <- l_canvas$rear
      l_mirror$rear <- l_canvas$front
      l_mirror$right <- rev(l_canvas$right)
      l_mirror$left <- rev(l_canvas$left)
    }

    return(list(main = l_canvas, mirror = l_mirror))
  }

  obtain_glazing_ratios <- function(lcdb) {
    # @title Obtain Glazing Ratios
    # @description This function calculates the glazing ratios for each side of the building based on window-to-wall ratios.
    #
    # @param lcdb A list containing the window-to-wall ratios for each side of the building.
    # @return A list with the glazing ratios for the front, rear, right, and left sides.

    # Extract window-to-wall ratios for each side
    k_fro <- lcdb$window_to_wall_per_side$front
    k_rea <- lcdb$window_to_wall_per_side$rear
    k_rig <- lcdb$window_to_wall_per_side$right
    k_lef <- lcdb$window_to_wall_per_side$left

    # Replace non-positive ratios with NA
    k_fro <- ifelse(k_fro <= 0, NA, k_fro)
    k_rea <- ifelse(k_rea <= 0, NA, k_rea)
    k_rig <- ifelse(k_rig <= 0, NA, k_rig)
    k_lef <- ifelse(k_lef <= 0, NA, k_lef)

    # Calculate mean ratios for correction
    k_ratio_cor <- mean(c(k_fro, k_rea), na.rm = TRUE)
    k_ratio_sag <- mean(c(k_rig, k_lef), na.rm = TRUE)

    # Calculate relative ratios
    r_fro <- k_fro / k_ratio_cor
    r_rea <- k_rea / k_ratio_cor
    r_rig <- k_rig / k_ratio_sag
    r_lef <- k_lef / k_ratio_sag

    # Replace NA ratios with 0
    r_fro <- ifelse(is.na(r_fro), 0, r_fro)
    r_rea <- ifelse(is.na(r_rea), 0, r_rea)
    r_rig <- ifelse(is.na(r_rig), 0, r_rig)
    r_lef <- ifelse(is.na(r_lef), 0, r_lef)

    return(list(front = r_fro, rear = r_rea, right = r_rig, left = r_lef))
  }

  # Filter applicable storeys
  k_storeys <- l_cbd$storeys %>%
    .[. != "BB"] %>%
    .[. != "TF"] %>%
    .[. != "PR"]

  # Determine candidate surfaces for hosting a window
  l_glazed_rooms <- lapply(l_lay, obtain_glazing_canvas, l_cbd)

  # Separe between main unit and attached unit, even if absent
  l_glazed_faces <- lapply(l_glazed_rooms, "[[", "main")
  l_glazed_secaf <- lapply(l_glazed_rooms, "[[", "mirror")

  # Remove incompatible storey-surfaces
  l_glazed_faces <- l_glazed_faces[!grepl("^B|^A|^T", names(l_glazed_faces))]
  l_glazed_secaf <- l_glazed_secaf[!grepl("^B|^A|^T", names(l_glazed_secaf))]

  # Determine location of roof-window, if any
  if ("AT" %in% names(l_lay)) {
    k_at_host <- l_dmg$w_block
    k_at_pos <- grep(max(k_at_host), k_at_host)[1]

    k_at_host <- rep(FALSE, length(k_at_host))
    k_at_host[k_at_pos] <- TRUE

    l_glazed_faces[["AT"]][["pitch"]] <- k_at_host
    l_glazed_secaf[["AT"]][["pitch"]] <- k_at_host
  } else {
    l_glazed_pitch <- NULL
  }

  # Count number of windows per axis: coronal & sagittal
  v_g_front <- lapply(l_glazed_faces, "[[", "front")
  v_g_rear <- lapply(l_glazed_faces, "[[", "rear")
  v_g_coronal <- sum(c(unlist(v_g_front), unlist(v_g_rear)))

  v_g_left <- lapply(l_glazed_faces, "[[", "left")
  v_g_right <- lapply(l_glazed_faces, "[[", "right")
  v_g_sagittal <- sum(c(unlist(v_g_left), unlist(v_g_right)))

  # Assign glazed areas for both axes
  l_dmg$area_face_glazed_sagittal <-
    l_dmg$area_vertical_glazed_sagittal / v_g_sagittal
  l_dmg$area_face_glazed_coronal <-
    l_dmg$area_vertical_glazed_coronal / v_g_coronal


  # Assign ratio for final adjustment fo window to wall
  l_glazing_ratios <- obtain_glazing_ratios(l_cbd)
  l_dmg$ratio_glazed_coronal_to_front <- l_glazing_ratios$front
  l_dmg$ratio_glazed_coronal_to_rear <- l_glazing_ratios$rear
  l_dmg$ratio_glazed_sagittal_to_right <- l_glazing_ratios$right
  l_dmg$ratio_glazed_sagittal_to_left <- l_glazing_ratios$left

  # Generate list of dimensions
  l_out <- list(
    dim = l_dmg,
    glazed_faces = l_glazed_faces,
    glazed_reversed = l_glazed_secaf,
    applicable = k_storeys
  )

  return(l_out)
}

obtain_glazing_dimensions <- function(i_rly, l_rly, l_gla, k_main = TRUE) {
  #' @title Obtain glazing dimensions across the house
  #'
  #' @param   i_rly list of storeys
  #' @param   l_rly list of room layouts
  #' @param   l_gla list of glazing parameters
  #' @param  k_main indication of building-unit
  #'
  #' @return list of estimated glazing dimensions

  # Load parameters
  l_dix <- l_gla$dim
  l_rly <- l_rly[[i_rly]]
  m_loc <- l_rly$room_location

  k_wd <- l_gla$dim$w_block
  k_dd <- l_gla$dim$d_block

  if (k_main == TRUE) {
    l_glz <- l_gla$glazed_faces[[i_rly]]
  } else {
    l_glz <- l_gla$glazed_reversed[[i_rly]]
    m_loc <- m_loc[, c(dim(m_loc)[2]:1), drop = FALSE]
    k_wd <- rev(k_wd)
    r_gl_a <- l_dix$ratio_glazed_sagittal_to_right
    r_gl_b <- l_dix$ratio_glazed_sagittal_to_left
    l_dix$ratio_glazed_sagittal_to_right <- r_gl_b
    l_dix$ratio_glazed_sagittal_to_left <- r_gl_a
  }

  # .. return effective areas for each room-surface

  if (i_rly %in% c("AT", "PR")) {
    l_canvas_pitch <- l_glz$pitch * l_dix$area_face_glazed_coronal * 0.5
    names(l_canvas_pitch) <- m_loc[dim(m_loc)[1], ]

    # .. generate list of areas and building properties
    l_canvas <- list(
      alpha = l_rly$faces$.alpha,
      pitch_front = as.list(l_canvas_pitch)
    )
  } else {
    # assign candidate front faces
    l_canvas_front <- l_glz$front * l_dix$area_face_glazed_coronal * l_dix$ratio_glazed_coronal_to_front
    if (sum(l_glz$front * k_wd) > 0) {
      l_canvas_front <-
        sum(l_canvas_front) * l_glz$front * k_wd / sum(l_glz$front * k_wd)
    }
    i <- 1
    lbl_front_glazed <- NULL
    for (i in 1:dim(m_loc)[2]) {
      k_col <- m_loc[, i]
      k_col <- k_col[grep("^-$", k_col, invert = T)]
      k_col <- k_col[length(k_col)]
      lbl_front_glazed <- c(lbl_front_glazed, k_col)
    }
    names(l_canvas_front) <- lbl_front_glazed

    # assign candidate rear faces
    l_canvas_rear <- l_glz$rear * l_dix$area_face_glazed_coronal * l_dix$ratio_glazed_coronal_to_rear
    if (sum(l_glz$rear * k_wd) > 0) {
      l_canvas_rear <-
        sum(l_canvas_rear) * l_glz$rear * k_wd / sum(l_glz$rear * k_wd)
    }
    i <- 1
    lbl_rear_glazed <- NULL
    for (i in 1:dim(m_loc)[2]) {
      k_col <- m_loc[, i]
      k_col <- k_col[grep("^-$", k_col, invert = T)]
      k_col <- k_col[1]
      lbl_rear_glazed <- c(lbl_rear_glazed, k_col)
    }
    names(l_canvas_rear) <- lbl_rear_glazed

    # assign candidate right faces
    l_canvas_right <- l_glz$right * l_dix$area_face_glazed_sagittal * l_dix$ratio_glazed_sagittal_to_right
    if (sum(l_glz$right * k_dd) > 0) {
      l_canvas_right <-
        sum(l_canvas_right) * l_glz$right * k_dd / sum(l_glz$right * k_dd)
    }
    i <- 1
    lbl_right_glazed <- NULL
    for (i in 1:dim(m_loc)[1]) {
      k_col <- m_loc[i, ]
      k_col <- k_col[grep("^-$", k_col, invert = T)]
      k_col <- k_col[length(k_col)]
      lbl_right_glazed <- c(lbl_right_glazed, k_col)
    }
    names(l_canvas_right) <- lbl_right_glazed

    # assign candidate left faces
    l_canvas_left <- l_glz$left * l_dix$area_face_glazed_sagittal * l_dix$ratio_glazed_sagittal_to_left
    if (sum(l_glz$left * k_dd) > 0) {
      l_canvas_left <-
        sum(l_canvas_left) * l_glz$left * k_dd / sum(l_glz$left * k_dd)
    }
    i <- 1
    lbl_left_glazed <- NULL
    for (i in 1:dim(m_loc)[1]) {
      k_col <- m_loc[i, ]
      k_col <- k_col[grep("^-$", k_col, invert = T)]
      k_col <- k_col[1]
      lbl_left_glazed <- c(lbl_left_glazed, k_col)
    }
    names(l_canvas_left) <- lbl_left_glazed

    # .. generate list of areas and building properties
    l_canvas <- list(
      front = as.list(l_canvas_front),
      rear = as.list(l_canvas_rear),
      right = as.list(l_canvas_right),
      left = as.list(l_canvas_left)
    )
  }

  # remove NaNs, from iteration
  l_canvas <- lapply(l_canvas, function(x) {
    Filter(function(y) {
      !any(sapply(y, is.na))
    }, x)
  })

  return(l_canvas)
}

generate_idf_surfaces <- function(l_b, b_core) {
  #' @title Generate idf blocks for all opaque surfaces
  #'
  #' @param     l_b list of active block/unit
  #' @param  b_core selected unit
  #'
  #' @return list of blocks in idf format
  #' @examples
  #' \dontrun{
  #' generate_idf_surfaces(l_block$`1F`$`(1F-1-1)`, TRUE)
  #' }


  # lambdas
  find_if_void <- function(obj_room, obj_next, lr) {
    # @title Find If Void
    # @description This function checks if there is a void between two rooms based on the conjoined zones data.
    #
    # @param obj_room The identifier for the current room.
    # @param obj_next The identifier for the next room.
    # @param lr A list containing conjoined zones data.
    #
    # @return A boolean value indicating whether there is a void between the two rooms.
    #
    # @examples
    # conjoined_zones <- list(conjoined_zones = data.frame(room_a = "room1", room_b = "room2", void = TRUE))
    # find_if_void("room1", "room2", conjoined_zones)

    # Extract conjoined zones data
    t_match <- lr$conjoined_zones

    # Filter to find matching rooms
    d_match <- t_match %>%
      dplyr::filter((room_a == obj_room & room_b == obj_next) |
        (room_b == obj_room & room_a == obj_next))

    # Check if there is a void between the rooms
    s_next_void <- ifelse(dim(d_match)[1] > 0 & any(d_match$void), TRUE, FALSE)

    return(s_next_void)
  }


  locate_room_pos_in_layout <- function(matx) {
    # @title Locate Room Position in Layout
    # @description This function locates the position of a room in a layout matrix.
    #
    # @param matx A matrix representing the layout of rooms.
    #
    # @return A vector containing the row and column position of the room in the layout matrix.
    #
    # @examples
    # layout_matrix <- matrix(c("FALSE", "TRUE", "FALSE", "FALSE"), nrow = 2)
    # locate_room_pos_in_layout(layout_matrix)

    pos <- 0

    # Iterate through the matrix to find the position of the room
    for (xi in 1:dim(matx)[1]) {
      for (yi in 1:dim(matx)[2]) {
        if (matx[xi, yi] == "TRUE") pos <- c(xi, yi)
      }
    }

    return(pos)
  }

  g_surface <- function(l, k_boundary, l_idfc) {
    # @title Generate Surface IDF
    # @description This function generates an IDF (Input Data File) object for a building surface based on the given layout and boundary conditions.
    #
    # @param l A list containing layout information, including map dimensions, faces, and room locations.
    # @param k_boundary A string representing the boundary condition of the surface.
    # @param l_idfc A list of IDF blocks.
    #
    # @return A list containing the IDF surface object and a summary of its properties.
    #
    # @examples
    # layout_info <- list(map = matrix(1, nrow = 1, ncol = 1), faces = list(.w = 5, .d = 5, .h = 3), room_location = "room1")
    # idf_blocks <- list('BuildingSurface:Detailed' = "Some comment")
    # g_surface(layout_info, "front", idf_blocks)

    # Get the dimensions of the map
    m_dim <- dim(l$map)
    lbl_block <- "BuildingSurface:Detailed"
    lbl_factor <- "AutoCalculate"
    lbl_space <- ""

    # Extract dimensions of the faces
    dw <- l$faces$.w
    dd <- l$faces$.d
    dh <- l$faces$.h

    # Get the room location
    lbl_zone <- l$room_location

    # Define the type and construction label based on the boundary condition
    lbl_type <- lbl_construction <-
      paste(toupper(substr(k_boundary, 1, 1)),
        substr(k_boundary, 2, nchar(k_boundary)),
        sep = ""
      )

    # Assign active and conjoined elements based on the boundary condition
    if (grepl("ground|floor", k_boundary)) {
      s_next <- l$room_floor_links_to
      lbl_srfc <- paste0(l$room_location, ":", "bottom") %>% gsub("\\(|\\)", "", .)
      l_vertices <- list(v1 = c(0, 0, 0), v2 = c(0, dd, 0), v3 = c(dw, dd, 0), v4 = c(dw, 0, 0))
    } else if (grepl("roof|ceiling", k_boundary)) {
      s_next <- l$room_ceiling_links_to
      lbl_srfc <- paste0(l$room_location, ":", "top") %>% gsub("\\(|\\)", "", .)
      l_vertices <- list(v3 = c(0, 0, dh), v4 = c(dw, 0, dh), v1 = c(dw, dd, dh), v2 = c(0, dd, dh))
    } else {
      lbl_srfc <- paste0(l$room_location, ":", k_boundary) %>% gsub("\\(|\\)", "", .)
      lbl_type <- lbl_construction <- "Wall"
      if (grepl("front", k_boundary)) {
        s_next <- l$room_front_links_to
        l_vertices <- list(v4 = c(0, 0, dh), v3 = c(dw, 0, dh), v2 = c(dw, 0, 0), v1 = c(0, 0, 0))
      }
      if (grepl("rear", k_boundary)) {
        s_next <- l$room_rear_links_to
        l_vertices <- list(v3 = c(0, dd, dh), v4 = c(dw, dd, dh), v1 = c(dw, dd, 0), v2 = c(0, dd, 0))
      }
      if (grepl("right", k_boundary)) {
        s_next <- l$room_right_links_to
        l_vertices <- list(v4 = c(dw, 0, dh), v3 = c(dw, dd, dh), v2 = c(dw, dd, 0), v1 = c(dw, 0, 0))
      }
      if (grepl("left", k_boundary)) {
        s_next <- l$room_left_links_to
        l_vertices <- list(v3 = c(0, 0, dh), v4 = c(0, dd, dh), v1 = c(0, dd, 0), v2 = c(0, 0, 0))
      }
    }

    # Assign boundary conditions based on the next room or external conditions
    if (grepl("^\\(", s_next, ignore.case = TRUE)) {
      lbl_boundary <- "Surface"
      lbl_sun_exp <- "NoSun"
      lbl_wind_exp <- "NoWind"
      if (grepl("ground|floor", k_boundary)) {
        lbl_boundary_obj <- paste0(s_next, ":", "top") %>% gsub("\\(|\\)", "", .)
      } else if (grepl("roof|ceiling", k_boundary)) {
        lbl_boundary_obj <- paste0(s_next, ":", "bottom") %>% gsub("\\(|\\)", "", .)
      } else {
        snext <- switch(k_boundary,
          "front" = "rear",
          "rear" = "front",
          "right" = "left",
          "left" = "right"
        )
        lbl_boundary_obj <- paste0(s_next, ":", snext) %>% gsub("\\(|\\)", "", .)
      }
      if (find_if_void(lbl_zone, s_next, l)) {
        lbl_construction <- "Void Air Boundary"
      } else {
        lbl_construction <- paste0("Interior ", lbl_construction)
      }
    } else if (s_next == "ground") {
      lbl_type <- ifelse(lbl_type == "Wall", lbl_type, "Floor")
      lbl_boundary <- "Ground"
      lbl_boundary_obj <- ""
      lbl_sun_exp <- "NoSun"
      lbl_wind_exp <- "NoWind"
      lbl_construction <- paste0("Exterior ", lbl_type)
    } else if (s_next == "exterior") {
      lbl_boundary <- "Outdoors"
      lbl_boundary_obj <- ""
      lbl_sun_exp <- "SunExposed"
      lbl_wind_exp <- "WindExposed"
      lbl_construction <- paste0("Exterior ", lbl_construction)
    } else {
      lbl_boundary <- "Adiabatic"
      lbl_boundary_obj <- ""
      lbl_sun_exp <- "NoSun"
      lbl_wind_exp <- "NoWind"
      lbl_construction <- paste0("Interior ", lbl_construction)
    }

    # Create list of IDF surface properties
    l_zon <- c(
      lbl_block, lbl_srfc, lbl_type, lbl_construction, lbl_zone,
      lbl_space, lbl_boundary, lbl_boundary_obj, lbl_sun_exp,
      lbl_wind_exp, lbl_factor, length(l_vertices),
      paste(sprintf("%.4f", l_vertices$v1), collapse = ","),
      paste(sprintf("%.4f", l_vertices$v2), collapse = ","),
      paste(sprintf("%.4f", l_vertices$v3), collapse = ","),
      paste(sprintf("%.4f", l_vertices$v4), collapse = ",")
    )

    # Create summary list of IDF surface properties to be included in cards
    l_sum <- list(
      name = lbl_srfc,
      zone = lbl_zone,
      space = lbl_space,
      type = lbl_type,
      construction = lbl_construction,
      boundary = lbl_boundary,
      boundary_object = lbl_boundary_obj,
      vertices = l_vertices
    )

    # Reformat list of IDF surface properties
    lbl_block_comment <- lbl_block
    l_zon <- add_idf_comment(l_zon, c("", l_idfc[[lbl_block_comment]]))

    return(list(idf = l_zon, summary = l_sum))
  }

  r_surface <- function(l, k_boundary, l_idfc) {
    # @title Generate Surface IDF
    # @description This function generates an IDF (Input Data File) object for a building surface based on the given layout and boundary conditions.
    #
    # @param l A list containing layout information, including map dimensions, faces, and room locations.
    # @param k_boundary A string representing the boundary condition of the surface.
    # @param l_idfc A list of IDF blocks.
    #
    # @return A list containing the IDF surface object and a summary of its properties.
    #
    # @examples
    # layout_info <- list(map = matrix(1, nrow = 1, ncol = 1), faces = list(.w = 5, .d = 5, .h = 3), room_location = "room1")
    # idf_blocks <- list('BuildingSurface:Detailed' = "Some comment")
    # r_surface(layout_info, "front", idf_blocks)

    lbl_block <- "BuildingSurface:Detailed"
    lbl_factor <- "AutoCalculate"
    lbl_space <- ""
    lbl_zone <- l$room_location
    lbl_type <- lbl_construction <-
      paste(toupper(substr(k_boundary, 1, 1)), substr(k_boundary, 2, nchar(k_boundary)), sep = "")

    # Locate the position of the room in the layout
    c_pos <- locate_room_pos_in_layout(l$map)
    l_act <- l$faces
    l_act$.w <- l_act$.d <- l_act$.hr <- l_act$.hl <- l_act$.alpha <- NULL
    l_act <- lapply(l_act, "[[", c_pos[1])

    # Assign active and conjoined elements based on the boundary condition
    if (grepl("ground|floor", k_boundary)) {
      s_next <- l$room_floor_links_to
      lbl_srfc <- paste0(l$room_location, ":", "bottom") %>% gsub("\\(|\\)", "", .)
      l_vertices <- l_act$bottom
      names(l_vertices) <- paste0("v", 1:length(l_vertices))
    } else if (grepl("roof|ceiling", k_boundary)) {
      if (length(l_act$top) == 1) {
        s_next <- l$room_ceiling_links_to
        lbl_srfc <- paste0(l$room_location, ":", "top") %>% gsub("\\(|\\)", "", .)
        l_vertices <- l_act$top$c
        names(l_vertices) <- paste0("v", 1:length(l_vertices))
      } else {
        s_next <- l$room_ceiling_links_to
        lbl_srfc_a <- paste0(l$room_location, ":", "top-a") %>% gsub("\\(|\\)", "", .)
        lbl_srfc_b <- paste0(l$room_location, ":", "top-b") %>% gsub("\\(|\\)", "", .)
        l_vertices_a <- l_act$top$a
        l_vertices_b <- l_act$top$b
        names(l_vertices_a) <- paste0("v", 1:length(l_vertices_a))
        names(l_vertices_b) <- paste0("v", 1:length(l_vertices_b))
      }
    } else {
      lbl_srfc <- paste0(l$room_location, ":", k_boundary) %>% gsub("\\(|\\)", "", .)
      lbl_type <- lbl_construction <- "Wall"
      if (grepl("front", k_boundary)) {
        s_next <- l$room_front_links_to
        l_vertices <- l_act$front
        names(l_vertices) <- paste0("v", 1:length(l_vertices))
      }
      if (grepl("rear", k_boundary)) {
        s_next <- l$room_rear_links_to
        l_vertices <- rev(l_act$rear)
        names(l_vertices) <- paste0("v", 1:length(l_vertices))
      }
      if (grepl("right", k_boundary)) {
        s_next <- l$room_right_links_to
        l_vertices <- rev(l_act$right)
        names(l_vertices) <- paste0("v", 1:length(l_vertices))
      }
      if (grepl("left", k_boundary)) {
        s_next <- l$room_left_links_to
        l_vertices <- l_act$left
        names(l_vertices) <- paste0("v", 1:length(l_vertices))
      }
    }

    # Assign boundary conditions based on the next room or external conditions
    if (grepl("^\\(", s_next, ignore.case = TRUE)) {
      lbl_boundary <- "Surface"
      lbl_sun_exp <- "NoSun"
      lbl_wind_exp <- "NoWind"
      if (grepl("ground|floor", k_boundary)) {
        lbl_boundary_obj <- paste0(s_next, ":", "top") %>% gsub("\\(|\\)", "", .)
      } else if (grepl("roof|ceiling", k_boundary)) {
        lbl_boundary_obj <- paste0(s_next, ":", "bottom") %>% gsub("\\(|\\)", "", .)
      } else {
        snext <- switch(k_boundary,
          "front" = "rear",
          "rear" = "front",
          "right" = "left",
          "left" = "right"
        )
        lbl_boundary_obj <- paste0(s_next, ":", snext) %>% gsub("\\(|\\)", "", .)
      }
      if (find_if_void(lbl_zone, s_next, l)) {
        lbl_construction <- "Void Air Boundary"
      } else {
        lbl_construction <- paste0("Interior ", lbl_construction)
      }
    } else if (s_next == "ground") {
      lbl_type <- "Floor"
      lbl_boundary <- "Ground"
      lbl_boundary_obj <- ""
      lbl_sun_exp <- "NoSun"
      lbl_wind_exp <- "NoWind"
      lbl_construction <- paste0("Exterior ", lbl_type)
    } else if (s_next == "exterior") {
      lbl_boundary <- "Outdoors"
      lbl_boundary_obj <- ""
      lbl_sun_exp <- "SunExposed"
      lbl_wind_exp <- "WindExposed"
      lbl_construction <- paste0("Exterior ", lbl_construction)
    } else {
      lbl_boundary <- "Adiabatic"
      lbl_boundary_obj <- ""
      lbl_sun_exp <- "NoSun"
      lbl_wind_exp <- "NoWind"
      lbl_construction <- paste0("Interior ", lbl_construction)
    }

    # Create list of surface properties
    if (grepl("roof|ceiling", k_boundary) & length(l_act$top) > 1) {
      l_zon_a <- c(
        lbl_block, lbl_srfc_a, lbl_type, lbl_construction, lbl_zone,
        lbl_space, lbl_boundary, lbl_boundary_obj, lbl_sun_exp,
        lbl_wind_exp, lbl_factor, length(l_vertices_a),
        as.character(lapply(l_vertices_a, function(x) {
          paste(
            sprintf("%.4f", x),
            collapse = ","
          )
        }))
      )
      l_zon_b <- c(
        lbl_block, lbl_srfc_b, lbl_type, lbl_construction, lbl_zone,
        lbl_space, lbl_boundary, lbl_boundary_obj, lbl_sun_exp,
        lbl_wind_exp, lbl_factor, length(l_vertices_b),
        as.character(lapply(l_vertices_b, function(x) {
          paste(
            sprintf("%.4f", x),
            collapse = ","
          )
        }))
      )
      l_sum_a <- list(
        name = lbl_srfc_a,
        zone = lbl_zone,
        space = lbl_space,
        type = lbl_type,
        construction = lbl_construction,
        boundary = lbl_boundary,
        boundary_object = lbl_boundary_obj,
        vertices = l_vertices_a
      )
      l_sum_b <- list(
        name = lbl_srfc_b,
        zone = lbl_zone,
        space = lbl_space,
        type = lbl_type,
        construction = lbl_construction,
        boundary = lbl_boundary,
        boundary_object = lbl_boundary_obj,
        vertices = l_vertices_b
      )
      l_sum <- list(l_sum_a, l_sum_b)
      lbl_block_comment <- gsub("[[:punct:]]", "_", lbl_block)
      l_zon_a <- add_idf_comment(l_zon_a, c("", l_idfc[[lbl_block_comment]]))
      l_zon_b <- add_idf_comment(l_zon_b, c("", l_idfc[[lbl_block_comment]]))
      l_zon <- c(l_zon_a, l_zon_b)
    } else {
      l_zon <- c(
        lbl_block, lbl_srfc, lbl_type, lbl_construction, lbl_zone,
        lbl_space, lbl_boundary, lbl_boundary_obj, lbl_sun_exp,
        lbl_wind_exp, lbl_factor, length(l_vertices),
        as.character(lapply(l_vertices, function(x) {
          paste(
            sprintf("%.4f", x),
            collapse = ","
          )
        }))
      )
      l_sum <- list(
        name = lbl_srfc,
        zone = lbl_zone,
        space = lbl_space,
        type = lbl_type,
        construction = lbl_construction,
        boundary = lbl_boundary,
        boundary_object = lbl_boundary_obj,
        vertices = l_vertices
      )
      lbl_block_comment <- lbl_block
      l_cmn <- l_idfc[[lbl_block_comment]]
      if (length(l_vertices) > 4) {
        lbl_extra <- gsub("\\s\\d\\s", " ", l_cmn[length(l_cmn)])
        l_cmn <- c(l_cmn, rep(lbl_extra, length(l_vertices) - 4))
      }
      l_zon <- add_idf_comment(l_zon, c("", l_cmn))
    }

    return(list(idf = l_zon, summary = l_sum))
  }

  make_zone_idf <- function(lb, core = TRUE) {
    # @title Generate idf block based based on room/zone parameters
    # @param   lb list of parameters of room/zone
    # @param core indication of building unit: core or attachments
    # @return lines of text containing an idf-block

    # Define if part of total floor area
    in_tfa <- ifelse(
      grepl("^\\(TF|^\\(PR", lb$room_location) | core == FALSE, "No", "Yes"
    )

    # Generate idf-header
    hdr_zon <- make_idf_block_title("Zone")

    # Generate list of parameters for idf-block
    idfo <- c(
      "Zone", lb$room_location, "0.0",
      sprintf("%.9f", lb$origins[1]),
      sprintf("%.9f", lb$origins[2]),
      sprintf("%.9f", lb$origins[3]),
      "", "1", rep("autocalculate", 3),
      rep("", 2), in_tfa
    )

    # Generate idf-comment
    idfo <- add_idf_comment(idfo, c("", l_idf_blocks$Zone))

    # Generate whole idf-block
    idfo <- c(hdr_zon, "", idfo)

    return(idfo)
  }


  # generate idf:zone
  l_zon <- make_zone_idf(l_b, b_core)

  # generate idf:surface, starting with lower stories, then top storeys

  if (!grepl("AT|PR", l_b$room_location)) {
    l_bottom <- ifelse(grepl("gro", l_b$room_floor_links_to), "ground", "floor")
    l_bottom <- g_surface(l_b, l_bottom, l_idf_blocks)

    l_front <- g_surface(l_b, "front", l_idf_blocks)
    l_rear <- g_surface(l_b, "rear", l_idf_blocks)
    l_left <- g_surface(l_b, "left", l_idf_blocks)
    l_right <- g_surface(l_b, "right", l_idf_blocks)

    l_top <- ifelse(grepl("TF", l_b$room_location), "roof", "ceiling")
    l_top <- g_surface(l_b, l_top, l_idf_blocks)
  } else {
    l_bottom <- r_surface(l_b, "floor", l_idf_blocks)
    l_front <- r_surface(l_b, "front", l_idf_blocks)
    l_rear <- r_surface(l_b, "rear", l_idf_blocks)
    l_left <- r_surface(l_b, "left", l_idf_blocks)
    l_right <- r_surface(l_b, "right", l_idf_blocks)
    l_top <- r_surface(l_b, "roof", l_idf_blocks)
  }


  # .. combine idf objects/sections
  l_block <- list(
    zone = l_zon,
    f_bottom = l_bottom$idf, f_top = l_top$idf, f_front = l_front$idf,
    f_rear = l_rear$idf, f_left = l_left$idf, f_right = l_right$idf
  )

  # .. combine idf objects/sections
  l_info <- list(
    f_bottom = l_bottom$summary, f_top = l_top$summary,
    f_front = l_front$summary, f_rear = l_rear$summary,
    f_left = l_left$summary, f_right = l_right$summary
  )

  return(list(block = l_block, info = l_info))
}

generate_list_of_surfaces <- function(b_it, l_it, m_dm, l_dm, l_in, l_cr) {
  #' @title Generate IDF surfaces
  #'
  #' @param b_it name of storey to be processed
  #' @param l_it list of active house layouts
  #' @param m_dm matrix of dimensions for each room
  #' @param l_dm list of dimensions for each room
  #' @param l_in list of custom input parameters
  #' @param l_cr logical indication of core unit
  #'
  #' @examples
  #' \dontrun{
  #' generate_list_of_surfaces(b_sto[1], l_mir$left, m_dim, l_drm, b_cub, TRUE)
  #' }


  # .. lambdas

  extract_block_info <- function(id, l_rooms, dim_mtx, l_dims, l_infs) {
    # @title Extract block information
    # @description This function extracts the information for a specific block from the input data.
    # @param id The identifier for the block.
    # @param l_rooms A list containing the layout information for the block.
    # @param dim_mtx A matrix containing the dimensions of the block.
    # @param l_dims A list containing the dimensions of the block.
    # @param l_infs A list containing the custom input parameters.
    # @return A list containing the extracted information for the block.

    # lambdas
    get_parameters_roominroof <- function(ld, lr) {
      # @title Get Parameters for Room in Roof
      # @description This function calculates various parameters for a room located in the roof, including angles, segment heights, and ridge location.
      #
      # @param ld A list containing the dimensions of the layout (`w`, `d`, `w_block`, `d_block`).
      # @param lr A list containing the room-in-roof parameters (`h`, `apex`).
      # @return A list with calculated parameters including angles, ridge location, heights, and segment dimensions.

      # Extract layout dimensions
      wd <- ld$w
      dd <- ld$d
      hs <- lr$h
      ht <- hf <- lr$apex

      # Calculate angles
      alpha <- atan((ht - hs) / (dd / 2)) * 180 / pi
      beta <- 180 - 90 - alpha

      # Calculate segment steps
      w_step <- c(0, cumsum(ld$w_block))
      d_step <- c(0, cumsum(ld$d_block))

      # Calculate segment heights
      hx <- list()
      i <- 1
      for (k in d_step) {
        j <- ifelse(k > dd / 2, abs(k - dd), k)
        hx[i] <- tan(alpha * pi / 180) * j + hs
        i <- i + 1
      }
      hx <- unlist(hx)

      # Determine ridge location
      m_ridge <- matrix(FALSE, nrow = length(ld$d_block), ncol = length(ld$w_block))
      p_ridge <- ceiling(dim(m_ridge)[1] / 2)
      if ((dim(m_ridge)[1] %% 2) == 0) p_ridge <- c(p_ridge, p_ridge + 1)
      m_ridge[p_ridge, ] <- TRUE

      # Create result list
      l <- list(
        alpha = alpha,
        beta = beta,
        ridge_location = m_ridge,
        ridge_height = ht,
        lower_height = hs,
        seg_heights = hx,
        seg_widths = w_step,
        seg_depths = d_step
      )

      return(l)
    }

    get_surface_roominroof <- function(l, lf, wr) {
      # @title Get Surface Room in Roof
      # @description This function calculates the coordinates for the surfaces of a room in a roof.
      # @param l List containing segment properties (widths, depths, heights, ridge height, ridge location).
      # @param lf List containing face properties (width, depth, height).
      # @param wr Width of the room.
      # @return A list of coordinates for the surfaces of the room in the roof.

      c <- c(0, 0, 0)
      l_seg_d <- 1:dim(l$ridge_location)[1] %>% rev()
      b_ridge <- l$ridge_location[, 1]

      # face_bottom
      get_rir_bottom_face <- function(k, l_p, w_r) {
        # @title Get Bottom Face Coordinates
        # @description This function calculates the coordinates for the bottom face of a segment.
        #
        # @param k Index of the segment.
        # @param l_p List containing segment properties (widths, depths, heights).
        # @param w_r Width of the segment.
        # @return A list of coordinates for the bottom face.

        # Extract segment properties
        w <- l_p$seg_widths
        d <- l_p$seg_depths
        h <- l_p$seg_heights

        # Calculate coordinates for the bottom face
        p1 <- c(c[1] + 0, c[2] + (d[k + 1] - d[k]), c[3])
        p2 <- c(c[1] + w_r, c[2] + (d[k + 1] - d[k]), c[3])
        p3 <- c(c[1] + w_r, c[2] + 0, c[3])
        p4 <- c(c[1] + 0, c[2] + 0, c[3])

        return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
      }

      get_rir_front_face <- function(k, l_p, w_r) {
        # @title Get Front Face Coordinates
        # @description This function calculates the coordinates for the front face of a segment.
        #
        # @param k Index of the segment.
        # @param l_p List containing segment properties (widths, depths, heights).
        # @param w_r Width of the segment.
        # @return A list of coordinates for the front face.

        # Extract segment properties
        w <- l_p$seg_widths
        d <- l_p$seg_depths
        h <- l_p$seg_heights

        # Calculate coordinates for the front face
        p1 <- c(c[1] + 0, c[2] + 0, c[3])
        p2 <- c(c[1] + w_r, c[2] + 0, c[3])
        p3 <- c(c[1] + w_r, c[2] + 0, c[3] + h[k])
        p4 <- c(c[1] + 0, c[2] + 0, c[3] + h[k])

        return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
      }

      get_rir_rear_face <- function(k, l_p, w_r) {
        # @title Get Rear Face Coordinates
        # @description This function calculates the coordinates for the rear face of a segment.
        #
        # @param k Index of the segment.
        # @param l_p List containing segment properties (widths, depths, heights).
        # @param w_r Width of the segment.
        # @return A list of coordinates for the rear face.

        # Extract segment properties
        w <- l_p$seg_widths
        d <- l_p$seg_depths
        h <- l_p$seg_heights

        # Calculate coordinates for the rear face
        p1 <- c(c[1] + 0, c[2] + (d[k + 1] - d[k]), c[3])
        p2 <- c(c[1] + w_r, c[2] + (d[k + 1] - d[k]), c[3])
        p3 <- c(c[1] + w_r, c[2] + (d[k + 1] - d[k]), c[3] + h[k + 1])
        p4 <- c(c[1] + 0, c[2] + (d[k + 1] - d[k]), c[3] + h[k + 1])

        return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
      }

      get_rir_left_face <- function(k, l_p, w_r) {
        # @title Get Left Face Coordinates
        # @description This function calculates the coordinates for the left face of a segment.
        #
        # @param k Index of the segment.
        # @param l_p List containing segment properties (widths, depths, heights, ridge height, ridge location).
        # @param w_r Width of the segment.
        # @return A list of coordinates for the left face.

        # Extract segment properties
        w <- l_p$seg_widths
        d <- l_p$seg_depths
        h <- l_p$seg_heights
        ht <- l_p$ridge_height
        rl <- l_p$ridge_location[, 1]
        dt <- ifelse(rl[k], (d[k + 1] - d[k]) / 2, 0)

        # Calculate coordinates for the left face
        p1 <- c(c[1] + 0, c[2] + 0, c[3] + 0)
        p2 <- c(c[1] + 0, c[2] + 0, c[3] + h[k])
        p3 <- c(c[1] + 0, c[2] + (d[k + 1] - d[k]), c[3] + h[k + 1])
        p4 <- c(c[1] + 0, c[2] + (d[k + 1] - d[k]), c[3] + 0)
        pt <- c(c[1] + 0, c[2] + dt, c[3] + ht)

        # Determine if ridge location affects the face
        if (rl[k] & sum(rl) == 1) {
          l_face <- list(p1 = p1, p2 = p2, p3 = pt, p4 = p3, p5 = p4)
        } else {
          l_face <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
        }

        return(l_face)
      }

      get_rir_right_face <- function(k, l_p, w_r) {
        # @title Get Right Face Coordinates
        # @description This function calculates the coordinates for the right face of a segment.
        #
        # @param k Index of the segment.
        # @param l_p List containing segment properties (widths, depths, heights, ridge height, ridge location).
        # @param w_r Width of the segment.
        # @return A list of coordinates for the right face.

        # Extract segment properties
        w <- l_p$seg_widths
        d <- l_p$seg_depths
        h <- l_p$seg_heights
        ht <- l_p$ridge_height
        rl <- l_p$ridge_location[, 1]
        dt <- ifelse(rl[k], (d[k + 1] - d[k]) / 2, 0)

        # Calculate coordinates for the right face
        p1 <- c(c[1] + w_r, c[2] + 0, c[3] + 0)
        p2 <- c(c[1] + w_r, c[2] + 0, c[3] + h[k])
        p3 <- c(c[1] + w_r, c[2] + (d[k + 1] - d[k]), c[3] + h[k + 1])
        p4 <- c(c[1] + w_r, c[2] + (d[k + 1] - d[k]), c[3] + 0)
        pt <- c(c[1] + w_r, c[2] + dt, c[3] + ht)

        # Determine if ridge location affects the face
        if (rl[k] & sum(rl) == 1) {
          l_face <- list(p1 = p1, p2 = p2, p3 = pt, p4 = p3, p5 = p4)
        } else {
          l_face <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
        }

        return(l_face)
      }

      get_rir_top_face <- function(k, l_p, w_r) {
        # @title Get Top Face Coordinates
        # @description This function calculates the coordinates for the top face of a segment.
        #
        # @param k Index of the segment.
        # @param l_p List containing segment properties (widths, depths, heights, ridge height, ridge location).
        # @param w_r Width of the segment.
        # @return A list of coordinates for the top face.

        # Extract segment properties
        w <- l_p$seg_widths
        d <- l_p$seg_depths
        h <- l_p$seg_heights
        ht <- l_p$ridge_height
        rl <- l_p$ridge_location[, 1]

        # Determine split factor based on ridge location
        k_split <- ifelse(sum(rl, na.rm = TRUE) > 1, 1, 2)
        dt <- ifelse(rl[k], (d[k + 1] - d[k]) / k_split, 0)

        # Calculate coordinates for the top face
        p1 <- c(c[1] + 0, c[2] + 0, c[3] + h[k])
        p2 <- c(c[1] + w_r, c[2] + 0, c[3] + h[k])
        p3 <- c(c[1] + w_r, c[2] + (d[k + 1] - d[k]), c[3] + h[k + 1])
        p4 <- c(c[1] + 0, c[2] + (d[k + 1] - d[k]), c[3] + h[k + 1])
        pt <- c(c[1] + w_r, c[2] + dt, c[3] + ht)
        pr <- c(c[1] + 0, c[2] + dt, c[3] + ht)

        # Determine if ridge location affects the face
        if (rl[k] & k_split > 1) {
          l_face_a <- list(p1 = p1, p2 = p2, p3 = pt, p4 = pr)
          l_face_b <- list(p1 = pr, p2 = pt, p3 = p3, p4 = p4)
          if (!dt %in% d[k:(k + 1)]) {
            l_face <- list(a = l_face_a, b = l_face_b)
          } else {
            if (dt == d[k]) {
              l_face <- list(c = l_face_a)
            } else {
              l_face <- list(c = l_face_b)
            }
          }
        } else {
          l_face <- list(c = list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
        }

        return(l_face)
      }

      # Iterate over bottom face segments
      l_bottom <- lapply(l_seg_d, get_rir_bottom_face, l, wr)
      names(l_bottom) <- paste0("face_", l_seg_d)

      # Iterate over front face segments
      l_front <- lapply(l_seg_d, get_rir_front_face, l, wr)
      names(l_front) <- paste0("face_", l_seg_d)

      # Iterate over rear face segments
      l_rear <- lapply(l_seg_d, get_rir_rear_face, l, wr)
      names(l_rear) <- paste0("face_", l_seg_d)

      # Iterate over left face segments
      l_left <- lapply(l_seg_d, get_rir_left_face, l, wr)
      names(l_left) <- paste0("face_", l_seg_d)

      # Iterate over right face segments
      l_right <- lapply(l_seg_d, get_rir_right_face, l, wr)
      names(l_right) <- paste0("face_", l_seg_d)

      # Iterate over top face segments
      l_top <- lapply(l_seg_d, get_rir_top_face, l, wr)
      names(l_top) <- paste0("face_", l_seg_d)

      # group lists
      l <- list(
        .w = lf$.w, .d = lf$.d, .alpha = l$alpha,
        .hr = max(l$ridge_height), .hl = max(l$lower_height),
        bottom = l_bottom, front = l_front, rear = l_rear,
        left = l_left, right = l_right, top = l_top
      )

      return(l)
    }

    assign_top_block_heights <- function(lly, lax) {
      # @title Assign Top Block Heights
      # @description This function assigns heights for the top floor based on whether the roof is flat or gable.
      #
      # @param lly A list containing the property `flat_roof` which indicates if the roof is flat.
      # @param lax A list containing the property `h` which is used to calculate the apex height.
      # @return A list with the height and apex of the top block.

      # Check if the roof is flat or gable
      if (lly$flat_roof == FALSE) {
        # Assign heights for gable roof
        rir_height <- 1.125
        rir_apex <- round(rir_height + lax$h * 1.1 - 1.4, 2)
      } else {
        # Assign heights for flat roof
        rir_height <- 0.250
        rir_apex <- rir_height
      }

      return(list(h = rir_height, apex = rir_apex))
    }


    locate_room_in_layout <- function(lbl, matx) {
      # @title Locate Room in Layout
      # @description This function locates a room in a layout matrix and marks its position.
      #
      # @param lbl The label of the room to locate.
      # @param matx A matrix representing the layout.
      # @return A matrix with the room's position marked as TRUE.

      # Iterate over each element in the matrix
      for (xi in 1:dim(matx)[1]) {
        for (yi in 1:dim(matx)[2]) {
          # Mark the position of the room
          matx[xi, yi] <- ifelse(matx[xi, yi] == lbl, TRUE, FALSE)
        }
      }

      return(matx)
    }

    # object parameters
    l_cuboid <- list()
    l_room <- l_rooms[[id]]
    l_r <- 1:dim_mtx$rows
    l_c <- 1:dim_mtx$cols

    # .. estimate house apex and generalise roof generation
    l_rir <- assign_top_block_heights(l_infs, l_dims)

    for (xi in l_r) {
      for (yi in l_c) {
        l_block <- l_room
        l_block$room_location <- l_block$room_location[xi, yi]
        l_block$room_assignment <- l_block$room_assignment[xi, yi]
        l_block$room_floor_links_to <- l_block$room_floor_links_to[xi, yi]
        l_block$room_ceiling_links_to <- l_block$room_ceiling_links_to[xi, yi]
        l_block$room_front_links_to <- l_block$room_front_links_to[xi, yi]
        l_block$room_rear_links_to <- l_block$room_rear_links_to[xi, yi]
        l_block$room_right_links_to <- l_block$room_right_links_to[xi, yi]
        l_block$room_left_links_to <- l_block$room_left_links_to[xi, yi]
        l_block$map <- locate_room_in_layout(
          l_block$room_location, l_room$room_location
        )
        l_block$origins <- l_block$origins[[xi, yi]]
        l_block$faces$.w <- l_dims$w_block[yi]
        l_block$faces$.d <- l_dims$d_block[xi]

        if (id %in% c("TF", "PR", "AT")) {
          # .. insert top roof section
          l_faces_rir <- get_parameters_roominroof(l_dims, l_rir)
          l_faces_rir <- get_surface_roominroof(l_faces_rir, l_block$faces, l_block$faces$.w)
          l_block$faces <- l_faces_rir

          if (id == "TF") {
            l_block$faces$.h <- l_block$faces$.hr
          }
        }

        l_cuboid[[l_block$room_location]] <- l_block
      }
    }

    l_cuboid <- l_cuboid[names(l_cuboid) %in% "-" == FALSE]

    return(l_cuboid)
  }


  # .. object parameters

  # Extract block information
  l_block <-
    lapply(b_it, extract_block_info, l_it, m_dm, l_dm, l_in)
  names(l_block) <- b_it

  # Generate list of surfaces
  idf_block <-
    lapply(b_it, function(x) lapply(l_block[[x]], generate_idf_surfaces, l_cr))
  names(idf_block) <- b_it

  # Extract block information
  lapply(idf_block[[b_it]], "[[", "block")

  # Return list of surfaces
  return(list(
    block = l_block[[b_it]],
    idf = lapply(idf_block[[b_it]], "[[", "block"),
    info = lapply(idf_block[[b_it]], "[[", "info")
  ))
}

mirror_layout <- function(l_it, b_att, b_sto, m_dim, l_drn, l_inf) {
  #' @title Generate Mirrored or Equal Cuboids for Attachments
  #'
  #' @description The function processes the input layout to generate mirrored layouts for the left and right sides. It then adjusts the origins of the new layouts and combines them based on the specified attachment condition.
  #'
  #' @param l_it A list of room layouts.
  #' @param b_att A string indicating the attachment condition ('left', 'right', 'both', or other).
  #' @param b_sto A vector of storeys/floors.
  #' @param m_dim A matrix of dimensions.
  #' @param l_drn A list of dimensions for each room.
  #' @param l_inf A list of custom input parameters.
  #'
  #' @return A list containing the mirrored layout for the specified attachment condition.
  #'
  #' @examples
  #' \dontrun{
  #'   result <- mirror_layout(l_it, b_att, b_sto, m_dim, l_drn, l_inf)
  #'   print(result)
  #' }

  #' Helper function to mirror a single layout
  mirror_rooms <- function(l_lay, l_rn) {
    # @title Mirror Rooms
    # @description This function mirrors the rooms in a layout.
    # @param l_lay A list containing the layout details.
    # @param l_rn A list containing the room names.
    # @return A list with the mirrored layout details.

    mirror_room_matrix <- function(m_l, prefix) {
      # @title Mirror Room Matrix
      # @description This function mirrors a room matrix horizontally and adjusts the room labels.
      #
      # @param m_l A matrix representing the room layout.
      # @param prefix A prefix to be added to the room labels.
      # @return A mirrored matrix with adjusted room labels.

      # Initialize an empty vector for the reversed matrix
      m_rev <- NULL

      # Reverse the columns of the matrix
      for (i in dim(m_l)[2]:1) m_rev <- c(m_rev, m_l[, i])

      # Adjust the room labels
      m_rev <- gsub("^\\([l|r]", "(x", m_rev, perl = TRUE)
      m_rev <- gsub("^\\((?![l|r|x])", paste0("(", prefix), m_rev, perl = TRUE)
      m_rev <- gsub("^\\(x", "(", m_rev, perl = TRUE)

      # Convert the vector back to a matrix
      m_rev <- matrix(m_rev, nrow = dim(m_l)[1], ncol = dim(m_l)[2])

      return(m_rev)
    }

    mirror_storey <- function(l_l, b_d) {
      # @title Mirror Storey
      # @description This function mirrors the layout of a storey, including room locations and links.
      #
      # @param l_l A list containing the layout details of the storey.
      # @param b_d A prefix indicating the side to mirror ('l' for left, 'r' for right).
      # @return A list with the mirrored layout details.

      # Store the original room locations
      l_ref <- l_l$room_location

      # Mirror various room matrices
      l_l$room_location <- mirror_room_matrix(l_l$room_location, b_d)
      l_l$room_assignment <- mirror_room_matrix(l_l$room_assignment, b_d)
      l_l$room_floor_links_to <- mirror_room_matrix(l_l$room_floor_links_to, b_d)
      l_l$room_ceiling_links_to <- mirror_room_matrix(l_l$room_ceiling_links_to, b_d)
      l_l$room_front_links_to <- mirror_room_matrix(l_l$room_front_links_to, b_d)
      l_l$room_rear_links_to <- mirror_room_matrix(l_l$room_rear_links_to, b_d)

      # Mirror and swap left and right links
      l_l$room_right_links_pre <- mirror_room_matrix(l_l$room_right_links_to, b_d)
      l_l$room_left_links_pre <- mirror_room_matrix(l_l$room_left_links_to, b_d)
      l_l$room_right_links_to <- l_l$room_left_links_pre
      l_l$room_left_links_to <- l_l$room_right_links_pre
      l_l$room_left_links_pre <- l_l$room_right_links_pre <- NULL

      # Replace left side with exterior if mirroring left
      if (b_d == "l") {
        col_n <- dim(l_l$room_location)[2]
        l_l$room_left_links_to[, 1] <- gsub("^\\(.*", "exterior", l_l$room_left_links_to[, 1])
        l_l$room_right_links_to[, col_n] <- l_ref[, 1]
      }

      # Replace right side with exterior if mirroring right
      if (b_d == "r") {
        col_n <- dim(l_l$room_location)[2]
        l_l$room_right_links_to[, col_n] <- gsub("^\\(.*", "exterior", l_l$room_right_links_to[, col_n])
        l_l$room_left_links_to[, 1] <- l_ref[, col_n]
      }

      # Adjust conjoined zones
      l_l$conjoined_zones$room_a <- gsub("^\\(", paste0("(", b_d), l_l$conjoined_zones$room_a)
      l_l$conjoined_zones$room_b <- gsub("^\\(", paste0("(", b_d), l_l$conjoined_zones$room_b)

      return(l_l)
    }

    move_origin <- function(ily, lrn, lly, side) {
      # @title Move Origin
      # @description This function moves the origin of a layout based on the side specified.
      #
      # @param ily The index of the layout.
      # @param lrn A list containing the properties `w_block` and `d_block` for width and depth blocks.
      # @param lly A list containing the layout details.
      # @param side The side to move the origin to ('left', 'right', or other).
      # @return A data frame with the new origins.

      # Get the original origins
      m_ori <- lly[[ily]]$origins

      # Calculate new origins based on the side
      if (side == "left") {
        ily_lbl <- paste0("L", ily)
        k_x <- rev(cumsum(lrn$w_block) * (-1))
        k_y <- rev(c(0, cumsum(lrn$d_block))[0:length(lrn$d_block)])
        k_z <- m_ori[[1, 1]][3]
      } else if (side == "right") {
        ily_lbl <- paste0("R", ily)
        k_x <- c(lrn$w, cumsum(rev(lrn$w_block)) + lrn$w)[0:length(lrn$w_block)]
        k_y <- rev(c(0, cumsum(lrn$d_block))[0:length(lrn$d_block)])
        k_z <- m_ori[[1, 1]][3]
      } else {
        ily_lbl <- paste0("X", ily)
        k_x <- m_ori[[1, 1]][1]
        k_y <- m_ori[[1, 1]][2]
        k_z <- m_ori[[1, 1]][3]
      }

      ily_lbl <- ily

      # Define new xyz origins
      l_ori <- l_orix <- list()
      for (yi in 1:length(k_y)) {
        for (xi in 1:length(k_x)) {
          l_orix[[xi]] <- c(k_x[xi], k_y[yi], k_z)
        }
        l_ori[[yi]] <- l_orix
      }
      d_ori <- as.data.frame(do.call(rbind, l_ori))

      # Reformat xyz origins
      colnames(d_ori) <- paste0("(", ily_lbl, "-n-", 1:length(k_x), ")")
      rownames(d_ori) <- paste0("(", ily_lbl, "-", 1:length(k_y), "-m)")

      return(d_ori)
    }

    # .. process inputs
    b_w <- l_lay$GF$faces$.w

    # .. mirror layout to given side
    l_lay_l <- lapply(l_lay, mirror_storey, "l")
    l_lay_r <- lapply(l_lay, mirror_storey, "r")

    # .. move origin of new layout to left side
    l_origins_left <- lapply(names(l_lay_l), move_origin, l_rn, l_lay, "left")
    names(l_origins_left) <- names(l_lay_l)
    for (k in names(l_lay_l)) l_lay_l[[k]]$origins <- l_origins_left[[k]]

    # .. move origin of new layout to right side
    l_origins_right <- lapply(names(l_lay_l), move_origin, l_rn, l_lay, "right")
    names(l_origins_right) <- names(l_lay_l)
    for (k in names(l_lay_r)) l_lay_r[[k]]$origins <- l_origins_right[[k]]

    # .. combine mirrored layouts
    l_shifted <- list(left = l_lay_l, right = l_lay_r)

    return(l_shifted)
  }

  # .. process inputs
  l_mir <- mirror_rooms(l_it, l_drn)
  l_drn$w_block <- rev(l_drn$w_block)
  l_drn$d_block <- rev(l_drn$d_block)

  # .. generate mirrored layouts
  if (grepl("both", b_att)) {
    l_mir_l <- lapply(b_sto, generate_list_of_surfaces, l_mir$left, m_dim, l_drn, l_inf, FALSE)
    l_mir_r <- lapply(b_sto, generate_list_of_surfaces, l_mir$right, m_dim, l_drn, l_inf, FALSE)
    names(l_mir_l) <- names(l_mir_r) <- b_sto
    l_mir_lay <- list(left = l_mir_l, right = l_mir_r)
  } else if (grepl("left", b_att)) {
    l_mir_l <- lapply(b_sto, generate_list_of_surfaces, l_mir$left, m_dim, l_drn, l_inf, FALSE)
    names(l_mir_l) <- b_sto
    l_mir_lay <- list(left = l_mir_l, right = NULL)
  } else if (grepl("right", b_att)) {
    l_mir_r <- lapply(b_sto, generate_list_of_surfaces, l_mir$right, m_dim, l_drn, l_inf, FALSE)
    names(l_mir_r) <- b_sto
    l_mir_lay <- list(left = NULL, right = l_mir_r)
  } else {
    l_mir_lay <- list(left = NULL, right = NULL)
  }

  return(l_mir_lay)
}

generate_surfaces_walls <- function(b_cub, l_rly, l_drm, l_tpl = t_layouts) {
  #' @title Generate IDF objects for each of the surfaces
  #'
  #' @param b_cub information card of house properties
  #' @param l_rly list of active rooms
  #' @param l_drm list of dimensions
  #' @param l_tpl list of layout templates
  #'
  #' @return list of opaque blocks by unit/storey/dict,idf
  #' @examples
  #' \dontrun{
  #'  generate_surfaces_walls(l_cuboid, l_room_layouts, l_dim)
  #' }

  # Load parameters
  b_sto <- b_cub$storeys
  b_att <- b_cub$attachment
  m_dim <- obtain_matrix_size(b_cub$layouts, l_tpl)

  # generate (idf) surfaces for each block within the main unit
  l_srf <- lapply(b_sto, generate_list_of_surfaces, l_rly, m_dim, l_drm, b_cub, TRUE)
  names(l_srf) <- b_sto

  # generate mirrored (idf) surfaces, for attached unit(s)
  l_srm <- mirror_layout(l_rly, b_att, b_sto, m_dim, l_drm, b_cub)

  # combine generated objects
  l_opq <- list(main = l_srf, left = l_srm$left, right = l_srm$right)

  # remove empty blocks
  l_opq <- l_opq[lengths(l_opq) > 0L]

  return(l_opq)
}

generate_surfaces_glazing <- function(b_cub, b_glz, l_gl, l_lg, l_bl) {
  #' @title Generate IDF fenestration objects for each of the surfaces
  #'
  #' @param  b_cub information card of house properties
  #' @param  b_glz list of active rooms
  #' @param   l_gl list of glazing dimensions
  #' @param   l_lg list of home info
  #' @param   l_bl list of layout templates
  #'
  #' @return list of fenestration blocks by unit/storey/dict,idf
  #'
  #' @examples
  #' \dontrun{
  #' generate_surfaces_glazing(l_cuboid, l_glazing, l_glazed, l_glazde, l_blocks)
  #' }

  # Helper function to generate glazing surfaces
  get_parameters_glazing <- function(b_it, l_gl, l_dm, block = "main") {
    # @title Generate Glazing Parameters
    # @description This function generates the parameters for glazing surfaces based on the given layout and dimensions.
    # @param b_it name of storey to be processed
    # @param l_gl list of active house layouts
    # @param l_dm list of dimensions for each room
    # @param block name of block to be processed
    # @return A list containing the parameters for glazing surfaces.

    # Define parameters for glazing surfaces
    l_face <- c("front", "rear", "left", "right")
    l_pitch <- c("pitch_front", "pitch_rear")

    # Define dimensions for glazing surfaces
    l_gl_sto <- l_gl[[b_it]]

    # Helper function to generate glazing parameters
    f_surface <- function(l, k_boundary, l_idfc = l_idf_blocks) {
      # @title Generate Fenestration Surface IDF
      # @description This function generates an IDF (Input Data File) object for a fenestration surface (e.g., window) based on the given layout and boundary conditions.
      #
      # @param l A list of vertices defining the fenestration surface.
      # @param k_boundary A string representing the boundary condition of the fenestration surface.
      # @param l_idfc A list of IDF blocks, default is `l_idf_blocks`.
      #
      # @return A list containing the IDF fenestration surface object and a summary of its properties.
      #
      # @examples
      # vertices <- list(c(0,0,0), c(1,0,0), c(1,1,0), c(0,1,0))
      # idf_blocks <- list('FenestrationSurface:Detailed' = "Some comment")
      # f_surface(vertices, "Wall", idf_blocks)

      # Define labels for the IDF fenestration surface object
      lbl_block <- "FenestrationSurface:Detailed"
      lbl_factor <- "AutoCalculate"
      lbl_type <- "Window"
      lbl_construction <- "Exterior Window"
      lbl_host <- k_boundary
      lbl_srfc <- paste(k_boundary, "win", sep = ":")
      lbl_surface_multi <- "1.0000"

      # Create list of IDF fenestration surface properties
      l_glz <- c(
        lbl_block, lbl_srfc, lbl_type, lbl_construction,
        lbl_host, "", lbl_factor, "",
        lbl_surface_multi, length(l),
        as.character(lapply(l, function(x) {
          paste(
            sprintf("%.4f", x),
            collapse = ","
          )
        }))
      )

      # Create summary list of IDF fenestration surface properties to be included in cards
      l_sum <- list(
        name = lbl_srfc,
        host = lbl_host,
        type = lbl_type,
        construction = lbl_construction,
        vertices = l
      )

      # Create IDF fenestration surface object
      lbl_block_comment <- lbl_block
      l_cmn <- l_idfc[[lbl_block_comment]]
      l_zon <- add_idf_comment(l_glz, c("", l_cmn))

      return(list(idf = l_zon, info = l_sum))
    }


    get_glazing_face_areas <- function(l_f, l_fga, l_da, l_att) {
      # @title Get Glazing Face Areas
      # @description This function calculates the areas of glazing surfaces for a given face and generates the corresponding IDF objects.
      #
      # @param l_f A string representing the face orientation (e.g., "front", "rear").
      # @param l_fga A list of glazing areas for each face.
      # @param l_da A list containing dimensions and face orientation for the glazing surfaces.
      # @param l_att A string indicating the layout type (e.g., "left", "right").
      #
      # @return A list containing the IDF objects, vertices, and summary information for each glazing surface.
      #
      # @examples
      # face_orientation <- "front"
      # glazing_areas <- list(front = list("glazing-1-2-3" = 10))
      # dimensions <- list(h = 2, w_block = c(4, 5, 6), d_block = c(1, 2, 3))
      # layout_type <- "main"
      # get_glazing_face_areas(face_orientation, glazing_areas, dimensions, layout_type)

      l_g_area <- l_fga[[l_f]]

      l_da <- list(
        face = l_f, face_h = l_da$h,
        face_w = l_da$w_block, face_t = l_da$d_block
      )

      # Function to calculate the vertices and IDF object for a glazing surface
      get_glazing_surface <- function(i_g, l_g, l_d, l_a) {
        # Define pre-set aspect limits
        glz_h_top <- 90 / 100
        glz_w_aspect <- 68 / 100

        # Obtain area of iteration
        area_glaz <- l_g[[i_g]]

        # Obtain location of room
        pos_rgx <- "(^.*)(\\-)(\\d+)(\\-)(\\d+)(\\)$)"
        pos_d <- as.integer(gsub(pos_rgx, "\\3", i_g))
        pos_w <- as.integer(gsub(pos_rgx, "\\5", i_g))

        # Obtain dimensions of room
        len_h <- l_d$face_h
        len_w <- l_d$face_w[pos_w]
        len_d <- l_d$face_t[pos_d]

        # Obtain areas of room: coronal & sagittal
        b_area_coronal <- len_w * len_h
        b_area_sagittal <- len_d * len_h

        # Based on orientation of facing window, obtain window dimensions
        if (l_d$face == "front" | l_d$face == "rear") {
          gc_opp_wall <- len_d
          diff_ratio <- area_glaz / b_area_coronal

          gw_eff <- len_w * glz_w_aspect # % of wall-horizontal
          gh_eff <- area_glaz / gw_eff # glazed-area / wall-horizontal

          gc_width <- len_w * 1 / 2
          gc_height <- (len_h * glz_h_top) - (gh_eff / 2)
        } else {
          gc_opp_wall <- len_w
          diff_ratio <- area_glaz / b_area_sagittal

          gw_eff <- len_d * glz_w_aspect # % of wall-horizontal
          gh_eff <- area_glaz / gw_eff # glazed-area / wall-horizontal

          gc_width <- len_d * 1 / 2
          gc_height <- (len_h * glz_h_top) - (gh_eff / 2)
        }

        # Obtain vertices
        gp_facex0 <- gc_width - gw_eff / 2
        gp_facex1 <- gc_width + gw_eff / 2
        gp_facey0 <- gc_height - gh_eff / 2
        gp_facey1 <- gc_height + gh_eff / 2

        # Assign vertices based on face orientation
        if (l_d$face == "front") {
          p1 <- c(gp_facex0, 0, gp_facey1)
          p2 <- c(gp_facex0, 0, gp_facey0)
          p3 <- c(gp_facex1, 0, gp_facey0)
          p4 <- c(gp_facex1, 0, gp_facey1)
        } else if (l_d$face == "rear") {
          p1 <- c(gp_facex1, gc_opp_wall, gp_facey1)
          p2 <- c(gp_facex1, gc_opp_wall, gp_facey0)
          p3 <- c(gp_facex0, gc_opp_wall, gp_facey0)
          p4 <- c(gp_facex0, gc_opp_wall, gp_facey1)
        } else if (l_d$face == "right") {
          p1 <- c(gc_opp_wall, gp_facex0, gp_facey1)
          p2 <- c(gc_opp_wall, gp_facex0, gp_facey0)
          p3 <- c(gc_opp_wall, gp_facex1, gp_facey0)
          p4 <- c(gc_opp_wall, gp_facex1, gp_facey1)
        } else if (l_d$face == "left") {
          p1 <- c(0, gp_facex1, gp_facey1)
          p4 <- c(0, gp_facex0, gp_facey1)
          p3 <- c(0, gp_facex0, gp_facey0)
          p2 <- c(0, gp_facex1, gp_facey0)
        } else {
          p1 <- p2 <- p3 <- p4 <- NULL
        }

        # Create a label for the glazing surface
        lbl_base <- paste(gsub("\\(|\\)", "", i_g), l_d$face, sep = ":")

        # Arrange vertices and generate IDF object if the area is greater than 0
        if (area_glaz > 0) {
          vg <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
          idf <- f_surface(vg, lbl_base)
        } else {
          idf <- vg <- NULL
        }

        # Return the surface label, vertices, IDF object, and summary information
        return(list(surface = lbl_base, vertices = vg, idf = idf$idf, info = idf$info))
      }

      # Adjust labels based on layout type
      if (!is.null(l_g_area)) {
        if (l_att == "left") {
          lbl_blocks <- gsub("^\\(", "(l", names(l_g_area))
        } else if (l_att == "right") {
          lbl_blocks <- gsub("^\\(", "(r", names(l_g_area))
        } else {
          lbl_blocks <- names(l_g_area)
        }
        names(l_g_area) <- lbl_blocks
      }

      # Calculate glazing surfaces for each face
      l_per_face <- lapply(names(l_g_area), get_glazing_surface, l_g_area, l_da, l_att)
      names(l_per_face) <- names(l_g_area)

      # Extract IDF objects, vertices, and summary information
      l_idf_per_face <- lapply(l_per_face, "[[", "idf")
      l_ver_per_face <- lapply(l_per_face, "[[", "vertices")
      l_inf_per_face <- lapply(l_per_face, "[[", "info")

      # Filter out empty results
      l_idf_per_face <- l_idf_per_face[lengths(l_idf_per_face) > 0L]
      l_ver_per_face <- l_ver_per_face[lengths(l_ver_per_face) > 0L]
      l_inf_per_face <- l_inf_per_face[lengths(l_inf_per_face) > 0L]

      return(list(
        idf = l_idf_per_face,
        vertices = l_ver_per_face,
        info = l_inf_per_face
      ))
    }

    get_glazing_pitch_areas <- function(l_f, l_fga, l_da, l_att) {
      # @title Get Glazing Pitch Areas
      # @description This function calculates the areas of glazing surfaces for a given pitch face and generates the corresponding IDF objects.
      #
      # @param l_f A string representing the face orientation (e.g., "pitch_front", "pitch_rear").
      # @param l_fga A list of glazing areas for each face.
      # @param l_da A list containing dimensions and face orientation for the glazing surfaces.
      # @param l_att A string indicating the layout type (e.g., "left", "right").
      #
      # @return A list containing the IDF objects, vertices, and summary information for each glazing surface.
      #
      # @examples
      # face_orientation <- "pitch_front"
      # glazing_areas <- list(pitch_front = list("glazing-1-2-3" = 10))
      # dimensions <- list(h_roomroof = 2, alpha = 30, w_block = c(4, 5, 6), d_block = c(1, 2, 3))
      # layout_type <- "main"
      # get_glazing_pitch_areas(face_orientation, glazing_areas, dimensions, layout_type)

      l_g_area <- l_fga[[l_f]]

      l_da <- list(
        face = l_f, face_hl = l_da$h_roomroof, face_a = l_da$alpha,
        face_w = l_da$w_block, face_t = l_da$d_block
      )

      # Function to calculate the vertices and IDF object for a glazing surface
      get_glazing_surface <- function(i_g, l_g, l_d) {
        # Get the area for the glazing surface
        a_s <- l_g[[i_g]]

        # Extract position indices from the identifier
        pos_d <- as.integer(gsub("(^.*)(\\-)(\\d+)(\\-)(\\d+)(\\)$)", "\\3", i_g))
        pos_w <- as.integer(gsub("(^.*)(\\-)(\\d+)(\\-)(\\d+)(\\)$)", "\\5", i_g))

        # Calculate effective heights based on face angle and dimensions
        gh_eff_l <- l_d$face_hl
        gh_eff_t <- tan(pi * l_d$face_a / 180) * l_d$face_t[pos_d] + gh_eff_l
        gh_eff <- gh_eff_t / 2
        gw_eff <- a_s / gh_eff

        # Calculate face center coordinates
        gc_facex <- l_d$face_w[pos_w] / 2
        gc_facey <- l_d$face_t[pos_d] / 2
        gc_facez <- gh_eff

        # Calculate face vertices
        gp_facex0 <- gc_facex - gw_eff / 2
        gp_facex1 <- gc_facex + gw_eff / 2
        gp_facey0 <- gc_facey - gh_eff / 2
        gp_facey1 <- gc_facey + gh_eff / 2
        gp_facez0 <- tan(pi * l_d$face_a / 180) * gp_facey0 + gh_eff_l
        gp_facez1 <- tan(pi * l_d$face_a / 180) * gp_facey1 + gh_eff_l

        # Determine vertices based on face orientation
        if (l_d$face == "pitch_front") {
          p1 <- c(gp_facex0, gp_facey1, gp_facez1)
          p2 <- c(gp_facex0, gp_facey0, gp_facez0)
          p3 <- c(gp_facex1, gp_facey0, gp_facez0)
          p4 <- c(gp_facex1, gp_facey1, gp_facez1)
        } else if (l_d$face == "pitch_rear") {
          p1 <- c(gp_facex1, gp_facey0, gp_facez0)
          p2 <- c(gp_facex1, gp_facey1, gp_facez1)
          p3 <- c(gp_facex0, gp_facey1, gp_facez1)
          p4 <- c(gp_facex0, gp_facey0, gp_facez0)
        } else {
          p1 <- p2 <- p3 <- p4 <- NULL
        }

        # Create a label for the glazing surface
        lbl_base <- paste(gsub("\\(|\\)", "", i_g), "top", sep = ":")

        # Generate IDF object if the area is greater than 0
        if (a_s > 0) {
          vg <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
          idf <- f_surface(vg, lbl_base)
        } else {
          idf <- vg <- NULL
        }

        # Return the surface label, vertices, IDF object, and summary information
        return(list(surface = lbl_base, vertices = vg, idf = idf$idf, info = idf$info))
      }

      # Adjust labels based on layout type
      if (!is.null(l_g_area)) {
        if (l_att == "left") {
          lbl_blocks <- gsub("^\\(", "(l", names(l_g_area))
        } else if (l_att == "right") {
          lbl_blocks <- gsub("^\\(", "(r", names(l_g_area))
        } else {
          lbl_blocks <- names(l_g_area)
        }
        names(l_g_area) <- lbl_blocks
      }

      # Calculate glazing surfaces for each face
      l_per_face <- lapply(names(l_g_area), get_glazing_surface, l_g_area, l_da)
      names(l_per_face) <- names(l_g_area)

      # Extract IDF objects, vertices, and summary information
      l_idf_per_face <- lapply(l_per_face, "[[", "idf")
      l_ver_per_face <- lapply(l_per_face, "[[", "vertices")
      l_inf_per_face <- lapply(l_per_face, "[[", "info")

      # Filter out empty results
      l_idf_per_face <- l_idf_per_face[lengths(l_idf_per_face) > 0L]
      l_ver_per_face <- l_ver_per_face[lengths(l_ver_per_face) > 0L]
      l_inf_per_face <- l_inf_per_face[lengths(l_inf_per_face) > 0L]

      return(list(
        idf = l_idf_per_face,
        vertices = l_ver_per_face,
        info = l_inf_per_face
      ))
    }


    if (!b_it %in% c("AT", "PR")) {
      l_per_dir <- lapply(l_face, get_glazing_face_areas, l_gl_sto, l_dm, block)
      names(l_per_dir) <- l_face
    } else {
      if (l_dm$pitched_roof_window == TRUE) {
        l_per_dir <- lapply(l_pitch, get_glazing_pitch_areas, l_gl_sto, l_dm, block)
      } else {
        l_per_dir <- list(NULL, NULL)
      }
      names(l_per_dir) <- l_pitch
    }

    l_idf_per_dir <- lapply(l_per_dir, "[[", "idf")
    l_ver_per_dir <- lapply(l_per_dir, "[[", "vertices")
    l_inf_per_dir <- lapply(l_per_dir, "[[", "info")

    l_idf_per_dir <- l_idf_per_dir[lengths(l_idf_per_dir) > 0L]
    l_ver_per_dir <- l_ver_per_dir[lengths(l_ver_per_dir) > 0L]
    l_inf_per_dir <- l_inf_per_dir[lengths(l_inf_per_dir) > 0L]

    return(list(idf = l_idf_per_dir, vertices = l_ver_per_dir, info = l_inf_per_dir))
  }

  # Load parameters
  l_dm <- b_glz$dim
  l_dm$pitched_roof_window <- b_cub$pitched_roof_window

  # Generate glazing surfaces for each block within the main unit
  if ("AT" %in% names(l_gl) | "PR" %in% names(l_gl)) {
    k_pres_zn <- names(l_bl$main$AT$block)[1]
    l_dm$alpha <- l_bl$main$AT$block[[k_pres_zn]]$faces$.alpha
  }

  # Generate glazing surfaces for aditional units
  b_it <- b_cub$storeys %>%
    .[. != "BB"] %>%
    .[. != "TF"] %>%
    .[. != "PR"]
  b_att <- b_cub$attachment

  l_gl_main <- lapply(b_it, get_parameters_glazing, l_gl, l_dm, "main")
  names(l_gl_main) <- b_it

  l_gl_right <- lapply(b_it, get_parameters_glazing, l_lg, l_dm, "right")
  names(l_gl_right) <- b_it

  l_gl_left <- lapply(b_it, get_parameters_glazing, l_lg, l_dm, "left")
  names(l_gl_left) <- b_it

  # Combine generated objects
  if (grepl("both", b_att)) {
    l_gl <- list(main = l_gl_main, left = l_gl_left, right = l_gl_right)
  } else if (grepl("left", b_att)) {
    l_gl <- list(main = l_gl_main, left = l_gl_left, right = NULL)
  } else if (grepl("right", b_att)) {
    l_gl <- list(main = l_gl_main, left = NULL, right = l_gl_right)
  } else {
    l_gl <- list(main = l_gl_main, left = NULL, right = NULL)
  }

  l_gl <- l_gl[lengths(l_gl) > 0L]

  return(l_gl)
}

generate_surfaces_door <- function(b_cub, b_glz, l_gl, l_dm) {
  #' @title Generate IDF door objects for each of the surfaces
  #'
  #' @param  b_cub information card of house properties
  #' @param  b_glz list of active rooms
  #' @param   l_gl list of glazing dimensions
  #' @param   l_dm list of home info
  #'
  #' @return list of door block by unit/storey/dict,idf
  #' @examples
  #' \dontrun{
  #' generate_surfaces_door(l_cuboid, l_glazing, l_glazed, l_dim)
  #' }

  # Helper function to generate door surfaces
  get_parameters_door <- function(b_it, l_gl, l_dm, l_face = "front", block = "main") {
    # @title Get Door Parameters
    # @description This function calculates the area of each door face based on the given geometric parameters.
    # @param b_it A string representing the boundary condition of the door.
    # @param l_gl A list of door face areas.
    # @param l_dm A list containing door dimensions and face orientation.
    # @param l_face A string representing the face orientation of the door.
    # @param block A string representing the block of the door.
    # @return A list containing the surface label, vertices, IDF object, and summary information.

    l_gl_sto <- l_gl[[b_it]]

    d_surface <- function(l, k_boundary, l_idfc = l_idf_blocks) {
      # @title Create IDF Door Surface
      # @description This function generates an IDF (Input Data File) object for a door surface in an energy simulation model.
      #'
      # @param l A list of vertices defining the door surface.
      # @param k_boundary A string representing the boundary condition of the door.
      # @param l_idfc A list of IDF blocks, default is `l_idf_blocks`.
      #'
      # @return A list containing the IDF door object and a summary of its properties.

      # Define labels for the IDF door object
      lbl_block <- "FenestrationSurface:Detailed"
      lbl_factor <- "AutoCalculate"
      lbl_type <- "Door"
      lbl_construction <- "Exterior Door"
      lbl_host <- k_boundary
      lbl_srfc <- paste(k_boundary, "door", sep = ":")

      # Create list of IDF door properties
      l_glz <- c(
        lbl_block, lbl_srfc, lbl_type, lbl_construction,
        lbl_host, "", lbl_factor, "", "", length(l),
        as.character(lapply(l, function(x) {
          paste(
            sprintf("%.4f", x),
            collapse = ","
          )
        }))
      )

      # create list summary of idf:door properties to be included in cards
      l_sum <- list(
        name = lbl_srfc,
        host = lbl_host,
        type = lbl_type,
        construction = lbl_construction,
        vertices = l
      )

      # create idf:door object
      # lbl_block_comment <- gsub('[[:punct:]]','_',lbl_block)
      lbl_block_comment <- lbl_block
      l_cmn <- l_idfc[[lbl_block_comment]]
      l_zon <- add_idf_comment(l_glz, c("", l_cmn))

      return(list(idf = l_zon, info = l_sum))
    }

    get_door_face_areas <- function(l_f, l_fga, l_da, l_att) {
      # @title Get Door Face Areas
      # @description This function calculates the area of each door face based on the given geometric parameters.
      #
      # @param l_f A string representing the face orientation of the door.
      # @param l_fga A list of door face areas.
      # @param l_da A list containing door dimensions and face orientation.
      # @param l_att A string representing the attachment condition of the door.
      #
      # @return A list containing the surface label, vertices, IDF object, and summary information.

      l_g_area <- l_fga[[l_f]]
      l_g_canvas <- l_g_loc <- !as.logical(l_g_area)
      names(l_g_canvas) <- names(l_g_area)
      l_g_canvas <- as.list(l_g_canvas)
      k_d_width <- sum(l_da$w_block * l_g_loc)

      if (grepl("front|rear", l_f)) {
        l_da <- list(face = l_f, face_h = l_da$h, face_w = k_d_width, face_t = l_da$d_block[1])
      } else {
        l_da <- list(face = l_f, face_h = l_da$h, face_w = l_da$d_block[1], face_t = k_d_width)
      }

      get_door_surface <- function(i_g, l_g, l_d) {
        # @title Get Door Surface
        # @description This function calculates the vertices and IDF object for a door surface based on given geometric parameters.
        #'
        # @param i_g An index or identifier for the host area.
        # @param l_g A list of host areas.
        # @param l_d A list containing door dimensions and face orientation.
        #'
        # @return A list containing the surface label, vertices, IDF object, and summary information.



        # Get the host area based on the index
        area_host <- l_g[[i_g]]

        # Define door geometry parameters
        gh_xup <- 0.05
        gh_eff <- 2.25 - gh_xup
        gw_eff <- 1.25
        ga_eff <- gh_eff * gw_eff

        bd_width <- l_d$face_w
        bd_height <- l_d$face_h
        bd_opp_side <- l_d$face_t

        gc_facex <- bd_width / 2
        gc_facez <- bd_opp_side

        # Calculate door dimensions based on effective geometry
        if (ga_eff < bd_width * bd_height) {
          if (gh_eff < bd_height) {
            if (gw_eff < bd_width) {
              gd_width <- gw_eff
              gd_height <- gh_eff
            } else {
              gd_width <- bd_width * 0.95
              gd_height <- gh_eff
            }
          } else {
            if (gw_eff < bd_width) {
              gd_width <- gw_eff
              gd_height <- gh_eff * 0.95
            } else {
              gd_width <- bd_width * 0.95
              gd_height <- gh_eff * 0.95
            }
          }
        } else {
          gd_width <- bd_width * 0.95
          gd_height <- bd_height * 0.95
        }

        # Calculate door vertices
        gp_facex0 <- gc_facex - gd_width / 2
        gp_facex1 <- gc_facex + gd_width / 2
        gp_facey0 <- 0.05
        gp_facey1 <- gd_height

        # Determine vertices based on door face orientation
        if (l_d$face == "front") {
          p1 <- c(gp_facex0, 0, gp_facey1)
          p2 <- c(gp_facex0, 0, gp_facey0)
          p3 <- c(gp_facex1, 0, gp_facey0)
          p4 <- c(gp_facex1, 0, gp_facey1)
        } else if (l_d$face == "rear") {
          p1 <- c(gp_facex1, gc_facez, gp_facey1)
          p2 <- c(gp_facex1, gc_facez, gp_facey0)
          p3 <- c(gp_facex0, gc_facez, gp_facey0)
          p4 <- c(gp_facex0, gc_facez, gp_facey1)
        } else if (l_d$face == "right") {
          p1 <- c(gc_facez, gp_facex0, gp_facey1)
          p2 <- c(gc_facez, gp_facex0, gp_facey0)
          p3 <- c(gc_facez, gp_facex1, gp_facey0)
          p4 <- c(gc_facez, gp_facex1, gp_facey1)
        } else if (l_d$face == "left") {
          p1 <- c(0, gp_facex1, gp_facey1)
          p4 <- c(0, gp_facex1, gp_facey0)
          p3 <- c(0, gp_facex0, gp_facey0)
          p2 <- c(0, gp_facex0, gp_facey1)
        } else {
          p1 <- p2 <- p3 <- p4 <- NULL
        }

        # Create a label for the door surface
        lbl_base <- paste(gsub("\\(|\\)", "", i_g), l_d$face, sep = ":")

        # Generate IDF object if the host area is valid
        if (isTRUE(area_host)) {
          vg <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
          idf <- d_surface(vg, lbl_base)
        } else {
          idf <- vg <- NULL
        }

        # Return the surface label, vertices, IDF object, and summary information
        return(list(surface = lbl_base, vertices = vg, idf = idf$idf, info = idf$info))
      }

      if (!is.null(l_g_area)) {
        if (l_att == "left") {
          lbl_blocks <- gsub("^\\(", "(l", names(l_g_area))
        } else if (l_att == "right") {
          lbl_blocks <- gsub("^\\(", "(r", names(l_g_area))
        } else {
          lbl_blocks <- names(l_g_area)
        }
        names(l_g_area) <- names(l_g_canvas) <- lbl_blocks
      }

      l_per_face <- lapply(names(l_g_area), get_door_surface, l_g_canvas, l_da)
      names(l_per_face) <- names(l_g_area)

      l_idf_per_face <- lapply(l_per_face, "[[", "idf")
      l_ver_per_face <- lapply(l_per_face, "[[", "vertices")
      l_inf_per_face <- lapply(l_per_face, "[[", "info")

      l_idf_per_face <- l_idf_per_face[lengths(l_idf_per_face) > 0L]
      l_ver_per_face <- l_ver_per_face[lengths(l_ver_per_face) > 0L]
      l_inf_per_face <- l_inf_per_face[lengths(l_inf_per_face) > 0L]

      return(list(
        idf = l_idf_per_face,
        vertices = l_ver_per_face,
        info = l_inf_per_face
      ))
    }


    l_per_dir <- lapply(l_face, get_door_face_areas, l_gl_sto, l_dm, block)
    names(l_per_dir) <- l_face

    l_idf_per_dir <- lapply(l_per_dir, "[[", "idf")
    l_ver_per_dir <- lapply(l_per_dir, "[[", "vertices")
    l_inf_per_dir <- lapply(l_per_dir, "[[", "info")

    l_idf_per_dir <- l_idf_per_dir[lengths(l_idf_per_dir) > 0L]
    l_ver_per_dir <- l_ver_per_dir[lengths(l_ver_per_dir) > 0L]
    l_inf_per_dir <- l_inf_per_dir[lengths(l_inf_per_dir) > 0L]

    return(list(idf = l_idf_per_dir, vertices = l_ver_per_dir, info = l_inf_per_dir))
  }

  b_it <- b_cub$storeys
  b_att <- b_cub$attachment
  b_acc <- b_cub$access

  l_gl_main <- lapply(b_it, get_parameters_door, l_gl, l_dm, b_acc, "main")
  names(l_gl_main) <- b_it

  l_gl_right <- lapply(b_it, get_parameters_door, l_gl, l_dm, b_acc, "right")
  names(l_gl_right) <- b_it

  l_gl_left <- lapply(b_it, get_parameters_door, l_gl, l_dm, b_acc, "left")
  names(l_gl_left) <- b_it


  if (grepl("both", b_att)) {
    l_gl <- list(main = l_gl_main, left = l_gl_left, right = l_gl_right)
  } else if (grepl("left", b_att)) {
    l_gl <- list(main = l_gl_main, left = l_gl_left, right = NULL)
  } else if (grepl("right", b_att)) {
    l_gl <- list(main = l_gl_main, left = NULL, right = l_gl_right)
  } else {
    l_gl <- list(main = l_gl_main, left = NULL, right = NULL)
  }

  l_gl <- l_gl[lengths(l_gl) > 0L]

  return(l_gl)
}

obtain_building_zones <- function(l_lay) {
  #' @title Generate IDF door objects for each of the surfaces
  #'
  #' @param  l_lay list of layout information
  #'
  #' @return list of thermal zones and spaces
  #'
  #' @examples
  #' \dontrun{
  #' obtain_building_zones(l_room_layouts)
  #' }

  # Obtain room parameters
  lbl_flr <- paste0("storey_", names(l_lay))
  lapply(l_lay, "[[", "room_location")
  lapply(l_lay, "[[", "room_assignment")
  lapply(l_lay, "[[", "conjoined_zones")

  # Retrieve conjoined zones
  l_sum <- lapply(l_lay, "[[", "conjoined_zones")
  names(l_sum) <- lbl_flr

  # Parse data frames
  d_sum <- plyr::ldply(l_sum, data.frame, .id = "storey") %>%
    tibble::tibble() %>%
    dplyr::select(storey, room_a, use_a) %>%
    dplyr::rename(zone = room_a, room_function = use_a)
  d_sun <- plyr::ldply(l_sum, data.frame, .id = "storey") %>%
    tibble::tibble() %>%
    dplyr::select(storey, room_b, use_b) %>%
    dplyr::rename(zone = room_b, room_function = use_b)

  # Combine data frames
  d_zon <- d_sum %>%
    dplyr::bind_rows(d_sun) %>%
    dplyr::distinct()
  l_spc <- d_zon %>%
    dplyr::group_by(storey, room_function) %>%
    dplyr::group_split()

  name_space <- function(d) {
    # @title Create Namespace for Space
    # @description This function generates a namespace label for a space based on its storey and room function, and lists its unique zone members.
    #
    # @param d A data frame containing columns `storey`, `room_function`, and `zone`.
    #
    # @return A list containing the namespace label and unique zone members.

    # Create a label by concatenating storey and room function, separated by colons
    lbl <- paste("space", d$storey[1], d$room_function[1], sep = ":")

    # Replace spaces with underscores in the label
    lbl <- gsub("\\s+", "_", lbl)

    # Get unique zone members
    members <- unique(d$zone)

    # Return the label and members as a list
    return(list(label = lbl, members = members))
  }

  l_spc <- lapply(l_spc, name_space)
  lbl_spc <- unlist(lapply(l_spc, "[[", "label"))
  l_spc <- lapply(l_spc, "[[", "members")
  names(l_spc) <- lbl_spc

  l_res <- list(thermal_zones = d_zon, thermal_spaces = l_spc)
  return(l_res)
}

generate_idf_zone_list <- function(l_zns) {
  #' @title Generate IDF list of thermal zones
  #'
  #' @param  l_zns list of available zones
  #'
  #' @return list of thermal zones in idf format

  # Define the IDF object type
  l_tsp <- l_zns$thermal_spaces
  lbl_tsp <- names(l_tsp)

  assign_idf <- function(k, l_ref) {
    # @title Assign IDF Zone List
    # @description This function assigns an IDF (Input Data File) Zone List based on a given key and reference list.
    #
    # @param k A key used to reference the zone list.
    # @param l_ref A list of references for the zones.
    #
    # @return A list containing the IDF Zone List with comments.
    #
    # @examples
    # assign_idf("Zone1", list(Zone1 = c("ZoneA", "ZoneB")))

    # Define the IDF object type
    v_idf <- "ZoneList"

    # Get the reference list for the given key
    v_ref <- l_ref[[k]]

    # Get the comments associated with the IDF object type
    lbl_cmmnts <- l_idf_blocks[[v_idf]]

    # Create the IDF Zone List
    l_zon <- c(
      v_idf,
      k,
      paste(v_ref, collapse = ",")
    )

    # Add comments to the IDF Zone List
    l_zon <- add_idf_comment(l_zon, c("", lbl_cmmnts))
  }

  lbl_block <- make_idf_block_title("ZoneList")
  l_blocks <- lapply(lbl_tsp, assign_idf, l_tsp)

  res <- c("", lbl_block, "", unlist(l_blocks))
  return(res)
}

compose_geometry_idf <- function(l_zns, l_blc, l_glz, l_drr) {
  #' @title Generate and concatenate multiple sections required for the generation
  #' of the geometry of a house in idf format
  #'
  #' @param l_zns list of zones, incl. vertices and pre-generated idf
  #' @param l_blc list of opaque surfaces, incl. vertices and pre-generated idf
  #' @param l_glz list of glazing, incl. vertices and pre-generated idf
  #' @param l_drr list of doors, incl. vertices and pre-generated idf
  #'
  #' @return a sequence of strings in idf format
  #' @examples
  #' \dontrun{
  #' compose_geometry_idf(l_zones, l_blocks, l_glazing, l_doors)
  #' }

  # generate idf:zones block
  l_znl <- generate_idf_zone_list(l_zns)

  # generate idf:opaque-surfaces block
  txt_blc <- lapply(l_blc, function(x) lapply(x, "[[", "idf")) %>%
    as.vector() %>%
    unlist() %>%
    as.character()

  # generate idf:glazing-surfaces block
  txt_glz <- lapply(l_glz, function(x) lapply(x, "[[", "idf")) %>%
    as.vector() %>%
    unlist() %>%
    as.character()
  hdr_glz <- make_idf_block_title("FenestrationSurface:Detailed")
  txt_glz <- c("", hdr_glz, "", txt_glz)

  # generate idf:door-surfaces block
  txt_drr <- lapply(l_drr, function(x) lapply(x, "[[", "idf")) %>%
    as.vector() %>%
    unlist() %>%
    as.character()

  # combine idf blocks
  txt_geo <- c(l_znl, txt_blc, txt_glz, txt_drr)

  return(txt_geo)
}


# aids for summaries -----------------------------------------------------------

obtain_rooms_summary <- function(l_rdim, l_rzns, l_rlys, l_rblc, l_rglz) {
  #' @title Obtain summary of house disaggregated per rooms
  #' @param l_rdim general dimensions of house
  #' @param l_rzns list of thermal zones
  #' @param l_rlys list of detailed room dimensions
  #' @param l_rblc list of idf-blocks
  #' @param l_rglz list of glazing dimensions
  #'
  #' @return dictionary of key characteristics of house
  #' @examples
  #' \dontrun{
  #' obtain_rooms_summary(l_room_dimensions, l_zones, l_room_layouts, l_blocks, l_glazing)
  #' }

  # Helper function to obtain room summary
  parse_room_dimensions <- function(ir, lb, ld) {
    # @title Parse Room Dimensions
    # @description This function parses the dimensions of rooms from a given layout and block data, calculating the area and volume for each room.
    #
    # @param ir An index or identifier for the room.
    # @param lb A list containing layout block data.
    # @param ld A list containing dimension block data.
    #
    # @return A data frame containing the area and volume for each room.

    # Function to read room dimensions and calculate area and volume
    read_room_dimensions <- function(ira, lst, mrv) {
      # Get the room map
      ram <- lst$block[[ira]]$map

      # Check if the room identifier matches specific patterns
      if (grepl("^\\(AT|^\\(TF|^\\(PR", ira)) {
        # Get the heights of the room faces
        rahl <- lst$block[[ira]]$faces$.hl
        rahr <- lst$block[[ira]]$faces$.hr

        # Function to extract heights from IDF data
        extract_idf_heights <- function(txr) {
          txr <- gsub("\\s+", "", txr)
          txr <- txr[grepl("^\\d[\\.?\\d+?]", txr)]
          txr <- gsub("(\\,|\\;)\\!.*", "", txr)

          zi <- NULL
          for (ij in txr) {
            ti <- unlist(strsplit(ij, "[,]"))
            zi <- c(zi, as.numeric(ti[3]))
          }

          return(list(z_low = min(zi), z_high = max(zi)))
        }

        # Extract heights from the IDF data
        l_rah <- extract_idf_heights(lst$idf[[ira]]$f_top)

        # Calculate the volume of the room
        o_vol <- max(mrv * l_rah$z_low * as.logical(ram)) +
          max(mrv * (l_rah$z_high - l_rah$z_low) * as.logical(ram))
      } else {
        # Get the height of the room faces
        rah <- lst$block[[ira]]$faces$.h
        # Calculate the volume of the room
        o_vol <- max(mrv * rah * as.logical(ram))
      }

      # Calculate the area of the room
      o_area <- max(mrv * as.logical(ram))

      return(list(area__m2 = o_area, volume__m3 = o_vol))
    }

    # Get the main layout for the room
    l_sto <- lb$main[[ir]]
    # Get the names of the rooms
    l_rom <- names(l_sto$block)
    l_rom_vol <- NULL

    # Calculate the room volumes based on width and depth blocks
    for (i in ld$w_block) {
      for (j in rev(ld$d_block)) {
        l_rom_vol <- c(l_rom_vol, i * j)
      }
    }
    m_rom_vol <- matrix(l_rom_vol,
      nrow = length(ld$d_block),
      ncol = length(ld$w_block), byrow = FALSE
    )

    # Read dimensions for each room
    l_rom_vol <- lapply(l_rom, read_room_dimensions, l_sto, m_rom_vol)
    names(l_rom_vol) <- l_rom

    # Convert the list of room dimensions to a data frame
    d_rom_dim <- plyr::ldply(l_rom_vol, data.frame, .id = "zone")

    return(d_rom_dim)
  }

  parse_window_presence <- function(ir, lg) {
    # @title Parse Window Presence
    # @description This function identifies the presence of windows on different sides of a room.
    #
    # @param ir An index or identifier for the room.
    # @param lg A list containing geometric data for the room.
    #
    # @return A unique list of window locations.

    # Define the sides of the room to check for windows
    ls_side <- c("front", "rear", "right", "left", "pitch_front", "pitch_rear")

    # Extract window locations from the geometric data
    l_win_loc <- lapply(ls_side, function(x) names(lg[[ir]]$vertices[[x]]))

    # Filter out empty window locations
    l_win_loc <- l_win_loc[lengths(l_win_loc) > 0L]

    # Get unique window locations
    l_win_loc <- unique(unlist(l_win_loc))

    return(l_win_loc)
  }

  extract_room_maps <- function(idm, lr) {
    # @title Extract Room Maps
    # @description This function extracts maps for each room based on a given identifier.
    #
    # @param idm An identifier for the map.
    # @param lr A list containing room data.
    #
    # @return A list of maps for each room.

    # Extract maps for each room based on the identifier
    lres <- lapply(names(lr), function(x) lr[[x]][[idm]])

    # Assign names to the result list
    names(lres) <- names(lr)

    return(lres)
  }

  extract_conjoined_zones <- function(i_zn, l_rooms) {
    # @title Extract Conjoined Zones
    # @description This function extracts conjoined zones for a given zone identifier, filtering and arranging the data.
    #
    # @param i_zn An identifier for the zone.
    # @param l_rooms A list containing room data.
    #
    # @return A data frame of conjoined zones with relevant information.

    # Get the index of the current, previous, and next rooms
    k_act <- grep(i_zn, names(l_rooms))
    k_pre <- k_act - 1
    k_nxt <- k_act + 1

    # Get the names of the current, previous, and next rooms
    k_act <- names(l_rooms)[k_act]
    k_pre <- ifelse(k_pre == 0, "^-$", names(l_rooms)[k_pre])
    k_nxt <- ifelse(k_nxt > length(names(l_rooms)), "^-$", names(l_rooms)[k_nxt])

    # Extract and filter conjoined zones for the current room
    l_rooms_cz <- lapply(l_rooms, "[[", "conjoined_zones")[[k_act]] %>%
      dplyr::filter(void == TRUE) %>%
      dplyr::mutate(redundant = ifelse(grepl(k_pre, room_b), TRUE, FALSE)) %>%
      dplyr::filter(redundant == FALSE) %>%
      dplyr::select(-redundant, -void, -use_b, -code_b) %>%
      dplyr::arrange(use_a, room_a) %>%
      dplyr::rename(use = use_a, code = code_a)

    return(l_rooms_cz)
  }

  extract_room_location <- function(lrin) {
    # @title Extract Room Location
    # @description This function extracts the location and origin of rooms from a given layout.
    #
    # @param lrin A list containing room layout information.
    #
    # @return A list of room locations and their origins.

    # Define the labels for the maps to be extracted
    lbl_maps <- c("room_location", "origins")

    # Extract room maps for the given labels
    l_origins <- lapply(lbl_maps, extract_room_maps, lrin)
    names(l_origins) <- lbl_maps

    # Function to extract room origin based on room location
    extract_room_origin <- function(irl, lrl) {
      # @title Extract Room Origin
      # @description This function extracts the origin of each room from the given layout information.
      #
      # @param irl An identifier for the room layout.
      # @param lrl A list containing room layout information, including origins and room locations.
      #
      # @return A list of rooms with their corresponding origins.

      # Get the origin and location matrices for the room
      m_ori <- lrl$origins[[irl]]
      m_lbl <- lrl$room_location[[irl]]

      # Get the dimensions of the location matrix
      k_row <- dim(m_lbl)[1]
      k_col <- dim(m_lbl)[2]
      k <- 1
      k_set <- list()

      # Iterate through each cell in the location matrix
      for (i in 1:k_row) {
        for (j in 1:k_col) {
          # Store the room and its origin in a list
          k_set[[k]] <- list(room = m_lbl[i, j], origin = m_ori[i, j])
          k <- k + 1
        }
      }

      return(k_set)
    }

    # Extract room origins for each room location
    l_origin <- lapply(names(l_origins$room_location), extract_room_origin, l_origins)
    names(l_origin) <- names(l_origins$room_location)

    return(l_origin)
  }

  merge_conjoined_zones <- function(l_con, l_rzn, l_map) {
    # @title Merge Conjoined Zones
    # @description This function merges conjoined zones from a list of layouts, summarizing the rooms per zone and storey, and identifying suitable zones to be conjoined.
    #
    # @param l_con A list of conjoined zones from layouts.
    # @param l_rzn A list containing thermal zones.
    # @param l_map A list containing room assignments.
    #
    # @return A list of merged conjoined zones.
    #
    # @examples
    # conjoined_zones <- list(layout1 = list(conjoined_zones = data.frame(void = TRUE, room_b = "room2", use_a = "use1", room_a = "room1")))
    # thermal_zones <- list(thermal_zones = data.frame(storey = "storey_1", room_function = "Stairwell", zone = "zone1"))
    # room_assignment <- list(room_assignment = c("storey_1"))
    # merge_conjoined_zones(conjoined_zones, thermal_zones, room_assignment)

    # Extract conjoined zones from the list of layouts
    l_conj <- lapply(names(l_con), extract_conjoined_zones, l_con) %>%
      plyr::ldply(data.frame) %>%
      dplyr::group_by(use)

    # Summarize table of rooms per zone and storey
    l_merged <- l_rzn$thermal_zones %>%
      dplyr::group_by(storey, room_function)

    # Identify suitable zones to be conjoined
    df_merged <- l_merged %>%
      dplyr::group_keys() %>%
      tibble::rowid_to_column(var = "index") %>%
      dplyr::mutate(storey = gsub("^storey_", "", storey)) %>%
      dplyr::mutate(storey = factor(storey,
        levels = names(l_map$room_assignment),
        ordered = TRUE
      )) %>%
      dplyr::mutate(slug = tolower(gsub("[[:punct:]]+", "", room_function))) %>%
      dplyr::mutate(slug = tolower(paste(storey, gsub("\\s+", "_", slug), sep = "---")))

    # Generate list of conjoined lists
    l_merged_sum <- l_merged %>% dplyr::group_map(~ unique(.))
    names(l_merged_sum) <- df_merged$slug

    # Initialize list of conjoined vertical zones
    l_conj_sets <- list()

    # Perform checking of conjoined vertical zones
    if ("Stairwell" %in% l_merged$room_function) {
      if (sum(l_merged$room_function == "Stairwell") > 1) {
        # Extract location information
        df_conj <- lapply(l_con, "[[", "conjoined_zones") %>%
          plyr::ldply(data.frame, .id = "storey") %>%
          dplyr::filter(use_a %in% "Stairwell" & use_b %in% "Stairwell") %>%
          dplyr::select(storey, room_a, room_b) %>%
          dplyr::mutate(
            st_a = gsub("^\\(([[:alnum:]]{,2})-(.*$)", "\\1", room_a),
            st_b = gsub("^\\(([[:alnum:]]{,2})-(.*$)", "\\1", room_b)
          ) %>%
          dplyr::mutate(
            rm_a = gsub("^\\(([[:alnum:]]{,2})-(.*$)", "\\2", room_a),
            rm_b = gsub("^\\(([[:alnum:]]{,2})-(.*$)", "\\2", room_b)
          ) %>%
          dplyr::mutate(rm_a = gsub("\\)", "", rm_a), rm_b = gsub("\\)", "", rm_b)) %>%
          dplyr::mutate(vertical = ifelse(st_a == st_b, FALSE, TRUE))

        # Create list for iteration
        l_conj_vec <- setNames(split(
          df_conj %>% dplyr::select(room_a, room_b),
          seq(nrow(df_conj))
        ), rownames(df_conj)) %>%
          lapply(., as.character)

        # Prepare iterative search of clusters
        seq_vec <- 1:length(l_conj_vec)
        vec_path <- NULL
        res_path <- list()

        # Perform iterative search of clusters
        for (k in seq_vec) {
          seq_chk <- setdiff(seq_vec, k)
          vec_ini <- l_conj_vec[[k]]
          vec_path <- c(vec_path, vec_ini) %>% unique()
          seq_path <- k
          for (j in seq_chk) {
            len_chk <- setdiff(vec_ini, l_conj_vec[[j]])
            if (length(len_chk) > 0) seq_path <- c(seq_path, j)
          }
          vec_path <- l_conj_vec[seq_path] %>%
            unlist() %>%
            as.character() %>%
            unique() %>%
            sort()
          res_path[[k]] <- vec_path
        }

        # Find unique clusters/paths
        l_conj_sets <- unique(res_path)
        names(l_conj_sets) <-
          paste0("house----stairwell_", letters[seq(1, length(l_conj_sets))])

        l_conj_sets <- lapply(
          l_conj_sets,
          function(x) list(zone = x, main = x %>% head(1))
        )
      } else {
        l_filter <- l_merged %>% dplyr::filter(room_function == "Stairwell")
        lbl_filter <- df_merged %>% dplyr::filter(room_function == "Stairwell")
        l_conj_sets <- list(list(
          zone = l_filter$zone,
          main = l_filter$zone
        ))
        names(l_conj_sets) <- lbl_filter$slug
      }
    }

    # Update list and table of zones
    k_id_zones <- df_merged %>%
      dplyr::mutate(out = ifelse(room_function == "Stairwell", TRUE, FALSE)) %>%
      dplyr::filter(out == FALSE) %>%
      dplyr::select(index) %>%
      unlist() %>%
      as.integer()
    l_merged_sum <- l_merged_sum[k_id_zones]
    l_merged_sum <- lapply(
      l_merged_sum,
      function(x) list(zone = x$zone, main = x$zone %>% head(1))
    )

    # Update list
    l_res <- modifyList(l_merged_sum, l_conj_sets)

    return(l_res)
  }

  update_conjoined_spaces <- function(l_rzn, l_con) {
    # @title Update Conjoined Spaces
    # @description This function updates the list of thermal spaces by incorporating conjoined stairwell spaces.
    #
    # @param l_rzn A list containing thermal spaces.
    # @param l_con A list containing conjoined zones.
    #
    # @return A list of updated thermal spaces.
    #
    # @examples
    # thermal_spaces <- list(thermal_spaces = list(space1 = "zone1", space2 = "zone2"))
    # conjoined_zones <- list(stairwell_space = list(zone = "zone3"))
    # update_conjoined_spaces(thermal_spaces, conjoined_zones)

    # Identify conjoined stairwell spaces
    k_con <- grep("stairwell", names(l_con), value = TRUE)
    k_pre <- grep("Stairwell", names(l_rzn$thermal_spaces), value = FALSE)

    # Check if there are conjoined stairwell spaces to update
    if (!identical(k_con, character(0)) & !identical(k_pre, character(0))) {
      # Extract conjoined stairwell zones
      l_con_space <- lapply(k_con, function(x) l_con[[x]]$zone)
      lbl_con_space <- paste0("space:", k_con) %>% gsub("_|----", ":", .)
      names(l_con_space) <- lbl_con_space

      # Update the list of thermal spaces by removing existing stairwell spaces
      l_update <- l_rzn$thermal_spaces[-k_pre]
      names(l_update) <- names(l_update) %>%
        gsub("_|----", ":", .) %>%
        gsub("\\:\\/\\:", "-", .) %>%
        tolower()

      # Merge the updated thermal spaces with the conjoined stairwell spaces
      l_update <- modifyList(l_update, l_con_space)
    } else {
      # If no conjoined stairwell spaces, return the original thermal spaces
      l_update <- l_rzn$thermal_spaces
    }

    return(l_update)
  }


  # extract glazed rooms
  l_win_pres <- lapply(
    names(l_rglz$main),
    parse_window_presence, l_rglz$main
  )
  l_win_pres <- l_win_pres[lengths(l_win_pres) > 0L]
  l_win_pres <- unlist(l_win_pres)

  # extract room areas and volumes
  d_room_dim <- lapply(
    names(l_rlys), parse_room_dimensions, l_rblc, l_rdim
  ) %>% dplyr::bind_rows()

  # merge tables
  d_room_dim <- l_rzns$thermal_zones %>%
    plyr::join(d_room_dim, by = "zone") %>%
    dplyr::mutate(glazed = ifelse(zone %in% l_win_pres, TRUE, FALSE)) %>%
    tibble::tibble()

  # extract room location per storey
  lbl_maps <- c(
    "room_location", "room_assignment",
    "room_floor_links_to", "room_ceiling_links_to",
    "room_left_links_to", "room_right_links_to",
    "room_front_links_to", "room_rear_links_to"
  )
  l_maps <- lapply(lbl_maps, extract_room_maps, l_rlys)
  names(l_maps) <- lbl_maps

  # extract room origins
  l_origin <- extract_room_location(l_rlys)

  # extract conjoined-zones
  l_conjoined <- merge_conjoined_zones(l_rlys, l_rzns, l_maps)

  # updated list of spaces and room members
  l_spaces <- update_conjoined_spaces(l_rzns, l_conjoined)

  return(list(
    info = d_room_dim, map = l_maps, origins = l_origin,
    conjoined = l_conjoined, spaces = l_spaces
  ))
}

obtain_fenestration_summary <- function(l_gl) {
  #' @title Obtain summary data of fenestration objects
  #'
  #' @param l_gl list of generated glazing/door objects
  #'
  #' @return a tibble summary
  #' @examples
  #' \dontrun{
  #' obtain_fenestration_summary(l_glazing)
  #' }

  # Load parameters
  sto <- names(l_gl$main)

  # Helper function to read vertices in storey
  read_vertices_in_storey <- function(ist, lvr) {
    # @title Read Vertices in Storey
    # @description This function reads the vertices of rooms in a given storey and calculates the area of each room face.
    #
    # @param ist An identifier for the storey.
    # @param lvr A list containing vertices data for each storey.
    #
    # @return A data frame containing the room, face, area, and storey information.
    #
    # @examples
    # vertices_data <- list(storey1 = list(vertices = list(front = list(room1 = matrix(c(0, 0, 0, 1, 1, 1), ncol = 3)))))
    # read_vertices_in_storey("storey1", vertices_data)

    # Extract vertices for the given storey
    l_vrt_face <- lvr[[ist]]$vertices
    face <- names(l_vrt_face)

    # Function to read vertices in a face
    read_vertices_in_face <- function(ifc, lvf) {
      # @title Read Vertices in Face
      # @description This function reads the vertices of rooms in a given face and calculates the area of each room face.
      #
      # @param ifc An identifier for the face.
      # @param lvf A list containing vertices data for each face.
      #
      # @return A data frame containing the room, face, and area information.
      #
      # @examples
      # face_vertices <- list(front = list(room1 = matrix(c(0, 0, 0, 1, 1, 1), ncol = 3)))
      # read_vertices_in_face("front", face_vertices)

      # Extract vertices for the rooms in the face
      l_vrt_rooms <- lvf[[ifc]]
      rooms <- names(l_vrt_rooms)

      # Function to read vertices in rooms and calculate area
      read_vertices_in_rooms <- function(ifs, lvs, jfc) {
        # Convert vertices to data frame and transpose
        v_vert <- lvs[[ifs]] %>%
          data.frame() %>%
          t() %>%
          data.frame()
        colnames(v_vert) <- c("x", "y", "z")

        # Calculate dimensions and area based on face orientation
        if (jfc %in% c("front", "rear")) {
          dim_w <- max(v_vert$x) - min(v_vert$x)
          dim_h <- max(v_vert$z) - min(v_vert$z)
          area <- dim_w * dim_h
        } else {
          dim_w <- max(v_vert$y) - min(v_vert$y)
          dim_h <- max(v_vert$z) - min(v_vert$z)
          area <- dim_w * dim_h
        }
        return(list(room = ifs, face = jfc, area = area))
      }

      # Apply the function to each room and combine results
      l_res <- lapply(rooms, read_vertices_in_rooms, l_vrt_rooms, ifc)
      l_res <- plyr::ldply(l_res, data.frame)
      return(l_res)
    }

    # Apply the function to each face and combine results
    l_res <- lapply(face, read_vertices_in_face, l_vrt_face)
    l_res <- l_res %>% dplyr::bind_rows()
    l_res$storey <- ist

    return(l_res)
  }

  l_res <- lapply(sto, read_vertices_in_storey, l_gl$main)
  l_res <- l_res %>%
    dplyr::bind_rows() %>%
    tibble::tibble() %>%
    dplyr::mutate(
      storey = factor(storey, levels = sto, ordered = TRUE),
      face = factor(face)
    ) %>%
    tidyr::pivot_wider(names_from = face, values_from = area) %>%
    dplyr::arrange(storey, room) %>%
    dplyr::group_by(storey)

  return(l_res)
}

obtain_surfaces_info <- function(l_bl, l_gl, l_dr) {
  #' @title Obtain generated list of surface parameters
  #'
  #' @param l_bl list of opaque surfaces
  #' @param l_gl list of glazing surfaces
  #' @param l_dr list of door surfaces
  #'
  #' @return a unified list of surface parameters
  #' @examples
  #' \dontrun{
  #' obtain_surfaces_info(l_blocks, l_glazing, l_doors)
  #' }

  l_bl_rooms <- lapply(l_bl$main, "[[", "info")
  l_gl_rooms <- lapply(l_gl$main, "[[", "info")
  l_dr_rooms <- lapply(l_dr$main, "[[", "info")

  l_bl_rooms <- l_bl_rooms[lengths(l_bl_rooms) > 0L]
  l_gl_rooms <- l_gl_rooms[lengths(l_gl_rooms) > 0L]
  l_dr_rooms <- l_dr_rooms[lengths(l_dr_rooms) > 0L]

  if ("AT" %in% names(l_bl_rooms)) {
    l_att <- l_bl_rooms$AT
    for (k in names(l_att)) {
      l_att_i <- l_att[[k]]
      if (any(sapply(l_att_i$f_top[[1]], is.list))) {
        j_top_a <- l_att_i$f_top[[1]]
        j_top_b <- l_att_i$f_top[[2]]
        l_att_i$f_top <- NULL
        l_att_i$f_top_a <- j_top_a
        l_att_i$f_top_b <- j_top_b
        l_att[[k]] <- l_att_i
      }
    }
    l_bl_rooms$AT <- l_att
  }

  l_res <- list(opaque = l_bl_rooms, glazing = l_gl_rooms, doors = l_dr_rooms)
  return(l_res)
}



# aids for dictionaries --------------------------------------------------------

generate_layout_table <- function(idl, llo, dref = d_rooms) {
  #' @title Generates a layout table for a given room layout.
  #'
  #' @description Creates a data frame representing the room layout based on provided dimensions and locations.
  #'
  #' @param idl Character string specifying the layout ID.
  #' @param llo A list containing room layout information.
  #' @param dref A data frame containing room reference data.
  #'
  #' @return A data frame representing the room layout with additional information.
  #'
  #' @examples
  #' \dontrun{
  #' generate_layout_table("layout1", llo, d_rooms)
  #' }

  # Extract dimensions and locations from layout information
  k_dim <- llo[[idl]]$size
  k_val <- llo[[idl]]$loc

  # Create labels for rooms based on reference data
  k_lbl <- data.frame(id = k_val) %>%
    plyr::join(dref) %>%
    dplyr::select(name) %>%
    unlist() %>%
    as.character()

  # Create a matrix representing the room layout
  d_rom <- matrix(
    data = k_lbl, byrow = FALSE,
    nrow = k_dim$cols, ncol = k_dim$rows
  )

  # Convert matrix to data frame and add additional information
  d_rom <- data.frame(
    cols = rep(1:k_dim$cols, each = k_dim$rows),
    rows = 1:k_dim$rows, use = as.character(d_rom)
  ) %>%
    dplyr::mutate(use = factor(use)) %>%
    dplyr::rename(name = use) %>%
    plyr::join(dref %>%
      dplyr::select(name, colour), by = "name")

  return(d_rom)
}



# main class/definition --------------------------------------------------------

generate_dwelling_geometry <- function(l_d_info, l_layouts = t_layouts) {
  #' @title Generate IDF list of thermal zones
  #'
  #' @param l_d_info list of custom input parameters
  #' @param l_layouts tupple of available layouts
  #' @param l_def list of default cuboid inputs
  #'
  #' @return list of house geometry object as summary-card and idf
  #'


  # .. read defaults ----
  l_dwell_default <- the$GLOBAL_PAR[
    c(
      "access", "flat_roof", "floor_height",
      "origin", "window_to_wall_per_side"
    )
  ]


  # .. parse cuboid info ----
  l_cuboid <- decode_cuboid_inputs(l_d_info, l_dwell_default)


  # .. parse dwelling dimensions ----
  l_dim <- obtain_dimensions(l_cuboid)
  l_room_layouts <- lapply(l_cuboid$storeys, generate_room_layout, l_cuboid, l_dim)
  names(l_room_layouts) <- l_cuboid$storeys


  # .. assign vertical voids
  l_room_layouts <- assign_vertical_voids(l_room_layouts)


  # .. glazing ----
  l_glazing <- obtain_glazing_areas(l_room_layouts, l_dim, l_cuboid)
  l_glazed <- lapply(l_glazing$applicable, obtain_glazing_dimensions, l_room_layouts, l_glazing)
  l_glazde <- lapply(l_glazing$applicable, obtain_glazing_dimensions, l_room_layouts, l_glazing, FALSE)
  names(l_glazed) <- names(l_glazde) <- l_glazing$applicable


  # .. generate IDF: opaque surfaces ----
  l_blocks <- generate_surfaces_walls(l_cuboid, l_room_layouts, l_dim)


  # .. generate IDF: fenestration surfaces (attached to hosting surfaces) ----
  l_glazing <- generate_surfaces_glazing(l_cuboid, l_glazing, l_glazed, l_glazde, l_blocks)


  # .. genrate IDF: door surfaces (attached to hosting surfaces) ----
  l_doors <- generate_surfaces_door(l_cuboid, l_glazing, l_glazed, l_dim)


  # assign thermal zones and zone-lists ----
  l_zones <- obtain_building_zones(l_room_layouts)


  # estimate volumes and areas ----
  l_sum <- obtain_rooms_summary(l_dim, l_zones, l_room_layouts, l_blocks, l_glazing)
  l_zones$thermal_spaces <- l_sum$spaces


  # collect idf objects ----
  l_idf <- compose_geometry_idf(l_zones, l_blocks, l_glazing, l_doors)


  # summarise fenestration objects ----
  l_glazing_sum <- obtain_fenestration_summary(l_glazing)
  l_door_sum <- obtain_fenestration_summary(l_doors)


  # gather all surfaces details
  l_surface_info <- obtain_surfaces_info(l_blocks, l_glazing, l_doors)


  # collect information lists ----
  l_zones$thermal_zones <- l_sum$info
  l_zones$map_zones <- l_sum$map
  l_zones$origins <- l_sum$origins
  l_zones$conjoined <- l_sum$conjoined
  l_zones$glazing <- l_glazing_sum
  l_zones$doors <- l_door_sum
  l_zones$surface_detail <- l_surface_info
  l_card <- list(cuboid = l_cuboid, zones = l_zones)


  return(list(card = l_card, idf = l_idf))
}


# layout prediction ------------------------------------------------------------

predict_room_info <- function(l_models, i_type, i_area, i_nrooms = NULL, i_nstoreys = NULL, d_ref = s_ehs_2011_rom) {
  #' @title Predicts room information based on input parameters.
  #'
  #' @description Uses pre-trained models to predict room-related information for a given dwelling.
  #'
  #' @param l_models A list of prediction models.
  #' @param i_type Character string representing the dwelling type.
  #' @param i_area Numeric value representing the floor area.
  #' @param i_nrooms Optional numeric value specifying the number of rooms.
  #' @param i_nstoreys Optional numeric value specifying the number of storeys.
  #' @param d_ref Data frame containing reference data for room information.
  #'
  #' @return A JSON string containing predicted room information.

  # Create a data frame with input parameters
  df <- data.frame(dwtype7x = i_type, floor_area = i_area)

  # Predict number of storeys if not provided
  if (is.null(i_nstoreys)) {
    df <- df %>%
      dplyr::mutate(number_of_storeys = predict(
        l_models$n_storeys, data.frame(dwtype7x = dwtype7x, floor_area = floor_area)
      ))
  } else {
    df <- df %>%
      dplyr::mutate(number_of_storeys = i_nstoreys)
  }

  # Predict number of rooms if not provided
  if (is.null(i_nrooms)) {
    df <- df %>%
      dplyr::mutate(number_of_rooms = predict(
        l_models$n_rooms, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, storeyx = number_of_storeys)
      ))
  } else {
    df <- df %>%
      dplyr::mutate(number_of_rooms = i_nrooms)
  }

  # Predict room levels and functions
  df <- df %>%
    dplyr::mutate(
      living.floor_level = predict(l_models$living_level, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      kitchen.floor_level = predict(l_models$kitchen_level, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      bedroom.floor_level = predict(l_models$bedroom_level, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      bathroom.floor_level = predict(l_models$bathroom_level, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_1.room_function = predict(l_models$extra_room_1, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_2.room_function = predict(l_models$extra_room_2, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_3.room_function = predict(l_models$extra_room_3, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_4.room_function = predict(l_models$extra_room_4, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_5.room_function = predict(l_models$extra_room_5, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_6.room_function = predict(l_models$extra_room_6, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      extra_rooms_7.room_function = predict(l_models$extra_room_7, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms))
    ) %>%
    dplyr::mutate(
      living.floor_level = levels(d_ref$living.floor_level)[round(living.floor_level, 0)],
      kitchen.floor_level = levels(d_ref$kitchen.floor_level)[round(kitchen.floor_level, 0)],
      bedroom.floor_level = levels(d_ref$bedroom.floor_level)[round(bedroom.floor_level, 0)],
      bathroom.floor_level = levels(d_ref$bathroom.floor_level)[round(bathroom.floor_level, 0)],
      extra_rooms_1.room_function = levels(d_ref$extra_rooms_1.room_function)[round(extra_rooms_1.room_function, 0)],
      extra_rooms_2.room_function = levels(d_ref$extra_rooms_1.room_function)[round(extra_rooms_2.room_function, 0)],
      extra_rooms_3.room_function = levels(d_ref$extra_rooms_1.room_function)[round(extra_rooms_3.room_function, 0)],
      extra_rooms_4.room_function = levels(d_ref$extra_rooms_1.room_function)[round(extra_rooms_4.room_function, 0)],
      extra_rooms_5.room_function = levels(d_ref$extra_rooms_1.room_function)[round(extra_rooms_5.room_function, 0)],
      extra_rooms_6.room_function = levels(d_ref$extra_rooms_1.room_function)[round(extra_rooms_6.room_function, 0)],
      extra_rooms_7.room_function = levels(d_ref$extra_rooms_7.room_function)[round(extra_rooms_7.room_function, 0)]
    ) %>%
    dplyr::mutate(
      living.room_function = predict(l_models$living_function, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      kitchen.room_function = predict(l_models$kitchen_function, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms)),
      bedroom.room_function = predict(l_models$bedroom_function, data.frame(dwtype7x = dwtype7x, floor_area = floor_area, number_of_rooms = number_of_rooms))
    ) %>%
    dplyr::mutate(
      living.room_function = levels(d_ref$living.room_function)[round(living.room_function, 0)],
      kitchen.room_function = levels(d_ref$kitchen.room_function)[round(kitchen.room_function, 0)],
      bedroom.room_function = levels(d_ref$bedroom.room_function)[round(bedroom.room_function, 0)]
    ) %>%
    dplyr::mutate(number_of_storeys = round(number_of_storeys, 0)) %>%
    dplyr::mutate(number_of_rooms = round(number_of_rooms, 0)) %>%
    dplyr::relocate(
      "dwtype7x", "floor_area", "number_of_rooms", "number_of_storeys",
      dplyr::starts_with("living"), dplyr::starts_with("kitchen"),
      dplyr::starts_with("bedroom"), dplyr::starts_with("bathroom")
    )

  # Convert data frame to JSON format
  df <- df %>% jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  return(df)
}

assign_house_layout <- function(k_sto, k_rom, k_bed, k_att, k_bsm, k_dim = c(4, 4), k_shp = "R", l_lay = t_layouts) {
  #' @title Assign Layout Templates Based on House Specifications
  #'
  #' @description This function assigns layout templates for a house based on the number of rooms,
  #' number of storeys, the presence of an attic and basement, and the shape and dimensions of the layout.
  #'
  #' @param k_sto Integer. Number of storeys in the house.
  #' @param k_rom Integer. Total number of rooms in the house.
  #' @param k_bed Character. The lowest storey with a bedroom. Possible values: 'GF' (Ground Floor), '1F' (First Floor), etc.
  #' @param k_att Logical. Indicates whether the house has an attic (TRUE) or not (FALSE).
  #' @param k_bsm Logical. Indicates whether the house has a basement (TRUE) or not (FALSE).
  #' @param k_dim Numeric vector of length 2. Dimensions of the layout, default is c(4,4).
  #' @param k_shp Character. Shape of the layout. Default is 'R' for Rectangular. Other shapes are not yet implemented.
  #' @param l_lay List. A list of layout templates.
  #'
  #' @return A vector of layout templates corresponding to the specified house configuration.
  #'
  #' @examples
  #' \dontrun{
  #' assign_house_layout(k_sto=2, k_rom=5, k_bed='1F', k_att=TRUE, k_bsm=TRUE, k_dim=c(4,4), k_shp='R', l_lay=t_layouts)
  #' assign_house_layout(k_sto=1, k_rom=3, k_bed='GF', k_att=FALSE, k_bsm=FALSE, k_dim=c(2,2), k_shp='R', l_lay=t_layouts)
  #' }


  # Helper to assign room distribution
  set_bedroom_distribution <- function(rom, sto) {
    k_base <- floor(rom / sto)
    k_roms <- rep(k_base, sto)
    k_iter <- rep(0, sto)
    j <- 1

    while (sum(k_roms) < k_rom) {
      k_iter[j] <- 1
      k_roms <- k_roms + k_iter
      k_iter <- rep(0, sto)
      j <- j + 1
    }
    return(k_roms)
  }

  # Helper to assign room layouts
  set_storey_layout <- function(st, dm, type = "mid-floor") {
    typ <- paste0(type, dm)

    if (typ == "mid-floor--4x4") {
      res <- switch(as.character(st),
        "0" = "lay_037",
        "1" = "lay_037",
        "2" = "lay_038",
        "3" = "lay_039",
        "lay_040"
      )
    } else if (typ == "mid-floor-alt--4x4") {
      res <- switch(as.character(st),
        "0" = "lay_037",
        "1" = "lay_037",
        "2" = "lay_038",
        "3" = "lay_039",
        "lay_040"
      )
    } else if (typ == "single-storey--4x4") {
      res <- switch(as.character(st),
        "0" = "lay_031",
        "1" = "lay_031",
        "2" = "lay_032",
        "3" = "lay_033",
        "lay_034"
      )
    } else if (typ == "mid-floor--2x2") {
      res <- switch(as.character(st),
        "0" = "lay_067",
        "1" = "lay_067",
        "2" = "lay_068",
        "3" = "lay_069",
        "lay_070"
      )
    } else if (typ == "mid-floor-alt--2x2") {
      res <- switch(as.character(st),
        "0" = "lay_067",
        "1" = "lay_067",
        "2" = "lay_068",
        "3" = "lay_069",
        "lay_070"
      )
    } else if (typ == "single-storey--2x2") {
      res <- switch(as.character(st),
        "0" = "lay_061",
        "1" = "lay_061",
        "2" = "lay_062",
        "3" = "lay_063",
        "lay_064"
      )
    }

    return(res)
  }


  k_size <- paste0("--", k_dim[1], "x", k_dim[2])

  # filter building shapes
  if (k_shp == "R") {
    if (k_sto <= 1) {
      message("single storey")
      k_loc <- "single-storey"
      l_roms <- set_storey_layout(k_rom, k_size, k_loc)
    } else {
      # message('more than one storey')

      if (k_bed %in% c("GF", "GG")) {
        # message('bedroom in ground floor')
        k_loc <- "mid-floor"
        l_roms <- set_bedroom_distribution(k_rom, k_sto) %>%
          lapply(set_storey_layout, k_size, k_loc) %>%
          unlist()
      } else {
        # message('bedroom in upper floors')
        k_loc <- "mid-floor"
        l_roms <- set_bedroom_distribution(k_rom, k_sto - 1) %>%
          lapply(set_storey_layout, k_size, k_loc) %>%
          unlist()
        l_gnds <- ifelse(k_size == "--4x4",
          ifelse(k_loc == "mid-floor", "lay_034", "lay_036"),
          ifelse(k_loc == "mid-floor", "lay_064", "lay_066")
        )
        l_roms <- c(l_gnds, l_roms)
      }

      # assign attic room layout
      # message('attic/room-in-roof layout')
      k_loc <- "attic-floor"
      l_atts <- ifelse(k_size == "--4x4",
        ifelse(k_loc == "attic-floor", "lay_027", "lay_027"),
        ifelse(k_loc == "attic-floor", "lay_077", "lay_077")
      )
      if (k_att == TRUE) {
        l_roms <- c(l_roms, l_atts)
      }

      # assign basement layout
      # message('basement/cellar layout')
      k_loc <- "basement-cellar"
      l_bsmn <- ifelse(k_size == "--4x4",
        ifelse(k_loc == "basement-cellar", "lay_085", "lay_085"),
        ifelse(k_loc == "basement-cellar", "lay_087", "lay_087")
      )
      if (k_bsm == TRUE) {
        l_roms <- c(l_bsmn, l_roms)
      }
    }
  } else {
    warning("warning: other shapes not yet implemented")
  }

  return(l_roms)
}
