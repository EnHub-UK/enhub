define_weather_file_source <- function(hid, lstEHS = s_ehs_2011_ext) {
  #' @title Define Weather File Source
  #'
  #' @description This function defines the weather file source for a given dwelling.
  #' It determines the weather file source based on dwelling location and user preferences.
  #'
  #' @param hid The ID of the dwelling.
  #' @param lstEHS A list containing EHS data.
  #'
  #' @return A list containing the weather file source and region information.

  # dwelling identifiers
  lstIDs <- the$core_index

  # Get dwelling identifier and ID
  varID <- lstIDs[hid, ]
  id <- get_aacode(varID$index)

  # Retrieve dwelling information
  tblDwl <- subset(lstEHS$dwell, V001_HousingCode == id)

  # Determine weather file source based on user preferences
  if (is.null(the$GLOBAL_PAR$use_weather_file)) {
    # If no default weather file is specified, use the survey data
    use_epw <- "<from survey>"
    reg_svy <- as.character(tblDwl$D003_Region)
  } else {
    # If a default weather file is specified, use it
    use_epw <- the$GLOBAL_PAR$use_weather_file
    reg_svy <- "-"
  }

  # Return weather file source and region information
  l_epw <- list(use_weather_file = use_epw, region_in_survey = reg_svy)
  return(l_epw)
}

locate_weather_files <- function(fname, l.par, d.epw = d_stations) {
  #' @title Locate and Download Weather Files
  #'
  #' @description Identifies and downloads weather files based on provided parameters and user preferences.
  #'
  #' @param fname Character string specifying the weather file name or source.
  #' @param l.par List containing weather file source information.
  #' @param d.epw Data frame containing weather station information.
  #'
  #' @return A list containing weather files and metadata.


  get_epw_info <- function(fname, df) {
    # @title Retrieve Weather File Information
    #
    # @param fname File name.
    # @param df Data frame containing weather station information.
    # @return Data frame with weather file information.
    file.zip <- paste0(fname, ".zip")
    info <- df %>%
      dplyr::filter(filepack == file.zip) %>%
      dplyr::select(-filepack, -regional_default)
    return(info)
  }

  list_weather_files <- function(ex, dir_path) {
    # @title List Weather Files in a Directory
    #
    # @param ex File extension.
    # @param dir_path Directory path.
    # @return Character vector of file paths.
    list.files(dir_path, full.names = TRUE, pattern = ex)
  }

  download_weather_files <- function(fn, ds, ex) {
    # @title Download Weather Files
    #
    # @param fn File name.
    # @param ds Destination directory.
    # @param ex File extensions.

    download_from_repo <- function(ext, f, d) {
      # @title Download from Repository
      #
      # @param ext File extension.
      # @param f File name.
      # @param d Destination directory.
      url <- the$.enhub.config$epw$url
      from <- paste0(url, f, ".", ext)
      dest <- file.path(d, paste0("data.", ext))
      res <- lapply(from, download.file, dest, quiet = TRUE)
      invisible(res)
    }

    dir.create(ds, recursive = TRUE, showWarnings = FALSE)
    res <- lapply(ex, download_from_repo, fn, ds)
    invisible(res)
  }

  # Determine weather file name based on user preferences
  if (fname %in% c("<from survey>", "", "-")) {
    # message('Location assumed from EHS region ... !!')
    region <- l.par$region_in_survey
    lbl <- d.epw %>%
      dplyr::filter(regional_default == region) %>%
      dplyr::select(filepack)
    fname <- gsub("\\.zip$", "", lbl$filepack)
  }

  # Construct directory path and list of extensions
  dir.epw <- file.path(the$.enhub.paths$epw, "stations", fname)
  l_ext <- c("epw", "ddy", "clm", "rain", "stat", "wea")

  # Check if files exist and download if necessary
  message("\n")
  if (dir.exists(dir.epw)) {
    message("-- EPW files are ready!")
  } else {
    message("-- EPW files are being downloaded ...")
    download_weather_files(fname, dir.epw, l_ext)
  }

  # List available weather files
  l_files <- lapply(l_ext, list_weather_files, dir.epw)
  names(l_files) <- l_ext

  # Add weather file information
  l_files[["info"]] <- get_epw_info(fname, d.epw)

  # EP-bug: .rain is not recognized
  file_src <- l_files$epw %>% gsub("\\.epw", "", .)
  file.copy(from = paste0(file_src, ".rain"), to = paste0(file_src, ".rain.csv"))
  l_files$rain <- l_files$rain[grepl("\\.rain$", l_files$rain)]

  return(l_files)
}
