# .. system command wrappers ---------------------------------------------------

r_uname <- function() {
  #' @title Determines the operating system
  #'
  #' @description This function identifies the user's operating system.
  #'
  #' @return A character string representing the operating system ("win", "mac", "linux", or "hpc").
  #'
  #' @details
  #' The function uses a combination of checks based on `Sys.info()` and `.Platform$OS.type` to identify the operating system.
  #' It returns a specific string based on the detected OS:
  #'  - "win" for Windows
  #'  - "mac" for macOS
  #'  - "linux" for Linux (assuming GNU)
  #'  - "hpc" for Linux (other distributions)
  #'
  #' If the function cannot identify the OS, it throws an error message ("Mystery Machine").
  #'
  #' @family R-environment helpers


  # Get operating system information
  os_type <- .Platform$OS.type
  sysname <- Sys.info()["sysname"]

  # Check for Windows
  if (os_type == "windows") {
    return("win")
  }

  # Check for macOS
  if (sysname == "Darwin") {
    return("mac")
  }

  # Check for Linux (GNU)
  if (os_type == "unix" && sysname == "linux-gnu") {
    return("linux")
  }

  # Check for other Linux distributions
  if (os_type == "unix" && sysname == "Linux") {
    return("hpc")
  }

  # Unknown operating system
  stop("Mystery Machine! Unknown operating system detected.")
}

r_whoami <- function() {
  #' @title Gets User Information
  #'
  #' @description Retrieves the user name for the current operating system.
  #'
  #' @return A character string containing a welcome message with the user name.
  #'
  #' @details
  #' The function determines the operating system using `r_uname`.
  #' If the OS is not Windows, it uses the `whoami` command to get the user name.
  #' For Windows, it defaults to "windows user".
  #'
  #' @family R-environment helpers


  # Get operating system
  os <- r_uname()

  # Get user name based on OS
  if (os != "win") {
    # Use the `whoami` command for non-Windows systems
    user <- system("whoami", wait = TRUE, intern = TRUE, ignore.stderr = TRUE)
  } else {
    # Use environment variable for Windows systems
    user <- Sys.getenv("LOGNAME")
  }

  # Create welcome message
  txt_out <- paste("welcome:", user, "------------------------+-")

  return(txt_out)
}

r_mkdir <- function(pathNew, myOS = r_uname()) {
  #' @title Creates a Directory
  #'
  #' @description Creates a directory at the specified path, handling
  #' platform differences.
  #'
  #' @param pathNew The path to the directory to be created.
  #' @param myOS The operating system (defaults to the result of `r_uname()`).
  #'
  #' @details
  #' The function creates a directory at the specified `pathNew`. It handles
  #' Windows and non-Windows systems differently. For Windows, it normalizes
  #' the path and uses `dir.create` to create the directory. For other
  #' systems, it uses the `mkdir -p` command to create the directory and
  #' any necessary parent directories.

  if (myOS == "win" & !dir.exists(pathNew)) {
    # Normalize path and create directory on Windows
    pathNew <- pathNew %>% normalizePath(winslash = "/", mustWork = F)
    dir.create(pathNew, showWarnings = FALSE, recursive = TRUE)
  } else {
    # Create directory on other systems
    system(paste("mkdir -p ", pathNew, "/", sep = ""))
  }
}

r_rm <- function(pathNew, myOS = r_uname()) {
  #' @title Removes a Directory
  #'
  #' @description Removes a directory and its contents at the specified
  #' path, handling platform differences.
  #'
  #' @param pathNew The path to the directory to be removed.
  #' @param myOS The operating system (defaults to the result of `r_uname()`).
  #'
  #' @details
  #' The function removes a directory and its contents at the specified
  #' `pathNew`. It handles Windows and non-Windows systems differently.
  #' For Windows, it normalizes the path and uses `unlink` to remove the
  #' directory and its contents. For other systems, it uses the `rm -rf`
  #' command to remove the directory and its contents.

  if (myOS == "win") {
    # Normalize path and remove directory on Windows
    pathNew <- pathNew %>% normalizePath(winslash = "/", mustWork = FALSE)
    unlink(pathNew, recursive = TRUE, force = TRUE)
  } else {
    # Remove directory on other systems
    system(paste("rm -rf ", pathNew, "/", sep = ""))
  }
}

r_zip <- function(path_to_zip) {
  #' @title Zips a Directory
  #'
  #' @description Zips a directory, handling platform differences.
  #'
  #' @param path_to_zip The path to the file/directory to be zipped.
  #'
  #' @details
  #' The function zips the specified directory using either the
  #' built-in `gzip` function (for non-Windows systems) or
  #' a custom zip function (for Windows) based on the specified `pathZip`
  #' in the `the$.enhub.config` settings.
  
  # The operating system
  myOS = r_uname()

  # Get path to zip executable
  path_bin <- the$.enhub.config$gzip$bin %>% normalizePath(winslash = "/")

  # normalize path
  path_to_zip <- path_to_zip %>% normalizePath(winslash = "/", mustWork = FALSE)

  if (file.exists(path_to_zip)) {
    if (myOS == "win") {
      # zip directory on Windows
      shell(paste(path_bin, "-f", path_to_zip), translate = TRUE)
    } else {
      # zip directory on *nix systems
      system(paste(path_bin, path_to_zip))
    }
  }
}

r_unzip <- function(pathNew, myOS = r_uname(), ...) {
  #' @title Unzips a Compressed File
  #'
  #' @description Unzips a compressed file, handling platform differences.
  #'
  #' @param pathNew The path to the compressed file (without the .gz extension).
  #' @param myOS The operating system (defaults to the result of `r_uname()`).
  #' @param ... Additional arguments to be passed to the unzip function.
  #'
  #' @details
  #' The function unzips the specified compressed file (with a .gz extension)
  #' using either the built-in `gunzip` function (for non-Windows systems) or
  #' a custom unzip function (for Windows) based on the specified `pathZip`
  #' in the `the$.enhub.config` settings.

  if (file.exists(paste0(pathNew, ".gz"))) {
    if (myOS == "win") {
      # Normalize path and unzip file
      pathZip <- paste(the$.enhub.config$gzip$bin, "-d -f") %>%
        normalizePath(winslash = "/")
      pathNew <- paste0(pathNew, ".gz") %>%
        normalizePath(winslash = "/", mustWork = FALSE)
      shell(paste(pathZip, pathNew), translate = TRUE)
    } else {
      # Unzip file on other systems
      system(paste("gunzip", pathNew))
    }
  }
}

r_mv <- function(varFrom, varTo, myOS = r_uname()) {
  #' @title Moves a File or Directory
  #'
  #' @description Moves a file or directory from one location to another,
  #' handling platform differences.
  #'
  #' @param varFrom The path to the file or directory to be moved.
  #' @param varTo The destination path for the file or directory.
  #' @param myOS The operating system (defaults to the result of `r_uname()`).
  #'
  #' @details
  #' The function moves a file or directory from `varFrom` to `varTo`. It
  #' handles Windows and non-Windows systems differently. For Windows, it
  #' uses the `MOVE /Y` command to move the file or directory. For other
  #' systems, it uses the `mv` command to move the file or directory.

  if (myOS == "win") {
    # Normalize paths and move file/directory on Windows
    pathFrom <- varFrom %>% normalizePath(mustWork = FALSE)
    pathTo <- varTo %>% normalizePath(mustWork = FALSE)
    shell(paste("MOVE /Y", pathFrom, pathTo))
  } else {
    # Move file/directory on other systems
    system(paste("mv", varFrom, varTo, sep = " "))
  }
}


# .. project helpers -----------------------------------------------------------

uuid <- function(uppercase = FALSE) {
  #' @title Generates a UUID
  #'
  #' @description Generates a universally unique identifier (UUID) in either
  #' lowercase or uppercase format.
  #'
  #' @param uppercase Logical indicating whether to generate an uppercase
  #' UUID (default: FALSE).
  #'
  #' @return A character string representing the generated UUID.
  #'
  #' @details
  #' The function generates a UUID by randomly sampling hexadecimal digits
  #' and constructing the UUID according to the UUID format. The UUID format
  #' consists of 32 hexadecimal digits separated by hyphens, with specific
  #' constraints on certain digits.
  #'
  #' @examples
  #' uuid() # Generates a lowercase UUID
  #' uuid(TRUE) # Generates an uppercase UUID
  #' @export


  # Define hexadecimal digits
  hex_digits <- c(0:9, letters[1:6])

  # Convert to uppercase if specified
  if (uppercase) {
    hex_digits <- toupper(hex_digits)
  }

  # Define digits for the fourth group (UUID version 4)
  y_digits <- hex_digits[9:12]

  # Generate UUID components
  part1 <- paste0(sample(hex_digits, 8, replace = TRUE), collapse = "")
  part2 <- paste0(sample(hex_digits, 4, replace = TRUE), collapse = "")
  part3 <- paste0("4", paste0(sample(hex_digits, 3, replace = TRUE), collapse = ""), collapse = "")
  part4 <- paste0(sample(y_digits, 1), paste0(sample(hex_digits, 3, replace = TRUE), collapse = ""), collapse = "")
  part5 <- paste0(sample(hex_digits, 12, replace = TRUE), collapse = "")

  # Combine components into UUID
  uuid <- paste(part1, part2, part3, part4, part5, sep = "-")

  return(uuid)
}

timestamp <- function() {
  #' @title Generates a Timestamp
  #'
  #' @description Creates a timestamp string suitable for use in file names.
  #'
  #' @return A character string representing the timestamp.
  #'
  #' @details
  #' The function generates a timestamp in the format "YYYY_MM_DD_HH_MM_SS"
  #' by formatting the current system time, removing punctuation, replacing
  #' spaces with underscores, and converting the string to lowercase.
  #' The resulting timestamp is enclosed with underscores.
  #'
  #' @examples
  #' \dontrun{
  #' timestamp() # Generates a timestamp string
  #' }


  # Get current system time
  current_time <- Sys.time()

  # Format time as a string in the format "Mon DD YY HH:MM:SS"
  time_string <- format(current_time, "%b %d %y %X")

  # Remove punctuation from the formatted time string
  time_string <- gsub("[[:punct:]]", "", time_string)

  # Replace spaces with underscores
  time_string <- gsub(" ", "_", time_string)

  # Convert the string to lowercase
  time_string <- tolower(time_string)

  # Enclose the timestamp with underscores
  timestamp <- paste0("_", time_string, "_")

  return(timestamp)
}

toggle_chronometer <- function() {
  #' @title Toggles a Chronometer
  #'
  #' @description This function starts or stops a chronometer to
  #' measure elapsed time.
  #'
  #' @details
  #' The function uses a global variable `.varChrono` to store the start time.
  #' If `.varChrono` exists, it calculates and displays the elapsed time
  #' and then removes the variable. Otherwise, it sets the start time to
  #' the current `proc.time()`.

  # Check if chronometer is running
  if (exists(".varChrono", where = -1, inherits = TRUE)) {
    # Calculate and display elapsed time
    display_chronometer(proc.time(), .varChrono)

    # Remove chronometer variable
    rm(list = ".varChrono", envir = .GlobalEnv)
  } else {
    # Start chronometer
    .varChrono <<- proc.time()
  }
}

display_chronometer <- function(proc, pt) {
  #' @title Displays Elapsed Time
  #'
  #' @description Calculates and displays the elapsed time
  #' from `proc.time()` values.
  #'
  #' @param proc A `proc.time()` object representing the end time.
  #' @param pt A `proc.time()` object representing the start time (defaults
  #' to the global variable `.varChrono`).
  #'
  #' @details
  #' The function calculates the difference between the `proc`
  #' and `pt` `proc.time()` objects. It replaces any NA values with zero
  #' and then displays the sum of the difference in minutes, rounded
  #' to two decimal places.

  # Calculate elapsed time difference
  elapsed_time <- as.vector(proc - pt)

  # Replace NA values with zero
  elapsed_time[is.na(elapsed_time)] <- 0

  # Calculate total elapsed time in minutes
  total_minutes <- round(sum(elapsed_time) / 60, 2)

  # Display elapsed time message
  message(cat(paste0("-- \033[1;32m", total_minutes, "\033[0m minutes of processing time\n")))
}

get_tables <- function(path_tbl) {
  #' @title Loads Multiple CSV Tables
  #'
  #' @description Loads multiple CSV tables from a specified directory path.
  #'
  #' @param path_tbl The path to the directory containing the CSV files.
  #'
  #' @return A list of data frames, where each data frame corresponds to a CSV file.
  #'
  #' @details
  #' This function loads multiple CSV files from a specified directory path.
  #' For each CSV file, it reads the data, converts column names to
  #' lowercase and replaces dots with underscores, and converts the data
  #' frame to a tibble. The resulting data frames are stored in a list with
  #' names derived from the file names.

  get_table <- function(l_f) {
    #' @title Loads a Single CSV Table
    #'
    #' @param l_f The file path of the CSV file.
    #'
    #' @return A tibble containing the loaded data.

    d_f <- read.csv(l_f)
    colnames(d_f) <- tolower(names(d_f))
    colnames(d_f) <- gsub("\\.", "_", colnames(d_f))
    d_f <- tibble::tibble(d_f)

    return(d_f)
  }

  # Get the base name of the directory
  lbl_root <- basename(path_tbl)

  # Get a list of CSV file paths
  l_tbl <-
    list.files(
      path = path_tbl, pattern = "*.csv",
      full.names = TRUE, recursive = TRUE
    ) %>%
    normalizePath(winslash = "/", mustWork = F)

  # Get the base names of the CSV files
  lbl_tbl <- basename(l_tbl)
  lbl_tbl <- gsub("(tbl|\\.csv|_*)", "", lbl_tbl)

  # Load the CSV files into a list of data frames
  l_tbl <- lapply(l_tbl, get_table)

  # Set names for the list of data frames
  names(l_tbl) <- paste0(lbl_root, ".", lbl_tbl)

  return(l_tbl)
}

get_jsons <- function(path_tbl) {
  #' @title Loads Multiple JSON Files
  #'
  #' @description Loads multiple JSON files from a specified directory path.
  #'
  #' @param path_tbl The path to the directory containing the JSON files.
  #'
  #' @return A list of JSON objects, where each object corresponds to a JSON file.
  #'
  #' @details
  #' This function loads multiple JSON files from a specified directory path.
  #' For each JSON file, it reads the data using `fromJSON`. The resulting
  #' JSON objects are stored in a list with names derived from the file names.

  # Get the base name of the directory
  lbl_root <- basename(path_tbl)

  # Get a list of JSON file paths
  l_tbl <-
    list.files(
      path = path_tbl, pattern = "*.json",
      full.names = TRUE, recursive = TRUE
    ) %>%
    normalizePath(winslash = "/", mustWork = F)

  # Get the base names of the JSON files
  lbl_tbl <- basename(l_tbl)
  lbl_tbl <- gsub("(tbl|\\.json|_*)", "", lbl_tbl)

  # Load the JSON files into a list of JSON objects
  l_tbl <- lapply(l_tbl, jsonlite::fromJSON)

  # Set names for the list of JSON objects
  names(l_tbl) <- paste0(lbl_root, ".", lbl_tbl)

  return(l_tbl)
}

convert_mtoe_to_kwh <- function(data, unit_in = "kWh") {
  #' @title Converts Energy Units Between Mtoe and kWh
  #'
  #' @description Converts energy values between million tonnes of oil
  #' equivalent (Mtoe) and kilowatt-hours (kWh) based on
  #' the conversion factor of 1 Mtoe = 11630 kWh (ref: DUKES).
  #'
  #' @param data The energy values to be converted.
  #' @param unit_in The input unit of the data ("kWh" or "mtoe").
  #'
  #' @return A numeric vector of converted energy values.
  #'
  #' @examples
  #' \dontrun{
  #' convert_mtoe_to_kwh(1, "mtoe")  # Convert 1 Mtoe to kWh
  #' }

  # Conversion factor
  mtoe_to_kwh_factor <- 11630

  # Convert to kWh
  if (unit_in == "kWh") {
    y <- as.numeric(data) / mtoe_to_kwh_factor
    unitOut <- "mtoe"
  } else {
    y <- mtoe_to_kwh_factor * as.numeric(data)
    unitOut <- "kWh"
  }

  # Format output
  res <- format(y, scientific = FALSE)

  return(res)
}

obtain_days_per_month <- function(month, year = NULL) {
  #' @title Obtain number of days in a month
  #' @description This function calculates the number of days in a given month.
  #'
  #' @param month An integer or character representing the month (1-12 or month name).
  #' @param year An optional integer representing the year (defaults to current year).
  #'
  #' @return A data frame with two columns: "Month" (character abbreviation) and "Days" (integer).
  #'
  #' @examples
  #' \dontrun{
  #' obtain_days_per_month(5)  # Get days in May
  #' }
  #'


  # Convert month to integer and get abbreviation
  month_int <- as.integer(month)
  month_abbr <- as.character(
    lubridate::month(lubridate::ymd(010101) + months(month_int - 1),
                     label = TRUE, abbr = FALSE))

  # Handle missing year and create date object
  year <- ifelse(is.null(year), as.numeric(format(Sys.Date(), "%Y")), year)
  date_obj <- as.Date(paste(year, month_int, "01", sep = "-"))

  # Get sequence of dates and calculate number of days
  dates <- seq(date_obj, by = "month", length = 2)
  num_days <- as.numeric(difftime(dates[2], dates[1], units = "days"))

  # Create data frame and return
  data_frame <- data.frame(Month = month_abbr, Days = num_days)
  return(data_frame)
}

make_sim_period <- function(yr) {
  #' @title Creates Simulation Period Data Frame
  #'
  #' @description Generates a data frame containing hourly time stamps
  #' for a specified year.
  #'
  #' @param yr The simulation year
  #'
  #' @return A list containing two data frames: `perMonth` with days per
  #' month and `period` with hourly time stamps.
  #'
  #' @details
  #' This function creates a data frame with hourly time stamps for a
  #' specified year. It also calculates days per month for the year.
  #' The function returns a list containing these two data frames.

  # Get days per month for the year
  d_monthly <- plyr::ldply(lapply(1:12, obtain_days_per_month, yr), data.frame)

  # Create hourly time stamps
  d_days <- data.frame(Hour = seq(c(ISOdate(yr, 1, 1, 0, 0)),
                                  by = "hour",
                                  length.out = sum(d_monthly$Days) * 24
  ))

  # Create data frame with hourly information
  d_days <- d_days %>%
    tibble::tibble() %>%
    dplyr::mutate(
      Months = lubridate::month(Hour, label = TRUE, abbr = FALSE),
      Days = lubridate::day(Hour),
      Weeks = lubridate::week(Hour)
    )

  # Return the results as a list
  return(list(perMonth = d_monthly, period = d_days))
}

rename_model_with_uuid <- function(idn) {
  #' @title Renames model files with UUID
  #'
  #' @description Renames files with a specified ID in multiple
  #' directories using a UUID.
  #'
  #' A UUID is a universally unique identifier, ie. a 128-bit
  #' number used to identify information in computer systems
  #' → [wikidata](https://www.wikidata.org/entity/Q195284)
  #'
  #' @param idn The original file ID/index
  #'
  #' @details This function renames files with a given ID to a
  #' new ID (UUID) in specified directories. It iterates through
  #' the directories in `the$.enhub.paths$eval`, constructs the
  #' old and new file paths, and renames the file using the
  #' `r_mv` function.
  #'
  #' @examples
  #' \dontrun{
  #' # Assuming `id_model` exists and was used to generate the model
  #' rename_with_uuid(id_model)
  #' }
  #'
  #' @export

  # Get directories to process
  l_paths <- the$.enhub.paths$eval[c("A", "B", "C")]

  # Read summary card JSON
  d_sum <- file.path(the$.enhub.paths$eval$A, idn, "summary_card.json") %>%
    jsonlite::read_json()

  # Extract UUID
  uid <- d_sum$uuid

  # Iterate through directories
  for (i in l_paths) {
    # Convert directory path to character
    i <- as.character(unlist(i))

    # Construct old and new file paths
    f_as <- file.path(i, idn) %>% normalizePath(winslash = "/", mustWork = F)
    f_ui <- file.path(i, uid) %>% normalizePath(winslash = "/", mustWork = F)

    # Rename file if directory exists
    if (dir.exists(i)) r_mv(f_as, f_ui)
  }

  idf_built_status("Directories renamed with UUID")
  message(paste(idn, "\u2192", uid))
}

get_ep_location <- function() {
  #' @title Gets EnergyPlus Binary Location
  #'
  #' @description Determines the location of the EnergyPlus binary file.
  #'
  #' @return The path to the EnergyPlus binary directory.
  #'
  #' @details
  #' This function retrieves the EnergyPlus binary directory path from
  #' the `the$.enhub.config` settings. It checks if the directory exists and
  #' prints a message accordingly.
  #'
  #' @note
  #' The function currently only supports retrieving the path from the
  #' configuration and does not implement platform-specific search logic.
  #'   - Windows: `cmd >> dir /b /s .\\*RunEPlus.bat`
  #'   - macOS: `mdfind -name runenergyplus`
  #'   - Linux: `find / -name runenergyplus`
  #' @seealso [get_enhub_location()] for a function that checks the location
  #' of the project directory.
  #' @examples
  #' \dontrun{
  #' get_ep_location() # Retrieves the EnergyPlus binary directory path
  #' }


  # Retrieve the EnergyPlus binary directory path from the configuration
  path <- normalizePath(the$.enhub.config$eplus$dir)

  # Check if the directory exists
  if (dir.exists(path)) {
    message("EnergyPlus directory found")
  } else {
    stop("EnergyPlus directory NOT found")
  }

  return(path)
}

get_enhub_location <- function() {
  #' @title Gets EnHub Project Location
  #'
  #' @description Determines the current EnHub project location based
  #'  on the operating system.
  #'
  #' @return The normalized path to the EnHub project directory.
  #'
  #' @details
  #' This function retrieves the current working directory (project location)
  #' using platform-specific methods. It normalizes the path for consistency
  #' across operating systems.
  #'
  #' @seealso [get_ep_location()] for a function that checks the location
  #' of the EnergyPlus (available) folder.
  #'
  #' @examples
  #' \dontrun{
  #' get_enhub_location() # Retrieves the EnHub directory path
  #' }


  # Normalizes the path based on operating system
  normalise_path <- function(type) {
    switch(type,
           win = getwd() %>% normalizePath(winslash = "/"),
           hpc = system("pwd", intern = T),
           mac = getwd(),
           linux = getwd()
    )
  }

  # Get operating system and normalize path
  path <- normalizePath(normalise_path(r_uname()))

  return(path)
}

check_enhub_setup <- function() {
  #' @title Checks EnHub Setup
  #'
  #' @description Verifies the existence of required directories for EnHub operations.
  #'
  #' @details
  #' This function checks if the specified EnergyPlus and gzip directories exist.
  #' If either directory is missing, an error is thrown.

  # Get operating system
  os <- r_uname()

  # Check for EnergyPlus directory
  eplus_dir <- the$.enhub.config$eplus$dir
  if (!dir.exists(eplus_dir)) {
    stop("EnergyPlus directory not found: ", eplus_dir)
  }

  # Check for gzip executable
  gzip_bin <- the$.enhub.config$gzip$bin
  if (!file.exists(gzip_bin)) {
    stop("Gzip executable not found: ", gzip_bin)
  }

}

validate_sim_period <- function(yr, day) {
  #' @title Validates Simulation Period
  #'
  #' @description Validates the provided start and end dates
  #' for a simulation period.
  #'
  #' @param yr The simulation year.
  #' @param day A vector of length 4 containing start and end
  #' day (day, month) as numeric values.
  #'
  #' @return None. The function exits normally if validation passes,
  #' and throws an error if validation fails.
  #'
  #' @details
  #' This function checks if the combination of `yr` and `day` (start
  #' and end day) can be successfully converted to valid ISO dates
  #' using `ISOdate`. If either conversion fails (resulting in NA), the
  #' function stops with an error message.
  #'
  #' @examples
  #' \dontrun{
  #' # Valid period
  #' validate_sim_period(2024, c(1, 1, 12, 31))
  #'
  #' # Invalid period (February with 31 days)
  #' validate_sim_period(2024, c(1, 2, 2, 31))
  #' }


  # Convert start date components to an ISO date
  start_date <- ISOdate(yr, day[1], day[2])

  # Convert end date components to an ISO date
  end_date <- ISOdate(yr, day[3], day[4])

  # Check if either start or end date conversion resulted in NA
  if (is.na(start_date) | is.na(end_date)) {
    stop("Invalid simulation period. Please verify start and end dates.")
  }
}

get_eplus_version <- function(path) {
  #' @title Extracts EnergyPlus Version from Path
  #'
  #' @description Extracts the EnergyPlus version from a given path string.
  #'
  #' @param path The path string to extract the version from.
  #'
  #' @return The extracted EnergyPlus version (currently only supports 9.3).
  #'
  #' @details
  #' This function checks if the provided path string contains a
  #' version number matching '9-3-X' or '9.3.X' format. If a match is
  #' found, it returns 9.3; otherwise, it stops with an error message
  #' indicating that EnergyPlus version 9.3 is required.

  # Define the required EnergyPlus version
  required_version <- 24.1

  # Regular expression to match the version format
  version_pattern <- "24-1-\\d+|24\\.1\\.\\d+"

  # Check if the path contains the required version
  if (grepl(version_pattern, path)) {
    return(required_version)
  } else {
    stop("EnergyPlus version ", required_version, " is required.")
  }
}

assign_font <- function() {
  #' @title Assigns Font Name for Plotting
  #'
  #' @description Assigns the font name for plotting, checking availability on macOS.
  #'
  #' @return The specified font name.
  #'
  #' @details
  #' This function assigns the font name for plotting, as defined
  #' in `the$.enhub.config$font$name`. On macOS, it checks the availability
  #' of the font using `fc-list` command. If the font is available, a
  #' message is printed; otherwise, a warning is issued.
  #'

  # Get font name from configuration
  font_name <- the$.enhub.config$font$name

  # Check operating system
  os <- r_uname()

  # Check font availability on macOS
  if (os == "mac") {

    # List available fonts
    cmd <- "fc-list : file family | grep \\/Library"
    font_list <- system(command = cmd, intern = TRUE)

    # Check if the specified font is available
    font_found <- grepl(font_name, font_list)

    # Print message based on font availability
    if (any(font_found)) {
      message("Font available")
    } else {
      warning("Font NOT available")
    }
  }

  return(invisible(NULL))
}

prepare_enhub_output_dir <- function() {
  #' @title Prepares Output Directories
  #'
  #' @description Creates a set of output directories for EnHub simulations.
  #'
  #' @details
  #' This function creates the necessary output directories for
  #' EnHub simulations, including directories for simulations,
  #' reports, sensitivity analysis, scenarios, and selection.

  # Define output directories
  output_dirs <- c(
    the$.enhub.paths$eval$A, the$.enhub.paths$eval$B, the$.enhub.paths$eval$C, the$.enhub.paths$store$reports,
    the$.enhub.paths$store$sensitivity, the$.enhub.paths$store$scenarios, the$.enhub.paths$store$selection
  )

  # Create output directories
  lapply(output_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
}

read_idf <- function(dir_path, file_path) {
  #' @title Reads an IDF File
  #'
  #' @description Reads an IDF file and returns its contents as a character vector.
  #'
  #' @param dir_path The directory path of the IDF file.
  #' @param file_path The file name of the IDF file.
  #'
  #' @return A character vector containing the lines of the IDF file.
  #'
  #' @details
  #' This function reads an IDF file from the specified directory and file path.
  #' The file content is read as a character vector, with tabs removed.

  # Construct the full file path
  file_path <- file.path(dir_path, file_path) %>% normalizePath()

  # Read the IDF file content
  file_content <- readLines(file_path)

  # Remove tabs from the file content
  cleaned_content <- gsub("\t", "", file_content)

  return(cleaned_content)
}

reset_folders <- function(path_names = NULL){
  #' @title Resets specified folders
  #'
  #' @description This function resets a set of specified folders by
  #' removing their contents and recreating them.
  #'
  #' @param path_names A character vector specifying the folders to reset.
  #' Possible values include "simulation", "scenarios", "sensitivity", "profiles".
  #'
  #' @details
  #' The function identifies the folders to be reset based on the
  #' provided `path_names` argument. It then removes the contents of these
  #' folders and recreates them. Finally, it prints a list of the
  #' recreated folders.
  #'
  #' @examples
  #' \dontrun{
  #' # Reset simulation and scenarios folders only
  #' reset_folders(c("simulation", "scenarios"))
  #'
  #' # Reset all folders
  #' reset_folders()
  #' }
  #'
  #' @export
  #'

  # Define the directories to reset
  dir_to <- c("simulation", "scenarios", "sensitivity", "profiles")

  # Check if path_names is NULL or valid
  if (is.null(path_names)) {
    path_names <- dir_to
  } else if (!all(path_names %in% dir_to)) {
    stop(paste("Only the following folders can be reset:", paste(dir_to, collapse = ", ")))
  }

  # Define a list mapping path_names values to their corresponding subdirectories
  path_map <- list(
    simulation = c(
      the$.enhub.paths$eval$A,
      the$.enhub.paths$eval$B,
      the$.enhub.paths$eval$C
    ),
    scenarios = the$.enhub.paths$store$scenarios,
    sensitivity = the$.enhub.paths$store$sensitivity,
    profiles = c(
      the$.enhub.paths$store$household,
      the$.enhub.paths$store$variable
    )
  )

  # Collect the corresponding subdirectories
  path_sub <- unlist(path_map[path_names])

  # Remove empty paths
  path_sub <- path_sub[path_sub != ""]

  # If no valid paths were found, issue a warning
  if (length(path_sub) == 0) {
    warning("No valid folders specified to reset.")
    return()
  }

  # Remove contents and recreate the directories
  suppressWarnings(invisible(
    lapply(path_sub, function(path) {
      unlink(path, recursive = TRUE, force = TRUE)
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    })
  ))

  # Print the list of recreated folders
  message(cat(
    list.dirs(path_sub, full.names = TRUE, recursive = TRUE),
    sep = "\n"
  ))

}
