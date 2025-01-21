check_external_dependencies <- function(fileout = "config/external.yaml", dirbase = getwd(), redefine = FALSE) {
  #' @title Check external dependencies
  #'
  #' @description This function checks the location of `EnergyPlus` and `Gzip`
  #' based on the operating system. It prompts the user to confirm or edit
  #' the configuration values and saves the updated configuration to a YAML
  #' file located in `<root>/config/external.yaml`.
  #'
  #' Typical content of the YAML file will be:
  #' ```yaml
  #' eplus:
  #'  dir: /Applications/EnergyPlus-24-1-0/
  #' gzip:
  #'  bin: /usr/bin/gunzip
  #' font:
  #'  name: CMU Concrete
  #' ```
  #'
  #' @param fileout The path to the YAML file where the configuration will
  #'  be saved. Default is `config/external.yaml`.
  #' @param dirbase The path of the directory where fileout is to be stored.
  #'  Default is `getwd()`.
  #' @param redefine A logical value indicating whether the configuration is
  #'  working for the user or it needs to be generated. Default is `FALSE`,
  #'  although it checks if the file exists.
  #'
  #' @return None

  # Function to prompt user for input
  get_input <- function(prompt) {
    # Display the prompt
    cat(prompt)
    # Read user input from stdin
    return(readLines(stdin(), n = 1))
  }

  # Function to confirm or edit a configuration value
  confirm_or_edit <- function(key, value) {
    # Function to check if a path exists
    path_exists <- function(path) {
      return(file.exists(path))
    }

    repeat {
      # Display current value
      cat(sprintf("Current value for %s: %s\n\n", key, value))

      # Prompt user to confirm or edit
      choice <- get_input("Press 1 to confirm, 2 to edit: ")

      # Get new value from user if choice is 2
      if (choice == "2") {
        value <- get_input(sprintf("Enter new value for %s: ", key))
      }

      # Check if the path exists
      if (path_exists(value)) {
        message(cat(paste0("\033[1;35m", pass_icon, "\033[0m ", "success! \n\n")))
        break
      } else {
        # Display error message and repeat the loop
        message(error_icon, " The path does not exist. Please enter a valid path.\n\n")
      }
    }

    # Return the confirmed or edited value
    return(value)
  }


  # Define location of configuration file
  fileout <- file.path(dirbase, fileout)

  # Detect the operating system
  os_type <- as.character(unlist(Sys.info()["sysname"]))

  # Some visual elements
  pass_icon <- "\u2714"
  error_icon <- "\u2716"

  # Set the EnergyPlus directory based on the operating system
  ep_dir <- switch(os_type,
    "Darwin" = "/Applications/EnergyPlus-24-1-0/",
    "Linux" = "/usr/local/EnergyPlus-24-1-0/",
    "Windows" = "U:\\/Applications\\/EnergyPlus-24.1.0",
    stop("Unsupported OS")
  )

  # Set the gzip binary path based on the operating system
  gz_dir <- switch(os_type,
    "Darwin" = "/usr/bin/gzip",
    "Linux" = "/usr/bin/gzip",
    "Windows" = "U:\\/Applications\\/Gzip\\/gzip.exe",
    stop("Unsupported OS")
  )
  if (!file.exists(gz_dir)) {
    gz_dir <- Sys.getenv("R_ZIPCMD", "zip")
  }

  # Set the font name
  font_name <- "CMU Concrete"

  # Clear the console
  cat("\014")

  # Display welcome message
  message("Configuration setup:\n")

  # Read or create a list to store configuration values
  if (file.exists(fileout)) {
    # Display a message to the user
    message(cat(paste0(
      "The configuration file exists here: ",
      "\033[0;34m", fileout, "\033[0m\n"
    )))
    file_config <- yaml::read_yaml(fileout)
  } else {
    # Display a message to the user
    message(cat(paste0(
      "The configuration file needs to be created... \n"
    )))
    message(cat(paste0(
      "Two external dependencies are required: ", "EnergyPlus & Gzip\n"
    )))

    file_config <- list(
      eplus = list(
        dir = ep_dir # EnergyPlus directory
      ),
      gzip = list(
        bin = gz_dir # Gzip binary path
      ),
      font = list(
        name = font_name # Font name
      )
    )
  }

  # Interact with user to confirm or edit values
  if (redefine == TRUE) {
    file_config$eplus$dir <-
      confirm_or_edit("EnergyPlus directory", file_config$eplus$dir)
    file_config$gzip$bin <-
      confirm_or_edit("Gzip binary", file_config$gzip$bin)
  }

  # Extract the directory path
  dirout <- dirname(fileout)

  # Check if the directory exists, if not, create it
  if (!dir.exists(dirout)) dir.create(dirout, recursive = TRUE)

  # Save the updated configuration back to a YAML file
  yaml::write_yaml(file_config, fileout)

  # Clear the console
  cat("\014")

  # Display welcome message
  message(" \033[1;35m", "Great!", "\033[0m ", "\n")

  # Print the configuration in JSON format
  cat(jsonlite::toJSON(file_config, pretty = TRUE, auto_unbox = TRUE))

  # Display a message to the user
  message(cat(paste0(
    "\n\n", "\033[1;35m", pass_icon, "\033[0m ",
    "configuration stored in ", "\033[0;32m", fileout, "\033[0m\n"
  )))
}

define_global_parameters <- function(fileout, dirbase = getwd(), redefine = FALSE) {
  #' Save Dictionary as JSON File
  #'
  #' This function saves a given dictionary (list) as a JSON file. It creates the
  #' necessary directories if they do not exist, located in `<root>/config/global.json`.
  #'
  #' @param fileout A string representing the output file path.
  #' @param dirbase A string representing the base directory path.
  #' @param redefine A boolean indicating whether to redefine and save the dictionary.
  #' @return None

  # Some visual elements
  pass_icon <- "\u2714"

  # Example usage
  my_dict <- list(
    "access" = "front",
    "battery" = FALSE,
    "cooling_setpoints" = "constant",
    "domestic_hot_water_temperature" = 72.445645,
    "door_transmittance" = NA,
    "energy_system" = NA,
    "enery_use_intensity" = "normal",
    "flat_roof" = FALSE,
    "floor_height" = 2.9,
    "floor_transmittance" = NA,
    "flow_service" = 0.0018,
    "glazing_ratio" = NA,
    "ground_floor_height" = NA,
    "heat_pump_cop" = 3.2,
    "heating_ashp" = "integrated",
    "heating_availability" = "intermittent",
    "heating_fraction_schedule" = "",
    "heating_gshp" = "borehole",
    "heating_hp_variation" = "high-temp",
    "heating_setpoints" = "constant",
    "heating_system_code" = NA,
    "heating_system_efficiency" = NA,
    "heating_water_temperature" = 70.445645,
    "home_appliances_on" = TRUE,
    "house_front_orientation" = NA,
    "ideal_load_energy_system" = FALSE,
    "infiltration_mixing" = FALSE,
    "infiltration_mode" = "simple",
    "layout_range" = c(2, 2),
    "low_zero_carbon_code" = NA,
    "occupancy_on" = TRUE,
    "orientation_front" = NA,
    "origin" = c(0, 0, 0),
    "outdoor_air_flow" = 0.0094,
    "outdoor_air_schedule" = "Always On",
    "roof_transmittance" = NA,
    "room_in_roof_height" = NA,
    "session_seed" = 8151623,
    "set_point_cooling" = 27.0102,
    "set_point_heating" = 19.5083,
    "simulation_period" = c(1, 1, 12, 31),
    "simulation_timestep" = 4,
    "simulation_year" = 2021,
    "storey_height" = NA,
    "total_floor_area" = NA,
    "underfloor_heat" = FALSE,
    "use_weather_file" = NA,
    "uuid" = NA,
    "ventilation_on" = TRUE,
    "wall_thickness" = NA,
    "wall_transmittance" = NA,
    "wall_type" = NA,
    "window_to_wall_per_side" = list(
      "front" = 25,
      "left" = 25,
      "rear" = 25,
      "right" = 25
    ),
    "window_transmittance" = NA
  )

  # Define the file path
  fileout <- file.path(dirbase, fileout)

  if (redefine | !file.exists(fileout)) {
    # Extract the directory path
    dir_path <- dirname(fileout)

    # Check if the directory exists, if not, create it
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

    # Write the JSON string to the file
    jsonlite::write_json(my_dict, fileout, pretty = TRUE, auto_unbox = TRUE)

    # Display a message to the user
    message(cat(paste0(
      "\n", "\033[1;35m", pass_icon, "\033[0m ",
      "global parameters stored in ", "\033[0;32m", fileout, "\033[0m\n"
    )))
  }
}

init_project <- function(core_data = "ehs", core_mode = "def", fresh_setup = TRUE) {
  #' @title Initialize the Project
  #'
  #' @description This function sets up the `R` environment by loading necessary
  #' libraries, setting up the session, and loading or generating data as needed.
  #'
  #' @param core_data A character string indicating the type of data.
  #' @param core_mode A character string indicating the mode of operation.
  #' @param fresh_setup A logical value indicating whether to perform a fresh setup.
  #' @return Invisible NULL.
  #' @export

  # Helper functions
  print_divider <- function(pad = "-", gap = 2L) {
    # @title Prints a divider line in console
    # @param pad A character string representing the padding character.
    # @param gap An integer specifying the gap between the padding characters.
    # @return A character string representing the divider line.
    paste0(rep(pad, getOption("width") - gap), collapse = "")
  }

  initialise_config <- function(config_yaml_path, default_params_json_path) {
    # @title Initialize EnHub Environment
    #
    # @description This function initializes the EnHub environment by loading configuration
    # settings, defining main directories, and preparing output directories.
    #
    # @param config_yaml_path A character string specifying the path to
    # the configuration YAML file.
    # @param default_params_json_path A character string specifying the
    # path to the default parameters JSON file.
    #
    # @return Returns `TRUE` invisibly after initializing the environment.
    
    # Helper to group objects from environemnt
    group_objects <- function(i_str, i_env = kenv) {
      # @title Group Objects by Pattern
      #
      # @description This function groups objects in the global environment based on a given pattern.
      # It returns a list of grouped objects with names derived from the original object names.
      #
      # @param i_str The pattern to match object names.
      # @param i_env The environment to search for objects.
      # @return A list of grouped objects.

      # Get object names matching the pattern
      o <- ls(pattern = i_str, envir = i_env, all.names = TRUE)

      # Extract labels from object names by removing the pattern
      lbl_o <- gsub(i_str, "", o)

      # Replace empty labels with "root"
      lbl_o <- gsub("^$", "root", lbl_o)

      # Create a list of objects with corresponding labels
      l_o <- lapply(o, get, envir = i_env)
      names(l_o) <- lbl_o

      # Remove original objects from the global environment
      rm(list = o, envir = i_env)

      return(l_o)
    }

    # Obtain the environment to be passed to the helper function
    kenv <- environment()


    # LOAD predefined values - - -

    # Load configuration from YAML file
    the$.enhub.config <- yaml::yaml.load_file(config_yaml_path)

    # Load default parameters from JSON file and remove empty elements
    k_defaults <- jsonlite::fromJSON(default_params_json_path)
    k_defaults <- k_defaults[lengths(k_defaults) != 0]

    # DEFINE main directories - - -

    # Get the user locations
    path_user_data <- tools::R_user_dir("enhub", which = "data") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    dir.create(path_user_data, recursive = TRUE, showWarnings = FALSE)

    # Get the location of EnergyPlus and EnHub directories
    path_EnergyPlus <- get_ep_location()
    path_EnHub <- get_enhub_location()
    path_EPW <- file.path(path_user_data, "data_sources/_weather")
    dir.create(path_EPW, recursive = TRUE, showWarnings = FALSE)

    # Create a list of configuration settings
    l_config <- list(
      eplus = list(version = get_eplus_version(path_EnergyPlus)),
      epw = list(
        source = "https://climate.onebuilding.org/default.html",
        url = "https://storage.googleapis.com/climate-onebuilding/"
      ),
      github = list(url = "github.com/Enhub-UK/Platform")
    )

    # Merge the global configuration with the loaded configuration
    the$.enhub.config <- modifyList(the$.enhub.config, l_config)

    # Update the YAML file with the merged configuration
    yaml::write_yaml(the$.enhub.config, config_yaml_path)

    # Define core paths for EnHub
    l_paths <-
      list(
        ep = path_EnergyPlus,
        enhub = path_EnHub,
        epw = path_EPW,
        user_data = path_user_data
      )

    # Define output paths and normalize them
    path_outs <- file.path(l_paths$enhub, "outcomes") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outsscenarios <- file.path(path_outs, "scenarios") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outssensitivity <- file.path(path_outs, "sensitivity_analysis") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outsselection <- file.path(path_outs, "selection") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outsreports <- file.path(path_outs, "reports") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outsprediction <- file.path(path_outs, "prediction") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outslhs <- file.path(path_outs, "LHS") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outsprofiles <- file.path(l_paths$user_data, "profiles") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outshousehold <- file.path(path_outsprofiles, "_household") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_outsvariable <- file.path(path_outsprofiles, "_variable") %>%
      normalizePath(winslash = "/", mustWork = FALSE)

    # Define simulation paths and normalize them
    path_sim <- file.path(l_paths$enhub, "outcomes/simulation") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_simA <- file.path(path_sim, "a_inputs") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_simB <- file.path(path_sim, "b_outputs") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_simC <- file.path(path_sim, "c_summaries") %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    path_simO <- file.path(path_sim, "Output") %>%
      normalizePath(winslash = "/", mustWork = FALSE)


    # GROUP directories and templates - - -

    # Define extra paths for EnHub
    l_paths_proj <-
      list(
        eval = group_objects("path_sim"),
        store = group_objects("path_outs")
      )

    # Merge the paths
    l_paths <- modifyList(l_paths, l_paths_proj)

    # Write on `the` environment
    the$.enhub.paths <- l_paths
    the$GLOBAL_PAR <- k_defaults

    # Clean up the environment by removing temporary variables and functions
    rm(
      path_EnergyPlus, path_EnHub, path_EPW, l_config,
      path_user_data, l_paths, l_paths_proj, k_defaults
    )


    # PREPARE folders - - -

    # Prepare EnHub output directories
    invisible(prepare_enhub_output_dir())

    # Return
    return(invisible(TRUE))
  }

  initialise_session <- function() {
    # Loads necessary data and environment for the analysis.
    #
    # This function sources various R scripts to load required data and functions.
    # It also performs environment checks and provides informative messages.
    #
    # @return None

    # Load essential modules

    # .. initialise enhub project
    initialise_config("config/external.yaml", "config/global.json")

    # .. referential calendar year
    the$l_days <- make_sim_period(the$GLOBAL_PAR$simulation_year)

    #.. make index table variable
    the$core_index <- d_core_index

    # .. global indentation used for formatting
    the$.indent <- paste(rep(" ", 4), collapse = "")

    # Check if Energy-Plus is available
    check_enhub_setup()

    # Clear the console
    cat("\014")

    # Print session information
    message(r_whoami())
    message(cat("EnHub path \u27A5 ", the$.enhub.paths$enhub))
    message(cat("  EP+ path \u27A5 ", the$.enhub.paths$ep))
  }

  # Define/read global parameters
  define_global_parameters("config/global.json", redefine = fresh_setup)

  # Initialise the project
  initialise_session()

  # Overwrite global environment
  the <<- the

  # Return nothing
  return(invisible(NULL))
}
