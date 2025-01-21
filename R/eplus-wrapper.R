
convert_idf_to_json <- function(filepath) {
  #' @title Convert IDF to EP-JSON
  #' @description A wrapper that converts an IDF file to JSON format using
  #' EnergyPlus the argument `--convert-only` for the binary `/energyplus`
  #' 
  #' For example, the following IDF object:
  #' ```fortran
  #' !-   =========== ALL OBJECTS IN CLASS: BOILER:HOTWATER ===========
  #' 
  #' Boiler:HotWater,        !- 
  #'    hw-standard-boiler, !- Name
  #'    NaturalGas,         !- Fuel Type
  #'    Autosize,           !- Nominal Capacity {W}
  #'    0.845,              !- Nominal Thermal Efficiency
  #'    LeavingBoiler,      !- Efficiency Curve Temperature Evaluation Variable
  #'    Boiler Efficiency,  !- Normalized Boiler Efficiency Curve Name
  #'    Autosize,           !- Design Water Flow Rate {m3/s}
  #'    0,                  !- Minimum Part Load Ratio
  #'    1,                  !- Maximum Part Load Ratio
  #'    1,                  !- Optimum Part Load Ratio
  #'    n-11005,            !- Boiler Water Inlet Node Name
  #'    n-11006,            !- Boiler Water Outlet Node Name
  #'    99,                 !- Water Outlet Upper Temperature Limit {C}
  #'    NotModulated,       !- Boiler Flow Mode
  #'    0,                  !- On Cycle Parasitic Electric Load {W}
  #'    1;                  !- Sizing Factor
  #' ```
  #' 
  #' is converted to the following JSON object:
  #' ```json
  #' {
  #'    "Boiler:HotWater": {
  #'        "hw-standard-boiler": {
  #'            "boiler_flow_mode": "NotModulated",
  #'            "boiler_water_inlet_node_name": "n-11005",
  #'            "boiler_water_outlet_node_name": "n-11006",
  #'            "design_water_flow_rate": "Autosize",
  #'            "efficiency_curve_temperature_evaluation_variable": "LeavingBoiler",
  #'            "fuel_type": "NaturalGas",
  #'            "maximum_part_load_ratio": 1,
  #'            "minimum_part_load_ratio": 0,
  #'            "nominal_capacity": "Autosize",
  #'            "nominal_thermal_efficiency": 0.845,
  #'            "normalized_boiler_efficiency_curve_name": "Boiler Efficiency",
  #'            "on_cycle_parasitic_electric_load": 0,
  #'            "optimum_part_load_ratio": 1,
  #'            "sizing_factor": 1,
  #'            "water_outlet_upper_temperature_limit": 99
  #'        }
  #'  }
  #' ```
  #'
  #' @param filepath The path to the input IDF file.
  #'
  #' @return None. The function performs file conversion and renaming operations.
  #'
  #' @family EnergyPlus evaluation


  # Normalize the input file path
  filepath <- normalizePath(filepath)

  # Define the path for the expected output directory
  p_exp <- gsub("model_sim.idf", "out_exp", filepath)

  # Define the path for the intermediate EP-JSON file
  f_epjson <- gsub(".idf", ".epJSON", filepath) %>%
    gsub("model_sim", "out_exp/model_sim", .) %>%
    normalizePath(mustWork = FALSE)

  # Define the path for the final JSON file
  f_json <- gsub(".idf", ".json", filepath) %>%
    normalizePath(mustWork = FALSE)

  # Determine the operating system
  myOS <- r_uname()

  # Convert and rename based on the operating system
  if (!myOS %in% c("win")) {
    # Define the path to the EnergyPlus executable for non-Windows systems
    exe <- file.path(the$.enhub.paths$ep, "/energyplus") %>%
      normalizePath(winslash = "/", mustWork = TRUE)

    # Construct the command to convert the IDF file to EP-JSON
    command <- paste(exe, "-x", "--convert-only", "-d", p_exp, filepath)

    # Execute the command and suppress warnings
    suppressWarnings(
      system(command,
             intern = FALSE,
             wait = TRUE,
             ignore.stderr = TRUE,
             ignore.stdout = TRUE
      )
    )

    # Move the EP-JSON file to the final JSON file path
    r_mv(f_epjson, f_json)

    # Remove the intermediate output directory
    r_rm(p_exp)
  } else {
    # Define the path to the EnergyPlus executable for Windows systems
    exe <- file.path(the$.enhub.paths$ep, "/energyplus.exe") %>%
      normalizePath(winslash = "/", mustWork = TRUE)

    # Construct the command to convert the IDF file to EP-JSON
    command <- paste(exe, "-x", "--convert-only", "-d", p_exp, filepath)

    # Execute the command and suppress warnings
    suppressWarnings(
      shell(command,
            intern = FALSE,
            wait = TRUE,
            ignore.stderr = TRUE,
            ignore.stdout = TRUE
      )
    )

    # Move the EP-JSON file to the final JSON file path
    r_mv(f_epjson, f_json)

    # Remove the intermediate output directory
    r_rm(p_exp)
  }
}

generate_idf <- function(idf, fixed_schedule = TRUE, resolution = "hourly", ...) {
  #' @title Generates an EnergyPlus IDF file
  #'
  #' @description This function populates an <span style="color:pink">
  #' EnergyPlus Input File (IDF)</span> based on the provided ID
  #' and various parameters. It includes sections for weather, location,
  #' schedules, dimensions, loads, heat loop, and Low and zero carbon (LZC)
  #' tech. Additionally---for enhanced data exchange, it creates a `JSON`
  #' summary card and converts the `IDF` to `JSON`.
  #'
  #' @param idf ID for the IDF file.
  #' @param fixed_schedule Logical indicating fixed schedules (TRUE) or
  #' dynamic (FALSE).
  #' @param resolution Character string specifying the simulation resolution.
  #' Default is 'hourly'. It supports different formats, eg: hourly, hrr, h,
  #' daily, ..., timestep (see [expand_output_resolution()]).
  #'
  #' @return A list containing:
  #'   - idf: The generated IDF file content as a character vector.
  #'   - epw: Defined weather file (list).
  #'   - sum: Generated summary card (JSON string).
  #'
  #' @family EnergyPlus evaluation
  #' @examples
  #' \dontrun{
  #'   check_external_dependencies(redefine = FALSE)
  #'   init_project(core_data = 'ehs', core_mode = 'def', fresh_setup = TRUE)
  #'   generate_idf(1, fixed_schedule = FALSE, resolution = 'h')
  #' }
  #' @export


  # Set-up -----------------------------------------------------------------

  # Define paths for various files
  path_hid <- file.path(the$.enhub.paths$eval$A, idf)
  file_hap <- file.path(path_hid, "typical_appliances.csv")
  file_idf <- file.path(path_hid, "model_sim.idf")
  file_sum <- file.path(path_hid, "summary_card.json")

  # Remove existing directory and create a new one
  r_rm(path_hid)
  r_mkdir(path_hid)

  # Some visual elements
  pass_icon <- "\u2714"
  error_icon <- "\u2716"

  # Global settings --------------------------------------------------------

  # Define global settings for fixed schedule and simulation resolution
  l_global <- list(fixed_schedule = fixed_schedule, simulation_resolution = resolution)
  the$GLOBAL_PAR <- modifyList(the$GLOBAL_PAR, l_global)

  # Define (active) weather parameters ------------------------------------

  # Define weather file source and locate weather files
  l_epw <- define_weather_file_source(idf) %>%
    modifyList(locate_weather_files(.$use_weather_file, .))

  # Populate idf sections --------------------------------------------------

  # Populate various sections of the IDF file
  idf_A <- populate_idf_section_A(idf)
  l_context <- idf_A$sum
  idf_A <- idf_A$idf

  idf_B <- populate_idf_section_B(idf, l_epw)
  l_location <- idf_B$sum
  idf_B <- idf_B$idf

  idf_E <- populate_idf_section_E(idf)
  l_dimensions <- idf_E$card
  idf_E <- idf_E$idf

  idf_C <- populate_idf_section_C(idf, l_dimensions)
  l_schedules <- idf_C$sum
  idf_C <- idf_C$idf

  idf_D <- populate_idf_section_D(idf)
  l_heat_loss <- idf_D$sum
  idf_D <- idf_D$idf

  idf_F <- populate_idf_section_F(idf, l_epw, l_schedules, l_dimensions)
  l_loads <- idf_F$sum
  idf_F <- idf_F$idf

  idf_G <- populate_idf_section_G(idf, l_dimensions, l_schedules, l_loads)
  l_heat_loop <- idf_G$sum
  idf_G <- idf_G$idf

  idf_I <- populate_idf_section_I(idf, l_epw, l_context, l_dimensions)
  l_lzc <- idf_I$sum
  idf_I <- idf_I$idf

  idf_J <- populate_idf_section_J(idf, l_schedules, l_dimensions, l_heat_loop, l_lzc)

  # Extract Summaries ------------------------------------------------------

  # Extract summaries from various sections
  l_sums <- list(
    context = l_context,
    location = l_location,
    schedules = l_schedules,
    envelope = l_dimensions,
    heat_loss = l_heat_loss,
    loads = l_loads,
    heat_loop = l_heat_loop,
    lzc = l_lzc
  )

  # Populate IDF header
  l_head <- populate_idf_header(idf, l_sums)
  idf_head <- l_head$idf

  # Combine sections -------------------------------------------------------

  # Combine all sections into a full IDF file
  idf_full <- c(idf_head, idf_A, idf_B, idf_C, idf_D, idf_E, idf_F, idf_G, idf_I, idf_J)
  idf_full <- gsub("\t", "", idf_full)
  idf_full <- idf_full[!is.na(idf_full)]

  # Generate Summary -------------------------------------------------------

  # Generate a JSON summary card
  l_summary <- jsonlite::fromJSON(l_head$card)
  l_summary <- jsonlite::toJSON(l_summary, pretty = FALSE, auto_unbox = TRUE)

  # Write Files (optional) -------------------------------------------------

  # Write the IDF file and summary card to disk
  writeLines(idf_full, file_idf)
  writeLines(l_summary, file_sum)

  # Convert the IDF file to JSON format
  convert_idf_to_json(file_idf)

  # Return results ---------------------------------------------------------
  
  # Display completion message
  message(cat(paste0(
    "\033[1;35m", pass_icon, "\033[0m ", "IDF generated and stored in ", 
    "\033[1;36m", path_hid, "\033[0m \n")))
  
  # Return the generated IDF file content, weather file, and summary card
  invisible(list(idf = idf_full, epw = l_epw, sum = l_summary))
}

run_energy_plus <- function(i, path_epw) {
  #' @title Runs an EnergyPlus simulation
  #'
  #' @description A wrapper to execute an EnergyPlus simulation based on the
  #' provided the index/location of the <span style="color:pink">EnergyPlus
  #' Input Files (IDF)</span> and <span style="color:pink">EnergyPlus Weather
  #' File (EPW)</span>. It handles file set-up, simulation execution, output
  #' processing, and clean-up.
  #'
  #' @param i Index or ID for the simulation.
  #' @param path_epw Path to the EnergyPlus Weather File
  #'
  #' @return Path to the simulation output directory.
  #'
  #' @family EnergyPlus evaluation
  #' @export


  # ..1 Set-up files ----------------------------------------------------------

  # Define paths and general parameters
  path_hid <- file.path(the$.enhub.paths$eval$A, i)
  file_idf <- file.path(path_hid, "model_sim.json")
  file_scd <- file.path(path_hid, "default_schedules.csv")

  # Get the operating system
  myOS <- r_uname()

  # Some visual elements
  pass_icon <- "\u2714"
  error_icon <- "\u2716"

  # Unzip auxiliary schedule file if zipped
  suppressWarnings(r_unzip(file_scd))

  # ..2 Simulation in EnergyPlus ----------------------------------------------

  # Normalize paths for IDF and EPW files
  ep_idf <- file_idf %>% normalizePath(winslash = "/", mustWork = FALSE)
  ep_epw <- path_epw %>% normalizePath(winslash = "/", mustWork = FALSE)

  # Start a chronometer to record processing time
  toggle_chronometer()

    # Define the simulation command based on the operating system
    if(!myOS %in% c("win")) {
      exename <- file.path(the$.enhub.paths$ep, "energyplus")
      command <- paste0(exename, " -w '", ep_epw, "' -d ", the$.enhub.paths$eval$O, " -r ", ep_idf)
      system(command, wait = FALSE, intern = TRUE, ignore.stderr = TRUE)
    }else{
      exename <- file.path(the$.enhub.paths$ep, "energyplus.exe")
      command <- paste0(exename, "-w '", ep_epw, "' -d ", the$.enhub.paths$eval$O, " -r ", ep_idf)
      shell(command, wait = FALSE, intern = TRUE, ignore.stderr = TRUE)
    }

    # Clear the console, and then display the command and completion message
    cat("\014")
    message(cat(paste0("command >: \033[1;35m", command, "\033[0m \n")))
    message(cat(paste0("\033[1;35m", pass_icon, "\033[0m ", "simulation completed! \n")))

  # Stop the chronometer
  toggle_chronometer()

  # ..3 Post-simulation -------------------------------------------------------

  # Define paths for simulation output
  path_from <- the$.enhub.paths$eval$O
  path_sim_in <- file.path(normalizePath(the$.enhub.paths$eval$A), i)
  path_sim_out <- file.path(normalizePath(the$.enhub.paths$eval$B), i)

  # Move EnergyPlus outputs to the output folder
  r_mkdir(path_sim_out)
  r_mv(file.path(path_from, "eplusout.csv"), file.path(path_sim_out, "results_eplus.csv"))
  r_mv(file.path(path_from, "eplustbl.csv"), file.path(path_sim_out, "results_summary.csv"))
  r_mv(file.path(path_from, "eplusout.wrl"), file.path(path_sim_out, "3d_model.wrl"))
  r_zip(file.path(path_sim_out, "results_eplus.csv"))
  r_zip(file.path(path_sim_out, "results_summary.csv"))
  r_zip(file.path(path_sim_in, "default_schedules.csv"))
  r_rm(path_from)

  # Remove unnecessary files based on the operating system
  if (myOS %in% c("win")) {
    suppressWarnings(file.remove(paste0(path_from, "/", list.files(path_from, pattern = "eplus"))))
    suppressWarnings(file.remove(paste0(path_from, "/", list.files(path_from, pattern = "sqlite"))))
    suppressWarnings(file.remove(paste0(path_from, "/", list.files(path_from, pattern = "fort"))))
  }

  # Return the path to the simulation output directory
  invisible(path_sim_out)
}

generate_idf_multiple <- function(s, profile_type = FALSE, res = "hourly") {
  #' @title Generate multiple IDF files
  #'
  #' @description This function generates multiple IDF files based on the
  #' provided simulation IDs. It checks for existing files and only generates
  #' the missing ones.
  #'
  #' @param s A vector of simulation IDs.
  #' @param profile_type Logical indicating whether to use fixed profiles (default: FALSE).
  #' @param res The resolution of the IDF file (default: "hourly").
  #' @return None. The function generates IDF files and schedules as needed.
  #' @family EnergyPlus evaluation


  # Record processing time
  toggle_chronometer()

  # Prepare storage folder
  s <- as.integer(s)
  path_idfs <- the$.enhub.paths$eval$A

  # Check for existing IDF and schedule files
  d_idf <- list_evaluation_files(path_idfs, "model_sim.idf")
  d_schedule <- list_evaluation_files(path_idfs, "default_schedules.csv*")

  # Identify valid and invalid files
  d_valid <- d_idf[d_idf %in% d_schedule]
  d_invalid <- d_idf[!(d_idf %in% d_schedule)]

  # Identify missing files
  d_absent <- s[!(s %in% d_valid)]
  d_absent <- c(d_absent, d_invalid)
  d_absent <- sort(d_absent, decreasing = FALSE)

  # Generate missing files
  if (is.integer(d_absent) & length(d_absent) != 0) {
    # Remove invalid files
    invisible(lapply(file.path(normalizePath(path_idfs), d_absent), r_rm))

    # Generate new IDF files
    invisible(lapply(d_absent, generate_idf,
                     fixed_schedule = profile_type,
                     resolution = res
    ))
  } else {
    message("It seems that all chosen IDFs have been created previously.")
  }

  # Return processing time
  toggle_chronometer()
}

simulate_available_idfs <- function(sid) {
  #' @title Simulate available IDFs
  #'
  #' @description This function simulates available IDFs based on a
  #' given simulation ID.
  #'
  #' @param sid Simulation ID.
  #' @return Invisibly returns the input `sid`.
  #' @family EnergyPlus evaluation

  # ..1 Set-up files ----------------------------------------------------------

  # Define paths for the IDF and summary card files
  path_hid <- file.path(the$.enhub.paths$eval$A, sid)
  file_idf <- file.path(path_hid, "model_sim.json")
  file_sum <- file.path(path_hid, "summary_card.json")

  # ..2 Read files ------------------------------------------------------------

  # Read the IDF and summary card files as JSON
  l_idf <- jsonlite::fromJSON(file_idf)
  l_sum <- jsonlite::fromJSON(file_sum)

  # Extract the building label and weather file path from the JSON data
  lbl_sid <- paste(names(l_idf$Building), ">", sid)
  lbl_epw <- l_sum$weather$epw

  # ..3 Call EnergyPlus wrapper ------------------------------------------------

  # Run the EnergyPlus simulation using the extracted weather file path
  d_simulation <- run_energy_plus(sid, lbl_epw)

  # Return the simulation ID invisibly
  invisible(sid)
}
