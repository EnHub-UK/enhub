
# data loading ----------------------------------------------------------------

read_idf_built <- function(idf, path_ref = the$.enhub.paths$eval$A) {
  #' @title Reads an IDF file from a stored batch
  #'
  #' @description This function reads an IDF file from a specified path within a stored batch.
  #' The function cleans the read data by removing tabs and certain characters.
  #'
  #' @param idf Character string specifying the IDF file name.
  #' @param path_ref Character string specifying the path to the stored batch.
  #'   Defaults to `the$.enhub.paths$eval$A`.
  #'
  #' @return A character vector containing the lines of the IDF file.

  txt_out <- file.path(path_ref, idf, "model_sim.idf") %>%
    normalizePath(winslash = "/", mustWork = FALSE)
  txt_out <- readLines(txt_out)
  txt_out <- gsub("\t", "", txt_out, useBytes = TRUE)
  txt_out <- gsub("\xb0", " ", txt_out, useBytes = TRUE)
  return(txt_out)
}

read_idf_outcomes <- function(idf, path_ref = the$.enhub.paths$eval$B) {
  #' @title Reads general properties from an IDF file
  #'
  #' @description Extracts general properties parsed to an IDF file from a specified path.
  #' The function cleans the read data by removing tabs and certain characters.
  #'
  #' @param idf Character string specifying the IDF file name.
  #' @param path_ref Character string specifying the path to the stored batch.
  #'   Defaults to `the$.enhub.paths$eval$B`.
  #'
  #' @return A character vector containing the lines of the extracted data.
  #' @family EnergyPlus evaluation
  #' @export

  fileTo <- file.path(path_ref, idf, "/results_summary.csv.gz") %>%
    normalizePath(winslash = "/", mustWork = FALSE)
  txt_out <- readLines(fileTo)
  txt_out <- gsub("\t", "", txt_out, useBytes = TRUE)
  txt_out <- gsub("\xb0", " ", txt_out, useBytes = TRUE)
  return(txt_out)
}

read_input_summary <- function(idf, path_ref = the$.enhub.paths$eval$A) {
  #' @title Reads input summary JSON file from a stored batch
  #'
  #' @description This function reads a JSON input summary file from a specified path
  #' within a stored batch. The function parses the JSON content and returns
  #' it as an R object.
  #'
  #' @param idf Character string specifying the IDF file name.
  #' @param path_ref Character string specifying the path to the stored batch.
  #'   Defaults to `the$.enhub.paths$eval$A`.
  #'
  #' @return An R object representing the parsed JSON content.
  #' @family EnergyPlus evaluation
  #' @export

  txt_out <- file.path(path_ref, idf, "summary_card.json") %>%
    normalizePath(winslash = "/", mustWork = FALSE)
  txt_out <- jsonlite::fromJSON(txt_out)

  return(txt_out)
}

read_input_file <- function(idf, path_ref = the$.enhub.paths$eval$A) {
  #' @title Reads JSON input file from a stored batch
  #'
  #' @description This function reads a JSON input file from a specified path within
  #' a stored batch. The function parses the JSON content and returns it
  #' as an R object.
  #'
  #' @param idf Character string specifying the IDF file name.
  #' @param path_ref Character string specifying the path to the stored batch.
  #'   Defaults to `the$.enhub.paths$eval$A`.
  #'
  #' @return An R object representing the parsed JSON content.
  #' @family EnergyPlus evaluation
  #' @export

  txt_out <- file.path(path_ref, idf, "model_sim.json") %>%
    normalizePath(winslash = "/", mustWork = FALSE)
  txt_out <- jsonlite::fromJSON(txt_out)

  return(txt_out)
}


# data parsing ----------------------------------------------------------------

parse_header <- function(txth) {
  #' @title Parse Header Elements from EP+ Summary File
  #'
  #' @description This function takes a header string from an EP+ summary file and
  #' parses it into a character vector of header elements.
  #'
  #' @description The function performs the following transformations on the input string:
  #' 1. Replaces leading ",," with "ctrl,item,".
  #' 2. Replaces "#" with "no_".
  #' 3. Replaces "\{" and "\/" with "__".
  #' 4. Removes "\}".
  #' 5. Replaces "-" and whitespace with "_".
  #' 6. Converts the string to lowercase.
  #' 7. Splits the string into a character vector using "," as the delimiter.
  #'
  #' @param txth A character string representing the header of an EP+ summary file.
  #'
  #' @return A character vector of parsed header elements.

  txth <- gsub("^\\,\\,", "ctrl,item,", txth)
  txth <- gsub("\\#", "no_", txth)
  txth <- gsub("\\{|\\/", "__", txth)
  txth <- gsub("\\}", "", txth)
  txth <- gsub("\\-|\\s", "_", txth)
  txth <- tolower(txth)
  txth <- unlist(strsplit(txth, ","))
  return(txth)
}

parse_body <- function(db, t_b, t_h) {
  #' @title Parse Body from EP+ Summary File
  #'
  #' @description Extracts and parses the body data from an EP+ summary file.
  #'
  #' @description This function processes the body section of an EP+ summary file, extracting
  #' individual rows, parsing them into columns based on the provided header, and
  #' combining them into a data frame.
  #'
  #' @param db A character vector representing the lines of an EP+ summary file.
  #' @param t_b A character string indicating the beginning of the body section.
  #' @param t_h A character vector representing the header for the body data.
  #'
  #' @return A data frame containing the parsed body data.

  line_match <- grep(t_b, db, useBytes = T)[1]

  if(db[line_match + 1] == ''){
    k_ini <- 1
    k_iter <- 0
  }else{
    k_ini <- 0
    k_iter <- 1
  }

  df_block <- list()
  j <- k_ini
  while (grepl("^\\,", db[line_match + 1 + k_ini + j])) {
    row_block <- db[line_match + 1 + k_ini + j]
    row_block <- gsub("\\,$", ",-", row_block)
    row_block <- unlist(strsplit(row_block, ","))
    row_block <- t(data.frame(row_block))
    colnames(row_block) <- t_h
    df_block[[j+k_iter]] <- row_block
    j <- j + 1
  }

  df_block <- plyr::ldply(df_block, data.frame)

  return(df_block)
}

replace_na_with_last <- function(x, a = !is.na(x)) {
  #' @title Replace NA Values with Previous Non-Missing Values
  #'
  #' @description Replaces NA values in a vector with the last non-missing value encountered.
  #'
  #' @param x A numeric vector containing potentially missing values (NA).
  #' @param a A logical vector of the same length as `x`, indicating which elements
  #' are not missing (TRUE) or missing (FALSE). Defaults to checking for missing
  #' values directly in `x`.
  #'
  #' @return A numeric vector with NA values replaced by the last non-missing value.

  x[which(a)[c(1, 1:sum(a))][cumsum(a) + 1]]
  # OR... use tidyr::fill(last)
}

process_output_resolution <- function(df) {
  #' @title Processes output resolution
  #'
  #' @description This function determines the data resolution based on the column name,
  #' processes the "Date.Time" column, and creates relevant date-related vectors.
  #'
  #' @param df A data frame containing output data with a column named "Date.Time".
  #' @return A list containing the following elements:
  #'   * `resolution`: A character string indicating the data resolution (hourly, daily, monthly, or timestep).
  #'   * `ddys`: A numeric vector indicating the indices of days with incomplete data.
  #'   * `days`: A numeric vector indicating the indices of full days.
  
  # Extract simulation year and column name
  yr <- the$GLOBAL_PAR$simulation_year
  lblDta <- colnames(df)[2]

  # Determine data resolution
  valRes <- ifelse(is.integer(grep("Monthly", lblDta)) &
                     grepl("Monthly", lblDta) == TRUE, "monthly",
            ifelse(is.integer(grep("Daily", lblDta)) &
                     grepl("Daily", lblDta) == TRUE, "daily",
            ifelse(is.integer(grep("Hourly", lblDta)) &
                     grepl("Hourly", lblDta) == TRUE, "hourly", "timestep")))

  # Process date and time column
  k_dates <- as.character(unlist(subset(df, select = Date.Time)))
  if (valRes == "monthly") k_dates <- paste0(k_dates, "-10_01:00:00")
  if (valRes == "daily") k_dates <- paste0(k_dates, "_01:00:00")
  k_dates <- gsub("^[[:space:]]", "", k_dates)
  k_dates <- gsub("\\s\\s", "_", k_dates)
  k_dates <- gsub("\\/", "-", k_dates)
  k_dates <- paste0(the$GLOBAL_PAR$simulation_year, "-", k_dates)
  k_dates <- lubridate::ymd_hms(k_dates)
  df$Date.Time <- k_dates

  # Calculate indices for incomplete and full days
  k_dates <- k_dates[2:length(k_dates)]
  dayIni <- max(grep(min(k_dates), k_dates)) + 1
  dayEnd <- max(grep(max(k_dates), k_dates)) + 1
  dayRes <- switch(valRes,
                   "hourly" = "60 min",
                   "daily" = "1 day",
                   "monthly" = "1 month",
                   "timestep" = "60 min"
  )

  timestep <- seq(c(ISOdate(yr, 1, 1, 0, 0)),
                  by = dayRes,
                  length.out = (dayEnd - dayIni + 1)
  )

  timestep_ddy <- df[c(1:dayIni - 1), "Date.Time"]
  timestep_ddy_days <- df[c(1, dayIni - 2), "Date.Time"]

  # Return output
  l_out <- list(valRes, c(1, dayIni - 1), c(dayIni, dayEnd))
  names(l_out) <- c("resolution", "ddys", "days")

  return(l_out)
}

dig_simulation_outcomes <- function(idm, path_ref = the$.enhub.paths$eval$B, outSet = "period", l_ref = s_ehs_2011_ext) {
  #' @title Digs simulation outcomes
  #'
  #' @description Reads EP tables including outputs and metrics.
  #'
  #' @param idm An identifier for the simulation model.
  #' @param path_ref The path to the simulation results directory (default: the$.enhub.paths$eval$B).
  #' @param outSet A character string specifying the output set ("period" or "ddy") (default: "period").
  #' @param l_ref A reference list for data processing (default: s_ehs_2011_ext).
  #' @return A data frame containing the processed simulation outputs.
  #'
  #' @details
  #' This function reads simulation output data, processes the data, and returns
  #' a cleaned and formatted data frame.

  fpath <- file.path(path_ref, idm, "results_eplus.csv.gz") %>%
    normalizePath(winslash = "/", mustWork = F)

  d_out <- read.csv(fpath, stringsAsFactors = FALSE, row.names = NULL)

  # verify resolution requested in output module, and get corresponding data
  l_resolution <- process_output_resolution(d_out)

  # Select type of evaluated days
  if (outSet == "ddy") {
    d_out <- d_out[l_resolution$ddys[1]:l_resolution$ddys[2], ]
  } else {
    d_out <- d_out[l_resolution$days[1]:l_resolution$days[2], ]
  }

  # Parse and format date-time column
  d_out <- d_out %>%
    dplyr::mutate(Date.Time = gsub("^ ", "", Date.Time)) %>%
    dplyr::mutate(Date.Time = gsub("\\s+", " ", Date.Time)) %>%
    dplyr::mutate(Date.Time = paste0(the$GLOBAL_PAR$simulation_year, "/", Date.Time)) %>%
    dplyr::mutate(Date.Time = lubridate::parse_date_time(Date.Time, "Y/m/d HMS")) %>%
    tibble::tibble()

  # Post-process electricity data
  if (any(grepl("ElectricityProduced", colnames(d_out)))) {
    d_out$ElectricityProduced.Facility..J..Hourly. <-
      d_out$ElectricityProduced.Facility..J..Hourly. * (-1)
  }

  # Adjust data for apartments
  varID <- as.character(unlist(l_ref$summarised$V001_HousingCode[idm]))
  dtaID <- obtain_archetype_parameters(varID, l_ref)
  if (dtaID$vFlt == "flat") {
    d_out[, grep(".Facility..", colnames(d_out))] <-
      d_out[, grep(".Facility..", colnames(d_out))] / dtaID$vFlr
  }

  return(d_out)
}

obtain_simulation_schedule <- function(i, pathIdf = the$.enhub.paths$eval$A, afr = 1, ato = 8760) {
  #' @title Reads a simulation schedule file
  #'
  #' @description This function reads a CSV file containing a simulation schedule for a given archetype.
  #' The file is expected to be located at `file.path(pathIdf, i, "/default_schedules.csv.gz")`.
  #'
  #' @param i Character string identifying the archetype.
  #' @param pathIdf Character string specifying the path to the directory containing schedule files.
  #' @param afr Numeric value (currently unused).
  #' @param ato Numeric value (currently unused).
  #'
  #' @return A tibble containing the simulation schedule with a `Date` column.

  # File path to the schedule file
  filepath <- file.path(pathIdf, i, "/default_schedules.csv.gz") %>%
    normalizePath(winslash = "/", mustWork = F)

  # Simulation year
  yr <- the$GLOBAL_PAR$simulation_year

  # Read the CSV file, process and return the data
  d <- read.csv(filepath, stringsAsFactors = FALSE) %>%
    dplyr::select(-X, -Hour) %>%
    tibble::tibble() %>%
    dplyr::mutate(Date = seq(c(ISOdate(yr, 1, 1, 1, 0)),
                      by = "60 min", length.out = 8760
    )) %>%
    dplyr::relocate(Date)

  return(d)
}

obtain_heating_status <- function(df){
  #' @title Determines heating status based on data frame content
  #'
  #' @description This function analyses a data frame containing various heating-related
  #' columns and infers the heating system status (on/off) and heating rate.
  #'
  #' @param df A data frame containing columns related to heating system operation.

  # Identify columns containing part-load information
  df_part_load <- df[,grep("(.(P|p)art.)", colnames(df))]

  # Calculate part-load efficiency (if data available)
  if(dim(df_part_load)[2]>0){
    df_part_load$PartLoad <- rowSums(df_part_load, na.rm = T)
  }else{
    df_part_load$PartLoad <- 0
  }
  df$Part.Load <- df_part_load$PartLoad

  # Identify columns containing heater performance data
  df_heat <-
    c("^date\\.|(^heat.*total.*|^coil.*|^back.*|^hw.*|^dhw.*|^hx.*).*rate.*")
  df_heat <- paste(df_heat, collapse="")
  df_heat <- df[, grep(df_heat, colnames(df), ignore.case = TRUE, perl = TRUE)]

  # Initialize heating status and rate columns
  df_heat$heat.on <- 0
  df_heat$heat.Rate..W <- abs(rowSums(
    df_heat[,grep("(heat|power)",colnames(df_heat), ignore.case = TRUE)]))

  # Infer heating status based on heat rate
  df_heat$heat.on <- ifelse(df_heat$heat.Rate..W > 0, TRUE, FALSE)

  # Handle special case: District Heating
  lbl_dist <- 'DistrictHeatingWater.Facility'
  if(any(grepl(paste0("^",lbl_dist), colnames(df), ignore.case = TRUE))){
    df_dist <- df[,grep(paste0("^",lbl_dist), colnames(df), ignore.case = TRUE)]
    colnames(df_dist) <- 'facility'

    # Override heating status and rate based on District Heating availability
    if(median(df$Part.Load)==0 & median(df_dist$facility)!=0){
      df_heat$heat.on <- ifelse(df_dist$facility!=0, T, F)
      df_heat$heat.Rate..W <- df_dist$facility / 3600000
    }
  }

  # Update original data frame with heating information
  df$Heating.Rate..W <- df_heat$heat.Rate..W
  df$Heating.Active.Status <- df_heat$heat.on

  return(df)
}

obtain_ddy_summary <- function(df, yr) {
  #' @title Extracts Day/Year Summary Information
  #'
  #' @description Extracts day/year summary information from a given data frame.
  #'
  #' @param df A data frame containing day/year summary data.
  #' @param yr The year of the data.
  #' @return A data frame containing day/year summary information.


  # (1) Extract environment summary data
  lbl_block <- "^,,Environment Name,Environment Type"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1]])
  df_sum <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl, -item) %>%
    dplyr::filter(environment_name != "SIMPERIOD") %>%
    dplyr::mutate(use_snow_values = ifelse(use_snow_values == "N/A", NA, ifelse(use_snow_values == "No", FALSE, TRUE))) %>%
    dplyr::mutate(use_rain_values = ifelse(use_rain_values == "N/A", NA, ifelse(use_rain_values == "No", FALSE, TRUE))) %>%
    dplyr::mutate(apply_weekend_holiday_rule = ifelse(apply_weekend_holiday_rule == "N/A", NA, ifelse(apply_weekend_holiday_rule == "No", FALSE, TRUE))) %>%
    dplyr::mutate(use_holidays = ifelse(use_holidays == "N/A", NA, ifelse(use_holidays == "No", FALSE, TRUE))) %>%
    dplyr::mutate(use_daylight_saving = ifelse(use_daylight_saving == "N/A", NA, ifelse(use_daylight_saving == "No", FALSE, TRUE))) %>%
    dplyr::mutate(source.start_dayofweek = ifelse(use_daylight_saving == "N/A", NA, use_daylight_saving)) %>%
    dplyr::mutate(
      start_dayofweek = as.factor(start_dayofweek),
      environment_type = as.factor(environment_type)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^duratio"), as.integer)) %>%
    dplyr::mutate(
      start_month = gsub("(\\d+)\\/(\\d+)", "\\1", start_date),
      start_day = gsub("(\\d+)\\/(\\d+)", "\\2", start_date),
      end_month = gsub("(\\d+)\\/(\\d+)", "\\1", end_date),
      end_day = gsub("(\\d+)\\/(\\d+)", "\\2", end_date)
    ) %>%
    dplyr::mutate(
      start_date = lubridate::dmy(paste(start_day, start_month, 2021, sep = "-")),
      end_date = lubridate::dmy(paste(end_day, end_month, 2021, sep = "-"))
    ) %>%
    dplyr::mutate(duration___n_hours = duration___no_days * 24) %>%
    dplyr::select(-start_month, -start_day, -end_month, -end_day) %>%
    tibble::tibble() %>%
    tibble::rownames_to_column("index")

  colnames(df_sum) <- gsub("\\_\\.y\\_\\_n\\.$|\\.$", "", colnames(df_sum))
  colnames(df_sum) <- gsub("\\_\\.|\\_{3}", "__", colnames(df_sum))

  # (2) Extract environment information data
  lbl_block <- "^,,Max Dry-Bulb Temp \\{C\\},Temp Range"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1]])
  df_info <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl, -item) %>%
    dplyr::mutate(rain = ifelse(rain == "No", FALSE, TRUE)) %>%
    dplyr::mutate(snow = ifelse(snow == "No", FALSE, TRUE)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^pressure|^wind_dir"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches(
      "^max_dry|___dc$|^hum_ind_val|^wind_spee|^clear"
    ), as.numeric)) %>%
    dplyr::mutate(
      temp_range_ind_type = as.factor(temp_range_ind_type),
      hum_ind_type = as.factor(hum_ind_type),
      hum_ind_units = as.factor(hum_ind_units)
    ) %>%
    tibble::tibble() %>%
    tibble::rownames_to_column("index")

  # Combine environment summary and information data
  df_sum <- df_sum %>%
    plyr::join(df_info, by = 'index') %>%
    tibble::tibble() %>%
    dplyr::mutate(index = as.integer(index))
  colnames(df_sum) <- gsub("\\_{3}", "__", colnames(df_sum))

  return(df_sum)
}

obtain_design_days <- function(df, df_index){
  #' @title Assigns labels to data frame rows based on time ranges
  #'
  #' @description This function assigns labels to rows of a data frame based on time ranges
  #' defined in another data frame.
  #'
  #' @param df A data frame to which labels will be assigned.
  #' @param df_index A data frame containing time ranges and corresponding labels.
  #' @return The input data frame with an added `ddy_item` column containing labels.

  # Helper function to find label for a given index
  find_label_in_range <- function(i, d){
    lbl <- d %>% dplyr::filter(i <= period__hours_end & i >= period__hours_ini) %>%
      dplyr::select(environment_name) %>% as.character()
    return(lbl)
  }

  # Prepare index data frame
  df_labels <- df_index %>%
    dplyr::mutate(period__hours_end = cumsum(duration__n_hours)) %>%
    dplyr::mutate(period__hours_ini = dplyr::lag(period__hours_end, default = 0) + 1) %>%
    dplyr::select(environment_name, period__hours_ini, period__hours_end)

  # Assign labels to data frame rows
  df <- df %>% tibble::tibble() %>%
    tibble::rownames_to_column('item') %>%
    dplyr::mutate(item = as.integer(item)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ddy_item = find_label_in_range(item, df_labels)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ddy_item %in% df_labels$environment_name) %>%
    dplyr::mutate(ddy_item = as.factor(ddy_item)) %>%
    dplyr::relocate(ddy_item) %>%
    dplyr::select(-item)

  return(df)
}

obtain_zone_summary <- function(df) {
  #' @title Extracts Zone Summary Information
  #'
  #' @description Extracts zone summary and information data from a given data frame.
  #'
  #' @param df A data frame containing zone data.
  #' @return A list containing two data frames:
  #'   - `summary`: Zone summary information.
  #'   - `key`: Zone key information.

  # (1) Extract zone summary data
  lbl_block <- "Zone Summary"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_sum <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::filter(!grepl("^L|^R|.*Total$", item)) %>%
    dplyr::mutate(
      conditioned_.y__n. =
        ifelse(conditioned_.y__n. == "No", FALSE, TRUE)
    ) %>%
    dplyr::mutate(
      part_of_total_floor_area_.y__n. =
        ifelse(part_of_total_floor_area_.y__n. == "No", FALSE, TRUE)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^multip"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches(
      "^area|^volu|^abov|^unde|^wind|^open|^ligh|^peop|^plug"
    ), as.numeric)) %>%
    tibble::tibble()
  colnames(df_sum) <- gsub("\\_\\.y\\_\\_n\\.$|\\.$", "", colnames(df_sum))
  colnames(df_sum) <- gsub("\\_\\.", "__", colnames(df_sum))

  # (2) Extract zone information data
  lbl_block <- "Zone Information"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_info <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl, -item) %>%
    dplyr::filter(!grepl("^L|^R|.*Total$", zone_name)) %>%
    dplyr::mutate(
      part_of_total_building_area =
        ifelse(part_of_total_building_area == "No", FALSE, TRUE)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^type|^number_of|multiplier$"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches(
      "^mini|^maxi|^nort|^orig|^cent|^ceil|^volu|\\_m$|m2$"
    ), as.numeric)) %>%
    tibble::tibble() %>%
    dplyr::select(zone_name, exterior_gross_wall_area___m2, ceiling_height___m) %>%
    dplyr::rename(item = zone_name)

  # (3) Combine zone summary and information data
  df_sum <- df_info %>% plyr::join(df_sum, by = 'item')
  colnames(df_sum) <- gsub("\\_{3}", "__", colnames(df_sum))

  # (4) Calculate zone key information
  df_key <- df_sum %>%
    dplyr::filter(!grepl("^T|^\\(L|^\\(R", item)) %>%
    dplyr::summarise(
      volume__m3 = sum(volume__m3),
      window_glass_area__m2 =
        sum(window_glass_area__m2),
      exterior_gross_wall_area__m2 =
        sum(exterior_gross_wall_area__m2),
      above_ground_gross_wall_area__m2 =
        sum(above_ground_gross_wall_area__m2),
      opening_area__m2 = sum(opening_area__m2),
      area__m2 = sum(area__m2)
    )
  colnames(df_key) <- gsub("\\_\\.", "__", colnames(df_key))
  colnames(df_key) <- gsub("\\_{3}", "__", colnames(df_key))

  # Return results as a list
  return(list(summary = df_sum, key = df_key))
}

obtain_surface_summary <- function(df){
  #' @title Extracts surface information from data frame
  #'
  #' @description This function extracts surface information from a data frame,
  #' cleans and reformats the data, and returns a summary.
  #'
  #' @param df A data frame containing surface information.
  #'
  #' @return A list containing a data frame with surface characteristics.

  # Identify the block containing surface information
  lbl_block <- '^HeatTransfer Surface'
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1]+2])

  # Data cleaning and transformation
  df_srfc <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl,-item) %>%
    dplyr::mutate(zone = gsub('^(.*)\\:(.*$)','\\1',surface_name)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^nominal|^area|^X|^tilt|^azimu|^reveal|^views"), as.numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^no_sides"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^zone|^extboundcondition|^extconvcoeffcalc|^intconvcoeffcalc|^sunexposure|^windexposure"), as.factor)) %>%
    dplyr::mutate(zone = gsub('^(.*)\\:(.*$)','\\1',zone)) %>%
    dplyr::relocate(zone,surface_class) %>%
    tibble::tibble()

  # Return the cleaned data
  return(list(surfaces = df_srfc))
}

obtain_material_summary <- function(df) {
  #' @title Extracts Material Summary Information
  #'
  #' @description Extracts construction, material, glazing, and opaque envelope information from a data frame.
  #'
  #' @param df A data frame containing building construction and material data.
  #' @return A list containing four data frames:
  #'   - `construction`: Construction information.
  #'   - `materials`: Material information.
  #'   - `glazing`: Glazing information.
  #'   - `opaque`: Opaque envelope information.

  # (1) Extract construction data
  lbl_block <- "Construction CTF"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
  df_cnstr <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl, -index) %>%
    dplyr::mutate(
      construction_name = tolower(construction_name),
      item = gsub("^\\s+", "", item)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^no_|^item"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^thermal|^outer|^inner|^time"), as.numeric)) %>%
    tibble::tibble()

  # (2) Extract material data
  lbl_block <- "Material CTF Summary"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
  df_mtral <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(
      material_name = tolower(material_name),
      item = gsub("^\\s+", "", item)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^item"), as.integer)) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("^therm|^thic|^dens|^condu|^speci"), as.numeric
    )) %>%
    tibble::tibble()

  # (3) Extract glazing data
  lbl_block <- "WindowConstruction"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
  df_glzng <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl, -index) %>%
    dplyr::mutate(
      construction_name = tolower(construction_name),
      item = gsub("^\\s+", "", item)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^item|^no"), as.integer)) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("^shgc|^solar|^visi|^condu"), as.numeric
    )) %>%
    tibble::tibble()

  # (4) Extract opaque envelope data
  lbl_block <- "Opaque Exterior"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
  df_opque <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(construction = tolower(construction)) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("^refl|^u_fac|^gros|^net_|^azim|^tilt"), as.numeric
    )) %>%
    tibble::tibble()

  # Return results as a list
  l <- list(
    construction = df_cnstr, materials = df_mtral,
    glazing = df_glzng, opaque = df_opque
  )

  return(l)
}

summarise_zone_dimensions <- function(do, dz) {
  #' @title Summarizes Zone Dimensions
  #'
  #' @description Summarizes zone dimensions and surface information for each thermal zone.
  #'
  #' @param do A data frame containing surface information.
  #' @param dz A data frame containing zone information.
  #' @return A list containing zone-specific information:
  #'   - `surfaces`: Surface data for the zone.
  #'   - `zone`: Zone-level data for the zone.

  # Helper function to group data by thermal zone
  group_by_thermal_zone <- function(i, dog, dzg) {
    dog <- dog %>% dplyr::filter(zone %in% i)
    dzg <- dzg %>% dplyr::filter(zone %in% i)
    l_z <- list(surfaces = dog, zone = dzg)
    return(l_z)
  }

  # Prepare surface data
  do <- do %>%
    dplyr::filter(!grepl("^L|^R", zone)) %>%
    dplyr::select(
      zone, surface_class, surface_name, base_surface,
      area_.net.___m2, area_.gross.___m2, azimuth___deg, tilt___deg,
      X.width___m, X.height___m, no_sides, sunexposure
    ) %>%
    dplyr::mutate(base_surface = ifelse(base_surface == "", NA, base_surface)) %>%
    dplyr::group_by(zone)

  # Prepare zone data
  dz <- dz$summary %>%
    dplyr::select(
      item, exterior_gross_wall_area__m2, opening_area__m2,
      ceiling_height__m, area__m2, volume__m3
    ) %>%
    dplyr::rename(zone = item)

  # Group data by thermal zone
  i_zon <- unique(dz$zone)
  l_zon <- lapply(i_zon, group_by_thermal_zone, do, dz)
  names(l_zon) <- i_zon

  # Filter core zones
  k_zone_core <- grep('\\([R|L]',names(l_zon), invert = TRUE)
  l_zon <- l_zon[k_zone_core]

  return(l_zon)
}

obtain_fuel_demand <- function(df, i_tfa) {
  #' @title Extracts Fuel Demand Information
  #'
  #' @description Extracts fuel demand data and calculates energy intensity from a data frame.
  #'
  #' @param df A data frame containing fuel demand data.
  #' @param i_tfa The total floor area.
  #' @return A list containing two data frames:
  #'   - `table`: Detailed fuel demand data.
  #'   - `summary`: Summary of fuel demand and energy intensity.

  # Define fuel demand column names
  lblDem <-
    c(
      "NaturalGas.Facility..J", "Electricity.Facility..J",
      "Coal.Facility..J", "FuelOilNo1.Facility..J",
      "OtherFuelNo1.Facility..J", "DistrictHeatingWater.Facility..J",
      "ElectricityProduced.Facility..J"
    )

  lbl_rgx <- paste(lblDem, collapse = "|")

  # Select fuel demand columns
  df_dem <- df %>% dplyr::select(Date.Time, dplyr::matches(lbl_rgx))

  # Separate date and energy columns
  varDat <- grep("Date", colnames(df_dem))
  varEne <- grep("..J", colnames(df_dem))

  # Clean column names
  colnames(df_dem) <- gsub(".Facility|.Hourly", "", colnames(df_dem))
  colnames(df_dem) <- gsub("\\.\\.$", "", colnames(df_dem))

  # Calculate total energy consumption and intensity
  df_dem_sum <- data.frame(energy__kWh = colSums(df_dem[, varEne])) %>%
    dplyr::mutate(energy__kWh = energy__kWh / 3600000) %>%
    dplyr::mutate(energy_intensity__kWh__m2 = energy__kWh / i_tfa)

  # Return results as a list
  return(list(table = df_dem %>% tibble::tibble(), summary = df_dem_sum))
}

obtain_fuel_expenditure <- function(df, df_ref = s_sap$CostAndCoefficientsSummary) {
  #' @title Calculates Fuel Expenditure
  #'
  #' @description Calculates fuel costs and standing charges based on energy use data and reference coefficients.
  #'
  #' @param df A data frame containing energy use data with columns for energy__kWh and fuel type.
  #' @param df_ref (optional) A data frame containing reference coefficients for fuel prices, standing charges, etc. Defaults to `s_sap$CostAndCoefficientsSummary`.
  #' @return A data frame with additional columns for fuel cost (fuel.cost) and standing charge (standing.charge).

  # Preprocess reference data frame
  colnames(df_ref) <- gsub("(.*)(__\\w_)", "\\1_", colnames(df_ref))
  colnames(df_ref)[grep("standing", colnames(df_ref))] <- "standing_charge_"

  # Initialize columns for charges
  df$standing.charge <- df$fuel.cost <- 0

  # Update standing charge based on fuel type
  for (i in c("gas", "electricity", "solid", "oil", "(community|district)")) {
    df$standing.charge[grep(i, rownames(df), ignore.case = T)] <-
      as.numeric(subset(df_ref, grepl(i, fuel), select = standing_charge_))
  }

  # Update fuel cost based on fuel type
  for (i in c("gas", "electricity", "solid", "oil", "(community|district)")) {
    df$fuel.cost[grep(i, rownames(df), ignore.case = T)] <-
      as.numeric(subset(df_ref, grepl(i, fuel), select = unit_price_kwh))
  }

  # Calculate total fuel cost
  df$fuel.cost <- df$energy__kWh * df$fuel.cost / 100

  # Handle missing standing charge (set to NA)
  df$standing.charge <-
    ifelse(df$standing.charge == 0, NA, df$standing.charge)

  # Handle missing values
  df[df == 0] <- NA
  df <- df %>% dplyr::filter(!is.na(energy__kWh))

  return(df)
}

obtain_energy_demand_profile <- function(data) {
  #' @title Converts energy data to kWh and returns JSON
  #'
  #' @description This function takes a data frame containing energy data in Joules, converts the
  #' energy values to kilowatt-hours (kWh), renames columns accordingly, and returns
  #' the data as a JSON string.
  #'
  #' @param data A data frame containing energy data.
  #' @return A JSON string representing the energy data in kWh.

  # Convert data to tibble and rename Date.Time column
  data <- data %>%
    tibble::tibble() %>%
    dplyr::rename(datetime = Date.Time)

  # Function to convert energy column from Joules to kWh
  convert_energy_column <- function(data, column_name, new_column_name) {
    if (any(grepl(column_name, colnames(data)))) {
      data <- data %>%
        dplyr::rename(!!new_column_name := !!dplyr::sym(column_name)) %>%
        dplyr::mutate(!!new_column_name := !!dplyr::sym(new_column_name) / 3600000)
    }
    data
  }

  # Convert energy columns
  data <- convert_energy_column(data, "Electricity..J", "electricity_kwh")
  data <- convert_energy_column(data, "NaturalGas..J", "gas_kwh")
  data <- convert_energy_column(data, "Coal..J", "solid_kwh")
  data <- convert_energy_column(data, "FuelOilNo1..J", "oil_kwh")
  data <- convert_energy_column(data, "OtherFuelNo1..J", "other_kwh")
  data <- convert_energy_column(data, "DistrictHeatingWater..J", "district_kwh")
  data <- convert_energy_column(data, "ElectricityProduced..J", "generated_electricity_kwh")

  # Convert data to JSON
  data_json <- data %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

  return(data_json)
}

obtain_carbon_and_primary <- function(df, df_ref = s_sap$CostAndCoefficientsSummary) {
  #' @title Calculates Carbon Emissions and Primary Energy Consumption
  #'
  #' @description Calculates carbon emissions and primary energy consumption based on energy use
  #' data and reference coefficients.
  #'
  #' @param df A data frame containing energy use data with columns for energy__kWh and fuel type.
  #' @param df_ref (optional) A data frame containing reference coefficients for CO2 emissions and primary energy factors. Defaults to `s_sap$CostAndCoefficientsSummary`.
  #' @return A data frame with additional columns for CO2 emissions (CO2.eq) and primary energy consumption (primary).

  # Initialize columns for coefficients and primary energy factors
  df$source.primary..kWh <- df$CO2.coeff <- 0

  # Update CO2 coefficients based on fuel type
  for (i in c("gas", "electricity", "solid", "oil", "(community|district)")) {
    df$CO2.coeff[grep(i, rownames(df), ignore.case = T)] <-
      as.numeric(subset(df_ref, grepl(i, fuel), select = emissions__kgco2_kwh))
  }

  # Update primary energy factors based on fuel type
  for (i in c("gas", "electricity", "solid", "oil", "(community|district)")) {
    df$source.primary..kWh[grep(i, rownames(df), ignore.case = T)] <-
      as.numeric(subset(df_ref, grepl(i, fuel), select = primary_energy_factor))
  }

  # Calculate CO2 equivalent emissions
  df$CO2.eq <- df$energy__kWh * df$CO2.coeff

  # Calculate primary energy consumption
  df$primary <- df$energy__kWh * df$source.primary..kWh

  # Handle missing values
  df[df == 0] <- NA
  df <- df %>% dplyr::filter(!is.na(energy__kWh))

  return(df)
}

obtain_end_use_summary <- function(df, i_tfa, df_sum) {
  #' @title Calculates End-Use Energy Summary
  #'
  #' @description Calculates energy consumption and intensity for various end-use categories
  #' from a data frame and summary data.
  #'
  #' @param df A data frame containing energy use data.
  #' @param i_tfa The total floor area.
  #' @param df_sum A data frame containing summary data.
  #' @return A data frame summarizing end-use energy consumption and intensity.

  # Helper to extract specific columns
  extract_outputs_per_name <- function(k, k_unit = "J", df_res = df_results) {
    # @title Extracts Outputs by Name
    #
    # @description Extracts the sum of columns matching a given pattern from a data frame.
    #
    # @param k The base name of the columns to extract.
    # @param k_unit The unit of the columns to extract (default: "J").
    # @param df_res The data frame containing the output data.
    # @return The sum of the selected columns.

    # Create the column name pattern
    (k <- paste0("^", k, "[[:alnum:]]*..", k_unit))

    # Select columns matching the pattern
    (lbl.sel <- colnames(df_res)[grep(k, colnames(df_res))])

    # Calculate the total sum of the selected columns
    tbl.sum <- sum(colSums(subset(df_res, select = c(lbl.sel)), na.rm = T))

    return(tbl.sum)
  }

  # Initialize empty list for end-use categories
  df_use <- list()

  # Extract energy consumption for each category
  df_use[["heating"]] <-
    extract_outputs_per_name("Heating.", df_res = df)
  df_use[["dhw"]] <-
    extract_outputs_per_name("WaterSystems.", df_res = df)
  df_use[["cooking"]] <-
    extract_outputs_per_name("cooking.*.", df_res = df)
  df_use[["appliances"]] <-
    extract_outputs_per_name("InteriorEquipment.", df_res = df) -
    df_use[["cooking"]]
  df_use[["lights"]] <-
    extract_outputs_per_name("InteriorLights.", df_res = df)

  # Convert list to data frame
  df_use <- as.data.frame(df_use)

  # Calculate other category and total consumption
  df_use$other <- sum(df_sum$energy__kWh, na.rm = T) * 3600000 - rowSums(df_use)

  # Calculate energy metrics and reshape data
  df_use <- data.frame(energy = t(df_use)) %>%
    dplyr::mutate(energy = energy / 3600000) %>%
    dplyr::mutate(energy.intensity = energy / i_tfa) %>%
    dplyr::mutate(contribution_x100 = energy / sum(energy, na.rm = T) * 100) %>%
    dplyr::rename(
      energy_demand__kWh = energy,
      energy_intensity__kWh_m2 = energy.intensity,
      contribution__x100 = contribution_x100
    )

  return(df_use)
}

obtain_end_use_fuel <- function(df) {
  #' @title Extracts End Use Fuel Data
  #'
  #' @description Extracts end use fuel data from a given data frame.
  #'
  #' @param df A data frame containing end use fuel data.
  #' @return A data frame containing end use fuel data with cleaned and processed columns.
  #'

  # Extract end use data
  lbl_block <- "End Uses By Subcategory"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])

  # Parse information
  df_endsub <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(item = ifelse(item == "", NA, item)) %>%
    dplyr::mutate(item = replace_na_with_last(item)) %>%
    dplyr::mutate(
      item = factor(tolower(item)),
      subcategory = factor(tolower(subcategory))
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^elec|^natu|^addi|^dist|^wate"), as.numeric)) %>%
    tibble::tibble() %>%
    dplyr::group_by(item)

  # Calculate summary statistics
  df_grp <- df_endsub %>%
    dplyr::select(-subcategory) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE))

  l <- list(end_use = df_endsub, summary = df_grp)

  return(df_endsub)
}

obtain_water_services <- function(df, dd) {
  #' @title Extracts Water Services Information
  #'
  #' @description Extracts water heating energy demand and water volume from data frames.
  #'
  #' @param df A data frame containing water heating energy data.
  #' @param dd A data frame containing water volume data.
  #' @return A list containing:
  #'   - `dhw_services`: A data frame with water heating services and their contribution.
  #'   - `dwh_demand__kWh`: Total water heating energy demand in kWh.
  #'   - `water_volume`: Total water volume in m3.

  # Extract water heating energy data
  tgx <- ".Water.Use.Equipment.Heating.Energy..J"
  df_dem <- df %>%
    tibble::tibble() %>%
    dplyr::select(contains(tgx))
  df_dem <- data.frame(values = colSums(df_dem, na.rm = TRUE) / 3600000)
  df_dem$service <- rownames(df_dem)
  df_dem <- df_dem %>%
    tibble::tibble() %>%
    dplyr::mutate(service = tolower(gsub(tgx, "", service))) %>%
    dplyr::mutate(contribution = 100 * values / sum(df_dem$values))

  # Calculate total water heating demand
  df_dem_sum <- sum(df_dem$values)

  # Extract total water volume
  i_water_vol__m3 <- sum(dd$water_.m3.)

  # Return results as a list
  l_dem <- list(
    dhw_services = df_dem,
    dwh_demand__kWh = df_dem_sum,
    water_volume = i_water_vol__m3
  )

  return(l_dem)
}

obtain_annual_temperature <- function(df) {
  #' @title Calculates annual temperature summaries
  #'
  #' @description This function calculates annual temperature summaries based on the provided data frame.
  #'
  #' @param df A data frame containing temperature data.
  #' @return A list of data frames containing three elements:
  #'  * `monthly`: Monthly mean temperature for each month.
  #'  * `hourly`: Hourly mean temperature for each month and zone.
  #'  * `on_heat`: Mean temperature for each month and zone, grouped by heating status.

  # Obtain available months
  lbl_mnth <- unique(as.character(the$l_days$period$Months))

  # Define variable of interest
  lbl_vari <- "Zone.Mean.Air.Temperature..C"


  # (1) Summarise monthly indoor temperature
  df_temp <- df %>%
    dplyr::select(Date.Time, dplyr::matches(lbl_vari)) %>%
    dplyr::mutate(month = lubridate::month(Date.Time, label = TRUE, abbr = TRUE)) %>%
    dplyr::select(-Date.Time)
  
  df_temp <- df_temp %>%
    tidyr::pivot_longer(cols = -month, names_to = "variable", values_to = "value") %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      mean_indoor_temperature__C = round(mean(value, na.rm = TRUE), 2)
    )


  # (2) Summarise hourly and monthly indoor temperature per thermal zone
  d_sum <- df %>%
    dplyr::select(Date.Time, Heating.Active.Status, dplyr::matches(lbl_vari)) %>%
    dplyr::mutate(
      month = lubridate::month(Date.Time, label = TRUE, abbr = TRUE),
      hour = lubridate::hour(Date.Time)
    ) %>%
    dplyr::select(-Date.Time)

  d_melted <- d_sum %>%
    tidyr::pivot_longer(
      cols = contains("Mean.Air.Temperature"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(variable = gsub(lbl_vari, "", variable)) %>%
    dplyr::group_by(month, hour, variable)

  d_sum_keys <- d_melted %>%
    dplyr::group_keys() %>%
    dplyr::mutate(values = d_melted %>% dplyr::group_map(~ mean(.x$value)) %>% unlist())
  l_sum <- d_sum_keys %>%
    tidyr::pivot_wider(names_from = variable, values_from = values) %>%
    dplyr::rename_with(~ gsub("\\.+Hourly\\.$", "", .)) %>%
    dplyr::rename_with(~ gsub("^X\\.", "zone:", .)) %>%
    dplyr::rename_with(~ gsub("\\.", "-", .)) %>%
    dplyr::group_by(month) %>%
    dplyr::group_split(.keep = FALSE)
  names(l_sum) <- lbl_mnth

  # (3) Summarise period while heating is enabled
  d_melted <- d_sum %>%
    tidyr::pivot_longer(
      cols = contains("Mean.Air.Temperature"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(variable = gsub(lbl_vari, "", variable)) %>%
    dplyr::group_by(month, Heating.Active.Status, variable)

  d_sum_keys <- d_melted %>%
    dplyr::group_keys() %>%
    dplyr::mutate(values = d_melted %>% dplyr::group_map(~ mean(.x$value)) %>% unlist())

  l_hon <- d_sum_keys %>%
    tidyr::pivot_wider(names_from = variable, values_from = values) %>%
    dplyr::rename_with(~ gsub("\\.+Hourly\\.$", "", .)) %>%
    dplyr::rename_with(~ gsub("^X\\.", "zone:", .)) %>%
    dplyr::rename_with(~ gsub("\\.", "-", .)) %>%
    dplyr::group_by(month) %>%
    dplyr::group_split(.keep = FALSE)

  names(l_hon) <- lbl_mnth

  # Return list of summaries
  return(list(monthly = df_temp, hourly = l_sum, on_heat = l_hon))
}

extract_zone_temperature <- function(d){
  #' @title Extracts zone and environment temperatures from data
  #'
  #' @description This function extracts zone operative temperature, mean air temperature,
  #' radiant temperature, and outdoor and ground temperatures from a data frame.
  #'
  #' @param d A data frame containing temperature data.
  #' @return A data frame with extracted temperature information.

  # Estimate dataset resolution
  i_timesteps <- dim(d)[1]
  i_timedays <- i_timesteps / 24

  # Extract Zone Operative Temperature [C]
  lbl <- "Zone.Operative.Temperature"
  d_opt <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colMeans()
  d_opt <- as.list(d_opt) %>% tibble::as_tibble()
  colnames(d_opt) <- gsub("\\.TimeStep\\.|\\.C\\.", "", colnames(d_opt)) %>%
    gsub(lbl, "", .)
  d_opt <- data.frame(label = names(d_opt), value = as.numeric(t(d_opt))) %>%
    dplyr::mutate(zone = gsub("^([[:alnum:]]*)\\.(.*$)", "\\1", label)) %>%
    dplyr::mutate(item = gsub("^([[:alnum:]]*)\\.(.*$)", "\\2", label)) %>%
    dplyr::mutate(class = "operative-temperature") %>%
    dplyr::relocate(zone, item, class)

  # Extract Zone Mean Air Temperature [C]
  lbl <- "Zone.Mean.Air.Temperature"
  d_mat <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colMeans()
  d_mat <- as.list(d_mat) %>% tibble::as_tibble()
  colnames(d_mat) <- gsub("\\.TimeStep\\.|\\.C\\.", "", colnames(d_mat)) %>%
    gsub(lbl, "", .)
  d_mat <- data.frame(label = names(d_mat), value = as.numeric(t(d_mat))) %>%
    dplyr::mutate(zone = gsub("^([[:alnum:]]*)\\.(.*$)", "\\1", label)) %>%
    dplyr::mutate(item = gsub("^([[:alnum:]]*)\\.(.*$)", "\\2", label)) %>%
    dplyr::mutate(class = "mean-air-temperature") %>%
    dplyr::relocate(zone, item, class)

  # Extract Zone Mean Radiant Temperature [C]
  lbl <- "Zone.Mean.Radiant.Temperature"
  d_rdt <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colMeans()
  d_rdt <- as.list(d_rdt) %>% tibble::as_tibble()
  colnames(d_rdt) <- gsub("\\.TimeStep\\.|\\.C\\.", "", colnames(d_rdt)) %>%
    gsub(lbl, "", .)
  d_rdt <- data.frame(label = names(d_rdt), value = as.numeric(t(d_rdt))) %>%
    dplyr::mutate(zone = gsub("^([[:alnum:]]*)\\.(.*$)", "\\1", label)) %>%
    dplyr::mutate(item = gsub("^([[:alnum:]]*)\\.(.*$)", "\\2", label)) %>%
    dplyr::mutate(class = "radiant-temperature") %>%
    dplyr::relocate(zone, item, class)

  # Extract Outdoor Db Temperature [C]
  lbl <- "^Environment.Site.Outdoor.Air.Drybulb.Temperature"
  d_out <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colMeans()
  d_out <- as.list(d_out) %>% tibble::as_tibble()
  colnames(d_out) <- gsub("\\.TimeStep\\.|\\.C\\.", "", colnames(d_out)) %>%
    gsub(lbl, "", .)
  d_out <- data.frame(label = names(d_out), value = as.numeric(t(d_out))) %>%
    dplyr::mutate(zone = "OUTDOORS") %>%
    dplyr::mutate(item = "outside") %>%
    dplyr::mutate(class = "outdoor-air-temperature") %>%
    dplyr::relocate(zone, item, class)

  # Extract Zone Mean Radiant Temperature [C]
  lbl <- "^Environment.Site.Ground.Temperature"
  d_gnd <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colMeans()
  d_gnd <- as.list(d_gnd) %>% tibble::as_tibble()
  colnames(d_gnd) <- gsub("\\.TimeStep\\.|\\.C\\.", "", colnames(d_gnd)) %>%
    gsub(lbl, "", .)
  d_gnd <- data.frame(label = names(d_gnd), value = as.numeric(t(d_gnd))) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::mutate(zone = "GROUND") %>%
    dplyr::mutate(item = "outside") %>%
    dplyr::mutate(class = "ground-temperature") %>%
    dplyr::relocate(zone, item, class)

  # Collate results
  l_res <- list(operative=d_opt, mean=d_mat,
                radiant=d_rdt, out=d_out, ground=d_gnd)

  d_res <- plyr::ldply(l_res, data.frame) %>% tibble::tibble() %>% dplyr::select(-`.id`)

  return(d_res)
}

obtain_zone_temperature_ddy <- function(dp){
  #' @title Extracts Zone Temperature Data by Day/Year
  #'
  #' @description Extracts zone temperature data for each day/year item.
  #'
  #' @param dp A data frame containing zone temperature data.
  #' @return A list of data frames, each containing zone temperature data for a specific day/year item.

  # Group data by day/year item
  l_dp <- dp %>% dplyr::group_by(ddy_item)

  # Extract unique day/year items
  lbl_ldp <- l_dp %>% dplyr::group_keys() %>% unlist() %>% as.character()

  # Split data by day/year item
  l_dp <- l_dp %>% dplyr::group_split()

  # Apply extraction function to each group
  l_res <- lapply(l_dp, extract_zone_temperature)
  names(l_res) <- lbl_ldp

  return(l_res)
}

obtain_coldest_state <- function(d) {
  #' @title Obtains the Coldest State from Data
  #'
  #' @description This function identifies the row with the lowest Outdoor.Air.Drybulb.Temperature..C..Hourly. value
  #' and returns it as a data frame with cleaned column names.
  #'
  #' @param d A data frame containing weather data.
  #' @return A data frame containing the coldest state with cleaned column names.

  # Find the row with the lowest temperature
  dc <- d %>%
    dplyr::arrange(Outdoor.Air.Drybulb.Temperature..C..Hourly.) %>%
    dplyr::slice(1)

  # Clean column names
  colnames(dc) <- tolower(gsub("\\.", "_", colnames(dc)))
  colnames(dc) <- gsub("\\_(hourly|weekly|monthly|timestep)\\_", "", colnames(dc))
  colnames(dc) <- gsub("\\_+$", "", colnames(dc))

  return(dc)
}

obtain_hdd_profile <- function(df, k) {
  #' @title Calculates Heating Degree Days (HDD) Profile
  #'
  #' @description This function calculates the HDD profile for a given dataset and base temperature (k).
  #'
  #' @param df A data frame containing weather data with columns `Date.Time` and
  #'   `Outdoor.Air.Drybulb.Temperature..C..Hourly.`.
  #' @param k The base temperature for calculating HDD.
  #'
  #' @return A list containing:
  #'   - `table`: A data frame with monthly mean temperature and total HDD.
  #'   - `hdd`: A list with `threshold` (base temperature) and `total` HDD.

  # Calculate daily HDD
  df <- df %>%
    dplyr::mutate(days = Date.Time, months = lubridate::month(Date.Time)) %>%
    dplyr::mutate(hdd = ifelse(Outdoor.Air.Drybulb.Temperature..C..Hourly. < k,
                        k - Outdoor.Air.Drybulb.Temperature..C..Hourly., 0
    ))

  # Calculate total daily HDD
  dta.sum.hdd <- df %>%
    dplyr::group_by(days) %>%
    dplyr::summarise(hdd = sum(hdd, na.rm = TRUE) / 24) %>%
    dplyr::mutate(months = lubridate::month(days, label = TRUE, abbr = TRUE)) %>%
    tibble::tibble()

  # Calculate total monthly HDD
  dta.hdd.month <- dta.sum.hdd %>%
    dplyr::group_by(months) %>%
    dplyr::summarise(sum_hdd = sum(hdd, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Calculate mean daily temperature
  dta.mean.temp <- df %>%
    dplyr::group_by(days) %>%
    dplyr::summarise(mean_temp = mean(Outdoor.Air.Drybulb.Temperature..C..Hourly., na.rm = TRUE)) %>%
    dplyr::mutate(months = lubridate::month(days, label = T, abbr = T)) %>%
    tibble::tibble()

  # Combine mean temperature and total HDD data
  dta.mean.temp <-
    plyr::join(dta.mean.temp, dta.hdd.month, by = "months") %>%
    tibble::tibble()

  # Calculate total HDD
  total_hdd <- sum(dta.hdd.month$sum_hdd)

  # Print total HDD for information
  # message(total_hdd)

  # Return results
  return(list(table = dta.mean.temp,
              hdd = list(threshold = k, total = total_hdd)))
}

obtain_site_air_profile <- function(d) {
  #' @title Extracts Site Air Temperature Profile
  #'
  #' @description This function extracts outdoor dry bulb temperature from a given dataset,
  #' renames columns, and converts the data to JSON format.
  #'
  #' @param d A data frame containing weather data with columns `Date.Time` and
  #'   `Outdoor.Air.Drybulb.Temperature..C..Hourly.`.
  #' @return A JSON string representing the extracted temperature data.

  # Select and rename columns
  d <- d %>%
    tibble::tibble() %>%
    dplyr::select(Date.Time, Outdoor.Air.Drybulb.Temperature..C..Hourly.) %>%
    dplyr::rename(
      datetime = Date.Time,
      value__C = Outdoor.Air.Drybulb.Temperature..C..Hourly.
    )

  # Convert to JSON
  ds <- d %>% jsonlite::toJSON(pretty = T, auto_unbox = T)

  return(ds)
}

obtain_outdoor_profile <- function(df, hdd_base){
  #' @title Obtain outdoor profile from data frame
  #'
  #' @description This function extracts relevant outdoor data from a provided data frame
  #' and calculates Heating Degree Days (HDD). It returns a list containing:
  #'
  #'  * `hdd`: A list with details about HDD calculations, including:
  #'     * `table`: A data frame summarizing HDD by period.
  #'     * `threshold`: The base temperature used for HDD calculation.
  #'     * `total`: Total HDD for the period.
  #'     * `coldest`: Information about the coldest state (potentially retrieved
  #'                   from a separate function).
  #'  * `site`: A data frame containing site-specific outdoor data (air
  #'              temperature, wind, rain).
  #'
  #' @param df A data frame containing outdoor environmental data.
  #' @param hdd_base The base temperature used for HDD calculation.
  #' @return A list containing HDD details and site-specific outdoor data.

  # Subset data for relevant columns
  df_sum <- df %>%
    dplyr::select(Date.Time,
           dplyr::starts_with('Environment.Site.Outdoor.Air'),
           dplyr::starts_with('Environment.Site.Wind'),
           dplyr::starts_with('Environment.Site.Rain')) %>% tibble::tibble()

  # Clean column names
  colnames(df_sum) <- gsub('Environment\\.Site\\.','',colnames(df_sum))

  # Obtain data from separate outdoor-conditions functions
  l_cold <- obtain_coldest_state(df_sum)
  df_hdd <- obtain_hdd_profile(df_sum, hdd_base)
  l_site <- obtain_site_air_profile(df_sum)

  # Summarize HDD profile
  l_sum <- list(
    hdd=list(
      table=df_hdd$table,
      threshold=df_hdd$hdd$threshold,
      total=df_hdd$hdd$total,
      coldest=l_cold),
    site=l_site)

  return(l_sum)
}

obtain_zone_temperatures <- function(df, var_t, var_heat=FALSE){
  #' @title Extracts zone temperatures from a data frame
  #'
  #' @description This function extracts zone temperatures from a data frame, optionally including heating status.
  #' It cleans and renames the data for subsequent analysis.
  #'
  #' @param df A data frame containing zone temperature data.
  #' @param var_t The column name prefix for temperature variables.
  #' @param var_heat Logical indicating whether to include heating status. Defaults to FALSE.
  #'
  #' @return A data frame with columns `date_time`, `zone`, and `value`.
  #'
  #' @examples
  #' \dontrun{
  #' obtain_zone_temperatures(df = my_data, var_t = "Zone.Operative.Temperature")
  #' }

  # Select relevant columns based on var_heat
  if (!var_heat) {
    d_res <- df %>%
      dplyr::select(Date.Time, dplyr::matches(var_t)) %>%
      tidyr::pivot_longer(cols = -Date.Time, names_to = "variable", values_to = "value")
  } else {
    var_h = "Heating.Active.Status"
    d_res <- df %>%
      dplyr::select(Date.Time, dplyr::matches(var_t), dplyr::matches(var_h)) %>%
      tidyr::pivot_longer(cols = -c(Date.Time, Heating.Active.Status), names_to = "variable", values_to = "value") %>%
      dplyr::rename(heating = Heating.Active.Status)
  }

  # Clean and rename variables
  d_res <- d_res %>%
    dplyr::mutate(variable = gsub(var_t, "", variable)) %>%
    dplyr::mutate(variable = gsub("\\.\\..*$", ")", variable)) %>%
    dplyr::mutate(variable = gsub("^X\\.", "(", variable)) %>%
    dplyr::mutate(variable = gsub("\\.", "-", variable)) %>%
    dplyr::rename(date_time = Date.Time,
                  zone = variable) %>%
    tibble::tibble()

  return(d_res)
}

obtain_indoor_profile <- function(df, hdd_base, dfz){
  #' @title Calculates indoor temperature profiles
  #'
  #' @description This function calculates indoor temperature profiles based on provided data.
  #' It categorizes spaces into 'main' and 'secondary' based on temperature levels.
  #' For each space, it calculates minimum, mean, and other temperature metrics.
  #'
  #' @param df A data frame containing indoor temperature and heating status data.
  #' @param hdd_base A value used for HDD calculation (not used in this function).
  #' @param dfz A list containing thermal zone information for iteration
  #' @return A list containing temperature profiles for 'room_001' and 'room_002'.

  # Helper to iterate over thermal zones
  extract_thermal_zone_temperatures <- function(l_tz, d_zn, d_rf){
    # @title Extracts thermal zone temperatures and related information
    #
    # @description This function extracts thermal zone temperatures and calculates
    # associated metrics for specified zones.
    #
    # @param l_tz A character vector of zone names.
    # @param d_zn A data frame containing zone data, including 'zone', 'heating', and 'value' columns.
    # @param d_rf A data frame containing room function data, including 'zone' and 'room_function' columns.
    #
    # @return A list containing the following elements:
    #   * `usage`: The room function of the zone.
    #   * `lowest_temperature_allday__C`: The minimum temperature for the entire day.
    #   * `lowest_temperature_heated__C`: The minimum temperature during heated periods.
    #   * `mean_temperature__C`: A data frame with the mean temperature for each date-time.

    # Filter data for specified zones
    df <- d_zn %>% dplyr::filter(zone %in% l_tz)
    df_heat <- df %>% dplyr::filter(heating == TRUE)

    # Extract room function
    lbl_function <- d_rf %>% dplyr::filter(zone %in% l_tz) %>% tail(1) %>%
      dplyr::select(room_function) %>% unlist() %>% as.character() %>% tolower()

    # Calculate minimum temperatures
    min_all <- min(df$value)
    min_heat <- min(df_heat$value)

    # Calculate mean temperature per date-time
    df_profile <- df %>% dplyr::group_by(date_time) %>%
      dplyr::summarise(value = mean(value))

    # Return results as a list
    return(list(usage = lbl_function,
                lowest_temperature_allday__C = min_all,
                lowest_temperature_heated__C = min_heat,
                mean_temperature__C = df_profile))
  }

  # Define variables of interest
  lbl <- 'Zone.Mean.Air.Temperature..C'

  df_T_ind <- obtain_zone_temperatures(df, lbl, TRUE)

  # Iterate over thermal zone and rename
  l_sum <- lapply(dfz$spaces, extract_thermal_zone_temperatures,
                  df_T_ind, dfz$summary)
  names(l_sum) <- paste0('room_', sprintf("%03d", seq(1:length(l_sum))))

  return(l_sum)
}

obtain_energy_system_performance <- function(df, l_sum){
  #' @title Extracts energy system performance data based on system code
  #'
  #' @description This function extracts relevant data for energy system performance
  #' based on the system code provided in the summary data.
  #'
  #' @param df A data frame containing energy system data.
  #' @param l_sum A list containing design day information and potentially
  #' the system code (may need adjustment based on your data structure).
  #'
  #' @return A list containing extracted performance data or '-' if the system code is not recognized.

  # Extract system code
  heatid <- l_sum$house$energy_system$code

  # Extract data based on system code patterns

  #... heat pump systems
  if(grepl('^MSH15|^MSH14',heatid)){

    colnames(df)
    df_fld <- df %>%
      dplyr::select(Date.Time,
             dplyr::starts_with('DHWLS.PUMP.Pump.Fluid.Heat.Gain.Rate')) %>% tibble::tibble()
    colnames(df_fld) <- c('date_time','pump_heat_gain__W')
    df_fld <- df_fld %>% tibble::tibble()

    df_hrt <- df %>%
      dplyr::select(Date.Time, dplyr::starts_with(
        'DOMESTIC.HOT.WATER.LOOP.Plant.Supply.Side.Heating.Demand.Rate')) %>%
      tibble::tibble()
    colnames(df_hrt) <- c('date_time','heating_demand_rate__W')
    df_hrt <- df_hrt %>% tibble::tibble()

    l_tmp <- list(pump_heat_gain = df_fld, heating_demand_rate = df_hrt)

  #... centrally heated with gas, oil, coal or electric boiler
  }else if(grepl('^MSH[1-6]D',heatid)){

    df_htr <- df %>%
      dplyr::select(Date.Time, contains('Boiler.Heating.Rate..W'),
             contains('Heater.Heating.Rate..W')) %>% tibble::tibble()
    colnames(df_htr) <- gsub('.Heating.Rate..W','__W',colnames(df_htr))
    colnames(df_htr) <- gsub('\\.','_',colnames(df_htr))

    df_plr <- df %>%
      dplyr::select(Date.Time, contains('Part.Load.Ratio')) %>% tibble::tibble()
    colnames(df_plr) <- gsub('.Part.Load.Ratio..','',colnames(df_plr))
    colnames(df_plr) <- gsub('\\.','_',colnames(df_plr))

    l_tmp <- list(heating_rate = df_htr, part_load_ratio = df_plr)

  #... room and electric storage
  }else if(grepl('^MSH[7-8]D',heatid)){

    df_htr <- df %>%
      dplyr::select(Date.Time, contains('Heater.Heating.Rate..W')) %>% tibble::tibble()
    colnames(df_htr) <- gsub('.Heating.Rate..W','__W',colnames(df_htr))
    colnames(df_htr) <- gsub('\\.','_',colnames(df_htr))

    df_plr <- df %>%
      dplyr::select(Date.Time, contains('Part.Load.Ratio')) %>% tibble::tibble()
    colnames(df_plr) <- gsub('.Part.Load.Ratio..','',colnames(df_plr))
    colnames(df_plr) <- gsub('\\.','_',colnames(df_plr))

    l_tmp <- list(heating_rate = df_htr, part_load_ratio = df_plr)

  #... warm air systems
  }else if(grepl('^MSH9D|^MSH10D',heatid)){

    df_htr <- df %>%
      dplyr::select(Date.Time, contains('Relief.Air.Heat.Transfer.Rate..W')) %>% tibble::tibble()
    colnames(df_htr) <- gsub('.Heat.Transfer.Rate..W','__W',colnames(df_htr))
    colnames(df_htr) <- gsub('\\.','_',colnames(df_htr))

    df_plr <- df %>%
      dplyr::select(Date.Time, contains('Coil.Heating.Rate..W')) %>% tibble::tibble()
    colnames(df_plr) <- gsub('.Heating.Rate..W','',colnames(df_plr))
    colnames(df_plr) <- gsub('\\.','_',colnames(df_plr))

    l_tmp <- list(heating_rate = df_htr, coil_heating_rate = df_plr)

  #... district heating
  }else if(grepl('^MSH1[12]D',heatid)){

    df_htr <- df %>%
      dplyr::select(Date.Time, contains('District.Heating.Hot.Water.Rate..W')) %>% tibble::tibble()
    colnames(df_htr) <- gsub('.District.Heating.Hot.Water.Rate..W','__W',colnames(df_htr))
    colnames(df_htr) <- gsub('\\.','_',colnames(df_htr))

    df_plr <- df %>%
      dplyr::select(Date.Time, contains('Part.Load.Ratio')) %>% tibble::tibble()
    colnames(df_plr) <- gsub('.Part.Load.Ratio..','',colnames(df_plr))
    colnames(df_plr) <- gsub('\\.','_',colnames(df_plr))

    l_tmp <- list(heating_rate = df_htr, part_load_ratio = df_plr)

  #... any other system / error
  }else{

    colnames(df)
    l_tmp <- '-'

  }

  # Return list of summaries
  return(l_tmp)
}

obtain_flow_temperatures <- function(df, l_sum){
  #' @title Extracts flow temperatures from data frame
  #'
  #' @description This function extracts inlet and outlet temperatures for different system components
  #' based on the provided system code.
  #'
  #' @param df A data frame containing temperature data.
  #' @param l_sum A list containing system information (including system code).
  #'
  #' @return A list containing extracted temperature data.

  heatid <- l_sum$house$energy_system$code

  if(grepl('^MSH[1-6]D|^MSH1[1-5]D',heatid)){

    # Extract inlet and outlet temperatures
    df_inl <- df %>% dplyr::select(dplyr::matches('Inlet.Temperature..C'))
    df_oul <- df %>% dplyr::select(dplyr::matches('Outlet.Temperature..C'))

    # Separate radiator temperatures
    df_rad_in <- df_inl %>% dplyr::select(contains('Baseboard'))
    df_rad_ou <- df_oul %>% dplyr::select(contains('Baseboard'))

    # Remove radiator temperatures from main data frames
    df_inl <- df_inl %>% dplyr::select(!contains('Baseboard'))
    df_oul <- df_oul %>% dplyr::select(!contains('Baseboard'))

    # Calculate maximum outlet temperature
    df_max <- apply(df_oul, 2, max) %>% data.frame()
    df_max$label <- rownames(df_max)
    colnames(df_max) <- c('value','label')
    df_max <- df_max %>% tibble::tibble() %>%
      dplyr::mutate(label = gsub('\\.','_',label)) %>%
      dplyr::mutate(label = gsub('_Hourly_|_Daily_|_Monthly_|_Timestep_','',label))

    # Calculate mean inlet and outlet temperatures
    df_inl <- colMeans(df_inl) %>% data.frame()
    df_inl$label <- rownames(df_inl)
    colnames(df_inl) <- c('value','label')
    df_inl <- df_inl %>% tibble::tibble() %>%
      dplyr::mutate(label = gsub('\\.','_',label)) %>%
      dplyr::mutate(label = gsub('_Hourly_|_Daily_|_Monthly_|_Timestep_','',label))

    df_oul <- colMeans(df_oul) %>% data.frame()
    df_oul$label <- rownames(df_oul)
    colnames(df_oul) <- c('value','label')
    df_oul <- df_oul %>% tibble::tibble() %>%
      dplyr::mutate(label = gsub('\\.','_',label)) %>%
      dplyr::mutate(label = gsub('_Hourly_|_Daily_|_Monthly_|_Timestep_','',label))

    # Calculate mean radiator inlet and outlet temperatures
    df_rad_in <- colMeans(df_rad_in) %>% data.frame()
    df_rad_in$label <- rownames(df_rad_in)
    colnames(df_rad_in) <- c('value','label')
    df_rad_in <- df_rad_in %>% tibble::tibble() %>%
      dplyr::mutate(label = gsub('\\.','_',label)) %>%
      dplyr::mutate(fluid = ifelse(grepl('_Air_',label),'air',
                            ifelse(grepl('_Water_',label),'water','other'))) %>%
      dplyr::mutate(label = gsub('_Hourly_|_Daily_|_Monthly_|_Timestep_','',label))

    df_rad_ou <- colMeans(df_rad_ou) %>% data.frame()
    df_rad_ou$label <- rownames(df_rad_ou)
    colnames(df_rad_ou) <- c('value','label')
    df_rad_ou <- df_rad_ou %>% tibble::tibble() %>%
      dplyr::mutate(label = gsub('\\.','_',label)) %>%
      dplyr::mutate(fluid = ifelse(grepl('_Air_',label),'air',
                            ifelse(grepl('_Water_',label),'water','other'))) %>%
      dplyr::mutate(label = gsub('_Hourly_|_Daily_|_Monthly_|_Timestep_','',label))

    # Combine results
    l_tmp <- list(system_in = df_inl, system_out = df_oul, system_max = df_max,
                  emitters_in = df_rad_in, emitters_out = df_rad_ou)

    # Additional processing for heat pump systems
    if(grepl('^MSH15|^MSH14',heatid)){

      lbl <- 'DOMESTIC_HOT_WATER_LOOP_'
      i_hp_in <- df_inl %>% dplyr::filter(grepl(lbl,label)) %>% dplyr::select(value) %>% unlist() %>% as.numeric()
      i_hp_ou <- df_oul %>% dplyr::filter(grepl(lbl,label)) %>% dplyr::select(value) %>% unlist() %>% as.numeric()
      l_hp_temp <- list(flow_in = i_hp_in, flow_out = i_hp_ou)

      l_tmp$hp_design_temperature <- l_hp_temp

    }

  }else{

    l_tmp <- '-'
  }

  return(l_tmp)
}

obtain_airflow_stats <- function(df) {
  #' @title Extracts Airflow and Setpoint Statistics
  #'
  #' @description Extracts ventilation, infiltration, and setpoint data from a given data frame.
  #'
  #' @param df A data frame containing airflow and setpoint data.
  #' @return A list containing three data frames:
  #'   - `ventilation`: Ventilation airflow statistics.
  #'   - `infiltration`: Infiltration airflow statistics.
  #'   - `setpoints`: Zone setpoint information.

  # (1) Extract ventilation airflow statistics
  lbl_block <- "ZoneVentilation Airflow Stats Nominal"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_ven <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^item|^no__"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches(
      "_m2$|^volu|^ach|^desi|^equa|^fan_eff|^max|^min|^delta"
    ), as.numeric)) %>%
    tibble::tibble()

  # (2) Extract infiltration airflow statistics
  lbl_block <- "ZoneInfiltration Airflow Stats Nominal"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_inf <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^item|^no__"), as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches(
      "_m2$|^volu|^ach|^desi|^equa"
    ), as.numeric)) %>%
    tibble::tibble()

  # (3) Extract setpoint information
  lbl_block <- "^Zone Sensible Heating"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_setp <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(
      item, thermostat_setpoint_temperature_at_peak_load_.c.,
      outdoor_temperature_at_peak_load_.c., design_day_name
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("\\.c\\."), as.numeric)) %>%
    dplyr::rename(zone = item) %>%
    tibble::tibble()

  # Return results as a list
  return(list(ventilation = df_ven, infiltration = df_inf, setpoints = df_setp))
}

obtain_indoor_performances <- function(df, len_res = 8760) {
  #' @title Extracts Indoor Performance Data
  #'
  #' @description Extracts annual building sensible heat gain and loss components from a data frame.
  #'
  #' @param df A data frame containing building performance data.
  #' @param len_res The length of the simulation results.
  #' @return A list containing two data frames:
  #'   - `addition`: Heat gain components.
  #'   - `removal`: Heat loss components.

  # Extract annual heat gain and loss data
  lbl_block <- "Annual Building Sensible Heat Gain Components"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
  df_exchng <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("\\.kwh\\.$"), as.numeric)) %>%
    tibble::tibble() %>%
    dplyr::filter(grepl("^Total", item)) %>%
    tibble::column_to_rownames(var = "item") %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("item") %>%
    dplyr::mutate(item = gsub("\\_\\.kwh\\.", "", item)) %>%
    dplyr::rename(total = Total.Facility)

  # Calculate heat gain contributions
  df_addtn <- df_exchng %>%
    dplyr::filter(total > 0) %>%
    dplyr::mutate(
      contribution = 100 * total / sum(total),
      per_day = total / (len_res / 24)
    ) %>%
    dplyr::mutate(item = gsub("_heat_addition", "", item)) %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    tibble::tibble()

  # Calculate heat loss contributions
  df_rmval <- df_exchng %>%
    dplyr::filter(total < 0) %>%
    dplyr::mutate(
      contribution = 100 * total / sum(total),
      per_day = total / (len_res / 24)
    ) %>%
    dplyr::mutate(item = gsub("_heat_removal", "", item)) %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    tibble::tibble()

  # Return results as a list
  l <- list(addition = df_addtn, removal = df_rmval)

  return(l)
}

obtain_glazing_contribution <- function(df){
  #' @title Calculates hourly glazing contribution to heating
  #'
  #' @description This function calculates the hourly average contribution of glazing
  #' (windows) to heating by analysing heat loss and gain rates.
  #'
  #' @param df A data frame containing date/time, heating status, and window
  #'          heat loss/gain data.
  #' @return A data frame summarizing average glazing contribution for each hour.

  # Column selection pattern
  lblRgx <- 'Date.Time|heat.on|Zone.Windows.Total.Heat.(Loss|Gain).Rate..W'

  # Select relevant columns and clean names
  df <- df %>% dplyr::select(dplyr::matches(lblRgx))
  colnames(df) <- gsub('Zone.Windows.Total.Heat.','', colnames(df))

  # Extract hour from date/time
  df$hour <- format(as.POSIXct(df$Date.Time,format="%H:%M:%S"),"%H")

  # Convert loss values to negative for consistency
  df[,grep("Loss",colnames(df))] <-
    df[,grep("Loss",colnames(df))] * -1

  # Group and summarize by hour
  df_byHour <- df %>%
    dplyr::group_by(hour) %>%
    # dplyr::summarise_all(list(mean)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), mean)) %>%
    dplyr::select(-Date.Time)

  return(df_byHour)
}

obtain_heat_transfer <- function(dp, tx){
  #' @title Combines envelope information and heat storage data
  #'
  #' @description This function extracts envelope information and heat storage rate from
  #' provided data sources and combines them into a list.
  #'
  #' @param dp Data frame containing building envelope or heat storage information.
  #' @param tx Data frame containing envelope information.
  #' @return A list containing envelope information and heat storage rate data.

  # Helper to summarise surface heat data
  extract_envelope_info <- function(df) {
    # @title Extracts Envelope Information
    #
    # @description Extracts opaque and translucent envelope information from a given data frame.
    #
    # @param df A data frame containing building envelope data.
    #
    # @return A list containing two data frames:
    #   - `opaque`: Opaque envelope information.
    #   - `translucent`: Translucent envelope information.

    # (1) Extract opaque envelope data
    lbl_block <- "Opaque Exterior"
    lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
    df_opque <- parse_body(df, lbl_block, lbl_header) %>%
      dplyr::select(-ctrl) %>%
      dplyr::filter(construction != "") %>%
      dplyr::mutate(dplyr::across(dplyr::matches("^construction"), tolower)) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("^refl|^u_fac|^gross|^net|^azi|^tilt"), as.numeric
      )) %>%
      tibble::tibble() %>%
      dplyr::filter(!grepl("^R|^L", item)) %>%
      dplyr::mutate(zone = gsub("^([[:alnum:]]{,2})\\:(.*)", "\\1", item)) %>%
      dplyr::select(
        item, construction, zone,
        dplyr::starts_with("gross"), dplyr::starts_with("cardin")
      ) %>%
      dplyr::mutate(face = ifelse(grepl("REAR|FRONT", zone),
                           "front-rear", "lateral"
      )) %>%
      dplyr::mutate(face = ifelse(grepl("floor$", construction), "floor", face)) %>%
      dplyr::mutate(face = ifelse(grepl("roof$", construction), "roof", face)) %>%
      dplyr::mutate(zone = gsub('^(.*)\\:(.*)','\\1',zone)) %>%
      dplyr::select(-construction, -item) %>%
      dplyr::rename(area = gross_area_.m2.) %>%
      dplyr::group_by(zone, face, cardinal_direction) %>%
      dplyr::summarise(sum = sum(area))

    # (2) Extract translucent envelope data
    lbl_block <- "Exterior Fenestration$"
    lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T) + 2])
    df_fnstr <- parse_body(df, lbl_block, lbl_header) %>%
      dplyr::select(-ctrl) %>%
      dplyr::filter(construction != "") %>%
      dplyr::mutate(dplyr::across(dplyr::matches("^construction"), tolower)) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("^refl|^u_fac|^glass|^frame|^div|^azi|^tilt"), as.numeric
      )) %>%
      tibble::tibble() %>%
      dplyr::filter(!grepl("^R|^L", item)) %>%
      dplyr::select(
        item, construction,
        dplyr::starts_with("glass_ar"), dplyr::starts_with("cardin")
      ) %>%
      dplyr::filter(!grepl("verage$", item)) %>%
      dplyr::mutate(face = ifelse(grepl("REAR|FRONT", item),
                           "front-rear", "lateral"
      )) %>%
      dplyr::select(-construction, -item) %>%
      dplyr::rename(area = glass_area_.m2.) %>%
      dplyr::group_by(face, cardinal_direction) %>%
      dplyr::summarise(sum = sum(area))

    # Return results
    l <- list(opaque = df_opque, translucent = df_fnstr)

    return(l)
  }

  # Helper to process heat storage data
  extract_heat_storage <- function(df){
    # @title Extracts heat storage values from data frame
    #
    # @description This function extracts and summarizes heat storage values from a data frame.
    # It focuses on columns starting with specific prefixes and calculates the sum.
    #
    # @param df A data frame containing heat storage rate data.
    # @return A data frame with zone names and corresponding heat storage values.

    # Heat storage column prefix
    lbl <- 'Surface.Heat.Storage.Rate'

    # Select relevant columns based on prefixes
    d_res <- df %>%
      dplyr::select(dplyr::matches(lbl)) %>%
      dplyr::select(
        dplyr::starts_with("GF"), dplyr::starts_with("X"),
        dplyr::starts_with("B"), dplyr::starts_with("A"), dplyr::starts_with("TF")
      ) %>%
      colSums() / 1000

    # Reshape data
    d_res <- as.list(d_res) %>% tibble::as_tibble()
    colnames(d_res) <- colnames(d_res) %>%
      gsub(lbl, "", .) %>%
      gsub("\\.Energy|\\.\\.W", "", .) %>%
      gsub("\\.Hourly", "", .) %>%
      gsub("\\.+$", "", .)

    # Extract zone names
    d_res <- cbind(label = names(d_res), t(d_res))
    colnames(d_res) <- c("label", paste0("V", 1:(ncol(d_res) - 1)))
    d_res <- d_res %>%
      data.frame() %>%
      tibble::tibble() %>%
      dplyr::rename(value = V1) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::mutate(zone = gsub("^([G|T]F|X\\dF|BB|A)\\.(\\d+)\\.(\\d+)(.*)", "\\1-\\2-\\3", label)) %>%
      dplyr::mutate(surface = gsub("^([G|T]F|X\\dF|BB|A)\\.(\\d+)\\.(\\d+)\\.(.*)", "\\4", label)) %>%
      dplyr::mutate(surface = tolower(surface)) %>%
      dplyr::select(-label) %>%
      dplyr::relocate(zone, surface) %>%
      dplyr::group_by(zone)

    return(d_res)
  }

  # Extract envelope information
  l_envelope <- extract_envelope_info(tx)

  # Extract heat storage rate
  d_store <- extract_heat_storage(dp)

  # Combine results
  return(list(envelope_info = l_envelope, storage_rate__W = d_store))
}

estimate_heat_loss_coefficient <- function(l_zn, l_mt, l_te, l_ar, d_dy, i_odt=NULL, i_gnt=NULL, i_hdd=NULL){
  #' @title Estimates heat loss coefficient for a building
  #'
  #' @description This function calculates the heat loss coefficient (HLC) for a building
  #' based on various building characteristics and weather data. It also estimates
  #' heat loss due to fabric conduction and ventilation.
  #'
  #' @description The heat loss coefficient (\eqn{HLC}) and heat capacity (\eqn{Q_{h}}) are calculated as follows:
  #' 
  #'   \deqn{ Qf/ΔT = \sum UxA }
  #' 
  #'   \deqn{ Qv/ΔT = 0.33 × n × V}
  #' 
  #'   \deqn{ HLC = (Q_{f} + Q_{v})/ΔT}
  #' 
  #'   \deqn{ Q_{h} = T_{in-base} × HLC}   
  #'   
  #'
  #' @param l_zn A list containing zone information (volume, etc.).
  #' @param l_mt A list containing material information (U-values, etc.).
  #' @param l_te A list containing temperature data (outdoor, setpoints).
  #' @param l_ar A list containing air-related data (ACH, etc.).
  #' @param d_dy A data frame containing drybulb temperature data (optional).
  #' @param i_odt A numeric value representing outdoor design temperature (optional, defaults to coldest outdoor temperature).
  #' @param i_gnt A numeric value representing ground temperature (optional).
  #' @param i_hdd A numeric value representing heating degree days (optional, defaults to total heating degree days).
  #'
  #' @return A list containing heat loss details for various components.

  # Define default values for optional arguments
  if(is.null(i_hdd)) i_hdd <- l_te$total
  if(is.null(i_odt)) i_odt <- l_te$coldest$outdoor_air_drybulb_temperature__c

  # Extract infiltration data
  df_achs <- l_ar$infiltration %>%
    dplyr::select(zone_name, name, ach___air_changes_per_hour) %>%
    dplyr::filter(grepl('INFILTRATION$',name)) %>%
    dplyr::rename(zone = zone_name) %>% dplyr::select(-name) %>% tibble::tibble()

  # Combine zone data with infiltration and setpoints
  df_stps <- l_ar$setpoints %>% plyr::join(df_achs, by = 'zone')


  # Fabric heat loss (Qf) calculation
  i_odp <- min(df_stps$outdoor_temperature_at_peak_load_.c.)
  i_tsp <- min(df_stps$thermostat_setpoint_temperature_at_peak_load_.c.)
  d_cnd <- l_mt$surfaces %>%
    dplyr::filter(!grepl('^L|^R', zone)) %>%
    plyr::join(df_stps, by = 'zone') %>%
    dplyr::mutate(ach___air_changes_per_hour = ifelse(
      is.na(ach___air_changes_per_hour), 0, ach___air_changes_per_hour)) %>%
    dplyr::mutate(outdoor_temperature_at_peak_load_.c. = ifelse(
      is.na(outdoor_temperature_at_peak_load_.c.), i_odp, outdoor_temperature_at_peak_load_.c.)) %>%
    dplyr::mutate(thermostat_setpoint_temperature_at_peak_load_.c. = ifelse(
      is.na(thermostat_setpoint_temperature_at_peak_load_.c.), i_tsp, thermostat_setpoint_temperature_at_peak_load_.c.)) %>%
    dplyr::mutate(conductance__W_K =
             nominal_u_.with_film_coefs.___w__m2_k * area_.net.___m2) %>%
    dplyr::mutate(outdoor_temperature_at_peak_load_.c. =
             ifelse(grepl('Ground',extboundcondition) & !is.null(i_gnt),
                    i_gnt, outdoor_temperature_at_peak_load_.c.)) %>%
    dplyr::mutate(heat_loss__W = conductance__W_K * (
      thermostat_setpoint_temperature_at_peak_load_.c. -
        outdoor_temperature_at_peak_load_.c.)) %>%
    dplyr::filter(!grepl('\\(?R|\\(?L',zone)) %>% tibble::tibble()


  # Ventilation heat loss (Qv) calculation
  d_vnt <- l_zn$summary %>%
    dplyr::rename(zone = item) %>%
    plyr::join(df_stps, by = 'zone') %>%
    dplyr::mutate(ach___air_changes_per_hour = ifelse(
      is.na(ach___air_changes_per_hour), 0, ach___air_changes_per_hour)) %>%
    dplyr::mutate(outdoor_temperature_at_peak_load_.c. = ifelse(
      is.na(outdoor_temperature_at_peak_load_.c.), i_odp, outdoor_temperature_at_peak_load_.c.)) %>%
    dplyr::mutate(thermostat_setpoint_temperature_at_peak_load_.c. = ifelse(
      is.na(thermostat_setpoint_temperature_at_peak_load_.c.), i_tsp, thermostat_setpoint_temperature_at_peak_load_.c.)) %>%
    dplyr::mutate(conductance__W_K =
             0.33 * ach___air_changes_per_hour * volume__m3) %>%
    dplyr::mutate(heat_loss__W =
             conductance__W_K * (
               thermostat_setpoint_temperature_at_peak_load_.c. -
                 outdoor_temperature_at_peak_load_.c.)) %>%
    dplyr::filter(!grepl('\\(R|\\(L',zone)) %>% tibble::tibble()


  # Heat loss transfer (HLT) in steady-state
  d_hlt <- data.frame(
    component = c('conduction', 'ventilation'),
    conductance__W_K = c(sum(d_cnd$conductance__W_K, na.rm = TRUE),
                         sum(d_vnt$conductance__W_K, na.rm = TRUE)),
    heat_loss__W = c(sum(d_cnd$heat_loss__W, na.rm = TRUE),
                     sum(d_vnt$heat_loss__W, na.rm = TRUE)))
  d_hlt <- d_hlt %>%
    dplyr::mutate(heat_loss__per = round(heat_loss__W / sum(d_hlt$heat_loss__W),2)) %>%
    dplyr::mutate(energy_usage__kWh = conductance__W_K * i_hdd / 24)


  # Summarise results
  d_out_cnd <- d_cnd %>%
    dplyr::select(zone, surface_name, area_.net.___m2,
           nominal_u_.with_film_coefs.___w__m2_k, heat_loss__W)
  d_out_vnt <- d_vnt %>%
    dplyr::select(zone, volume__m3, ach___air_changes_per_hour, heat_loss__W)
  d_out_tmp <- df_stps %>%
    dplyr::select(zone, thermostat_setpoint_temperature_at_peak_load_.c.)
  l <- list(fabric_heat_loss = d_out_cnd,
            ventilation_heat_loss = d_out_vnt,
            temperatures__C=list(room = d_out_tmp, outdoor_ddy = mean(
              df_stps$outdoor_temperature_at_peak_load_.c.)),
            heat_loss = d_hlt)

  return(l)
}

obtain_inside_heat_transfer_rate <- function(d) {
  #' @title Extracts inside heat transfer rates from data
  #'
  #' @description This function extracts inside heat transfer rates for glazing,
  #' opaque surfaces, infiltration, and collates them into a data frame.
  #' Window Transmitted Solar is subtracted so that Glazing heat gain
  #' represents the transfer of all heat through the window excluding
  #' beam and diffuse short-wave solar heat effects. This makes it more
  #' comparable with the opaque surface conduction results.
  #'
  #' @param d A data frame containing heat transfer rate data.
  #' @return A data frame with information on inside heat transfer rates.

  # Get number of timesteps and days
  i_timesteps <- dim(d)[1]
  i_timedays <- i_timesteps / 24

  # Extract heat transfer for glazing
  lbl <- "Surface.Window.Heat.Loss.Rate"
  lbg <- "Surface.Window.Heat.Gain.Rate"
  lbs <- "Total.Transmitted.Solar.Radiation.Rate"
  d_glz <- d %>%
    dplyr::select(
      dplyr::matches(lbl),
      dplyr::matches(lbg),
      dplyr::matches(lbs)
    ) %>%
    colMeans()
  d_glz <- as.list(d_glz) %>% tibble::as_tibble()
  colnames(d_glz) <- gsub("\\.TimeStep\\.|\\.W\\.", "", colnames(d_glz)) %>%
    gsub(lbl, "", .) %>%
    gsub(lbg, "", .) %>%
    gsub(lbs, "", .)
  d_glz <- cbind(label = names(d_glz), t(d_glz))
  colnames(d_glz) <- c("label", paste0("V", 1:(ncol(d_glz) - 1)))
  d_glz <- d_glz %>%
    data.frame() %>%
    tibble::tibble() %>%
    dplyr::rename(value = V1) %>%
    dplyr::filter(!grepl("^R|^L", label)) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::mutate(value = ifelse(grepl("\\.Gain\\.", label), value, value * (-1))) %>%
    dplyr::mutate(label = gsub("\\.TimeStep\\.|\\.Hourly\\.|\\.W\\.", "", label)) %>%
    dplyr::mutate(label = gsub(paste(lbl, lbg, lbs, sep = "|"), "", label)) %>%
    dplyr::mutate(zone = gsub("^([[:alnum:]]*)\\.(.*$)", "\\1", label)) %>%
    dplyr::mutate(item = gsub("^([[:alnum:]]*)\\.(.*$)", "\\2", label)) %>%
    dplyr::mutate(class = "glazing") %>%
    dplyr::relocate(zone, item, class)

  # Extract heat transfer for opaque surfaces
  lbl <- "Surface.Inside.Face.Conduction.Heat.Transfer.Rate"
  d_opq <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colMeans()
  d_opq <- as.list(d_opq) %>% tibble::as_tibble()
  colnames(d_opq) <- gsub("\\.TimeStep\\.|\\.W\\.", "", colnames(d_opq)) %>%
    gsub(lbl, "", .)
  d_opq <- cbind(label = names(d_opq), t(d_opq))
  colnames(d_opq) <- c("label", paste0("V", 1:(ncol(d_opq) - 1)))
  d_opq <- d_opq %>%
    data.frame() %>%
    tibble::tibble() %>%
    dplyr::rename(value = V1) %>%
    dplyr::filter(!grepl("^R|^L", label)) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::mutate(zone = gsub("^([[:alnum:]]*)\\.(.*$)", "\\1", label)) %>%
    dplyr::mutate(item = gsub("^([[:alnum:]]*)\\.(.*$)", "\\2", label)) %>%
    dplyr::mutate(class = ifelse(grepl("CEILING|ROOF", toupper(item)), "roof-ceiling",
      ifelse(grepl("FLOOR|GROUND", toupper(item)), "floor-ground",
        ifelse(grepl("DOOR", toupper(item)), "door-opening",
          ifelse(grepl("PARTITION", toupper(item)), "partition", "wall")
        )
      )
    )) %>%
    dplyr::relocate(zone, item, class)


  # Extract heat transfer for infiltration
  lbl <- "Zone.Infiltration.Sensible.Heat.Loss.Energy"
  d_inf <- d %>%
    dplyr::select(dplyr::matches(lbl)) %>%
    colSums() / (3600 * 24)
  d_inf <- as.list(d_inf) %>% tibble::as_tibble()
  colnames(d_inf) <- gsub("\\.TimeStep\\.|\\.J\\.", "", colnames(d_inf)) %>%
    gsub(lbl, "", .)
  d_inf <- cbind(label = names(d_inf), t(d_inf))
  colnames(d_inf) <- c("label", paste0("V", 1:(ncol(d_inf) - 1)))
  d_inf <- d_inf %>%
    data.frame() %>%
    tibble::tibble() %>%
    dplyr::rename(value = V1) %>%
    dplyr::filter(!grepl("^R|^L", label)) %>%
    dplyr::mutate(value = (-1) * as.numeric(value)) %>%
    dplyr::mutate(zone = gsub("^([[:alnum:]]*)\\.(.*$)", "\\1", label)) %>%
    dplyr::mutate(item = gsub("^([[:alnum:]]*)\\.(.*$)", "\\2", label)) %>%
    dplyr::mutate(class = "infiltration") %>%
    dplyr::relocate(zone, item, class)

  # Collate results
  l_res <- list(glazing = d_glz, opaque = d_opq, infiltration = d_inf)
  d_res <- plyr::ldply(l_res, data.frame) %>%
    tibble::tibble() %>%
    dplyr::select(-`.id`)

  return(d_res)
}

obtain_heat_transfer_ddy <- function(dp){
  #' @title Calculates heat transfer by design day item
  #'
  #' @description This function groups the input data by `ddy_item` and calculates the inside heat transfer rate for each group.
  #'
  #' @param dp A data frame containing heat transfer data with a `ddy_item` column.
  #' @return A list of data frames, each representing heat transfer data for a specific `ddy_item`.

  # Group data by design day item
  l_dp <- dp %>% dplyr::group_by(ddy_item)
  lbl_ldp <- l_dp %>% dplyr::group_keys() %>% unlist() %>% as.character()
  l_dp <- l_dp %>% dplyr::group_split()

  # Calculate heat transfer for each design day item
  l_res <- lapply(l_dp, obtain_inside_heat_transfer_rate)
  names(l_res) <- lbl_ldp

  return(l_res)
}

summarise_heat_transfer_per_design_days <- function(l_hlcs, d_ddys){
  #' @title Summarizes heat load performance by design day
  #'
  #' @description This function summarizes heat load performance for each design day.
  #'
  #' @param l_hlcs A list of heat load data frames.
  #' @param d_ddys A data frame containing design day information.
  #' @return A list of lists, each containing summary data and design conditions for a specific design day.

  # Helper function to summarize heat load for a specific design day
  obtain_hlp_ddy_summary <- function(i, lo, dd){
    # Extract data for the specific design day
    # @description Extracts heat load performance data for a specific design day.
    # @param i The design day name.
    # @param lo A list of heat load data frames.
    # @param dd A data frame containing design day information.
    # @return A list containing performance and design conditions data frames.

    # Extract data for the specific design day
    d_sub <- lo[[i]]
    d_ref <- dd %>% dplyr::filter(environment_name == i)

    # Summarize heat load by zone and class
    d_sum <- d_sub %>%
      dplyr::group_by(zone, class) %>%
      dplyr::summarise(sum = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup()

    return(list(performance = d_sum, design_conditions = d_ref))
  }

  # Apply the helper function to each design day
  l_hlc_sum <- lapply(names(l_hlcs), obtain_hlp_ddy_summary, l_hlcs, d_ddys)
  names(l_hlc_sum) <- names(l_hlcs)

  return(l_hlc_sum)
}

assign_comfort_limits <- function(df, rng = "months") {
  #' @title Assigns Comfort Temperature Limits
  #'
  #' @description Assigns comfort temperature limits based on outdoor air temperature data.
  #'
  #' @param df A data frame containing date/time and outdoor air temperature data.
  #' @param rng The time period for grouping data (e.g., 'months', 'weeks').
  #' @return A data frame with comfort limits for each time period.

  # Extract relevant columns and rename
  dd <- df %>%
    dplyr::select(Date.Time, contains("Site.Outdoor.Air.Drybulb.Temperature..C"))
  colnames(dd) <- c("Date.Time", "ExtTemp")

  # Add time period information
  if (rng == "months") {
    dd <- dd %>% dplyr::mutate(range = lubridate::month(Date.Time, label = TRUE, abbr = TRUE))
  } else {
    dd <- dd %>% dplyr::mutate(range = lubridate::week(Date.Time))
  }

  # Calculate temperature statistics by time period
  d_lim <- dd %>%
    dplyr::group_by(range) %>%
    dplyr::summarise(
      maxii = max(ExtTemp, na.rm = TRUE),
      minii = min(ExtTemp, na.rm = TRUE),
      meani = mean(ExtTemp, na.rm = TRUE),
      oscil = maxii - minii,
      cR = assign_comfort_ranges(oscil),
      tPref = 17.6 + 0.31 * meani,
      tPref.from = tPref - cR,
      tPref.to = tPref + cR
    ) %>%
    dplyr::ungroup()

  # Select relevant columns
  d_lim <- d_lim %>% dplyr::select(range, tPref, tPref.from, tPref.to)

  return(d_lim)
}

obtain_comfort_parameters <- function(df) {
  #' @title Obtains comfort parameters from building simulation data
  #'
  #' @description Estimates preliminary comfort states based on air temperature and PMV values.
  #'
  #' @param df A data frame containing building simulation data.
  #' @return A list containing two data frames:
  #'   * `pmv`: A data frame summarizing the distribution of PMV values.
  #'   * `comfort_temperature`: A data frame summarizing comfort states based on internal temperature.
  #'
  #' @details
  #' This function processes data related to zone air temperature and PMV (Predicted
  #' Mean Vote) to provide insights into occupant comfort. It calculates average
  #' internal temperature and classifies it as "a bit too hot," "ok," or "a bit too cold"
  #' based on predefined comfort limits. It also analyses PMV values and categorizes
  #' them into comfort ranges using defined labels.

  # Define temperature and PMV variable names
  lbl <- "Zone.Air.Temperature..C"
  lvl <- "Zone.Thermal.Comfort.Fanger.Model.PMV"

  # Select relevant data for temperature analysis
  dft <- df %>%
    dplyr::select(Date.Time, contains(lbl)) %>%
    dplyr::mutate(range = lubridate::month(Date.Time, label = TRUE, abbr = TRUE))

  # Join with comfort limit data (assumed to be defined elsewhere)
  dft <- plyr::join(dft, assign_comfort_limits(df), by = 'range') %>%
    dplyr::mutate(InternalAverageTemperature = rowMeans(
      dplyr::select(., contains(lbl)),
      na.rm = TRUE
    )) %>%
    dplyr::mutate(on_comfort = factor(
      ifelse(InternalAverageTemperature > tPref.to,
             "a bit too hot", ifelse(InternalAverageTemperature < tPref.from,
                                     "a bit too cold", "ok"
             )
      ),
      levels = c("a bit too cold", "ok", "a bit too hot"), ordered = T
    )) %>%
    dplyr::select(
      Date.Time, tPref.from, tPref.to,
      InternalAverageTemperature, on_comfort
    ) %>%
    tibble::tibble()

  # Select relevant data for PMV analysis
  dff <- df %>% dplyr::select(contains(lvl))

  # PMV comfort labels
  lbl_pmv <- c(
    "cold", "cool", "slightly cool",
    "neutral", "slightly warm", "warm", "hot"
  )

  # Process PMV data
  dff <- dff %>%
    tidyr::pivot_longer(cols = tidyr::everything(), names_to = "variable", values_to = "value") %>%
    tibble::tibble() %>%
    dplyr::mutate(fanger_vote = cut(value,
      breaks = c(-10, -2, -1, 0, 1, 2, 3, 10),
      labels = lbl_pmv
    )) %>%
    dplyr::mutate(variable = gsub(lvl, "", variable))

  # Summarise PMV data
  dfc <- dff %>%
    dplyr::count(variable, fanger_vote) %>%
    tidyr::pivot_wider(names_from = fanger_vote, values_from = n, values_fill = list(n = 0)) %>%
    dplyr::mutate(variable = gsub("\\.\\.\\..*$", "", variable))

  # Return results
  return(list(pmv = dfc, comfort_temperature = dft))
}

obtain_heating_size <- function(df, l_sum) {
  #' @title Extracts Heating System Information
  #'
  #' @description Extracts information on central plant, water heater, heat pump water heater,
  #' coil sizing, zone sizing, and component sizing from a data frame and a summary list.
  #'
  #' @param df A data frame containing system information data.
  #' @param l_sum A list containing a summary of the house energy system.
  #'
  #' @return A list containing extracted information:
  #'   - report: Central plant information (or "not applicable").
  #'   - heater: Water heater information (or "not applicable").
  #'   - heat_pump: Heat pump water heater information (or "not applicable").
  #'   - coil: Coil sizing information (or "not applicable").
  #'   - sizing: Zone sizing information.
  #'   - components: Component sizing information.

  # Extract energy system id
  l_en_sys <- l_sum$house$energy_system
  heatid <- l_en_sys$code

  # (1) Extract central plant information
  lbl_block <- "Central Plant"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  if (any(!is.na(lbl_header))) {
    dfr <- parse_body(df, lbl_block, lbl_header) %>%
      dplyr::select(-ctrl) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("__w|_\\.w"), as.numeric
      )) %>%
      dplyr::filter(item != "None") %>%
      tibble::tibble()
    if (dim(dfr)[1] < 1 | is.na(dfr$item[1])) {
      dfr <- "not applicable"
    }
  } else {
    dfr <- "not applicable"
  }

  # (2) Extract water heater information
  lbl_block <- "Water Heater Information"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  if (any(!is.na(lbl_header))) {
    dfi <- parse_body(df, lbl_block, lbl_header) %>%
      dplyr::select(-ctrl) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("__w|__m3|_efficiency|_factor"), as.numeric
      )) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("item"), as.integer
      )) %>%
      dplyr::filter(!is.na(item)) %>%
      tibble::tibble()
    if (dim(dfi)[1] < 1 | is.na(dfi$item[1])) {
      dfi <- "not applicable"
    }
  } else {
    dfi <- "not applicable"
  }

  # (3) Extract coil sizing information
  lbl_block <- "^Heat Pump Water Heater Information"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  if (any(!is.na(lbl_header))) {
    dfh <- parse_body(df, lbl_block, lbl_header) %>%
      dplyr::select(-ctrl) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("__w|__m3|_efficiency|_factor"), as.numeric
      )) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("item"), as.integer
      )) %>%
      dplyr::filter(!is.na(item)) %>%
      tibble::tibble()
    if (dim(dfh)[1] < 1 | is.na(dfh$item[1])) {
      dfh <- "not applicable"
    }
  } else {
    dfh <- "not applicable"
  }

  # (4)
  lbl_block <- "^Coil Sizing Summary"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  if (any(!is.na(lbl_header))) {
    dfc <- parse_body(df, lbl_block, lbl_header) %>%
      dplyr::select(-ctrl) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("_\\.w|_\\.c"), as.numeric
      )) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("item"), as.integer
      )) %>%
      tibble::tibble()
    if (dim(dfc)[1] < 1 | is.na(dfc$item[1])) {
      dfc <- "not applicable"
    }
  } else {
    dfc <- "not applicable"
  }

  # (5) Extract zone sizing information
  lbl_block <- "Zone Sizing Information"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_siz <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("_w$|_s$|_c$|_kgdryair$|_m2$|_occupants$"), as.numeric
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("item"), as.integer
    )) %>%
    tibble::tibble()

  # (6) Extract component sizing information
  lbl_block <- "Component Sizing Information"
  lbl_header <- parse_header(df[grep(lbl_block, df, useBytes = T)[1] + 2])
  df_com <- parse_body(df, lbl_block, lbl_header) %>%
    dplyr::select(-ctrl) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^value"), as.numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("item"), as.integer)) %>%
    tibble::tibble()


  # Return results as a list with filtering
  l <- list(
    report = dfr, heater = dfi, heat_pump = dfh,
    coil = dfc, sizing = df_siz, components = df_com
  )

  l <- l[lengths(l) > 1]

  return(l)
}

obtain_heating_efficiencies <- function(l_sum, l_perf, m_usge, df_sum, l_idf, l_heat) {
  #' @title Calculates Heating Efficiencies
  #'
  #' @description Calculates heating system efficiencies based on provided data.
  #'
  #' @param l_sum A list containing summary information.
  #' @param l_perf A list containing performance data.
  #' @param m_usge A matrix containing energy usage data.
  #' @param df_sum A data frame containing summary data.
  #' @param l_idf A list containing IDF data.
  #' @param l_heat A list for storing heating efficiency results.
  #'
  #' @return A list containing calculated heating efficiencies.

  # Extract energy system id
  l_ene_sys <- l_sum$house$energy_system
  heatid <- l_ene_sys$code

  if(grepl("^MSH[1-8]D|^MSH14|^MSH15", heatid)) {
    # Extract water heater recovery time
    d_wht <- lapply(
      l_idf$`WaterHeater:Sizing`, "[[",
      "waterheater_name"
    ) %>%
      unlist() %>%
      data.frame()
    if (dim(d_wht)[1] > 0) {
      colnames(d_wht) <- "component"
    }

    # Update l_heat with recovery time
    d_rec <- lapply(
      l_idf$`WaterHeater:Sizing`, "[[",
      "time_storage_can_meet_peak_draw"
    ) %>%
      unlist() %>%
      data.frame()
    if (dim(d_rec)[1] > 0) {
      colnames(d_rec) <- "fraction"
    }

    if (dim(d_rec)[1] > 0) {
      d_wht <- d_rec %>%
        dplyr::mutate(
          minutes = 60 * fraction,
          component = d_wht$component
        ) %>%
        tibble::tibble() %>%
        dplyr::select(component, minutes)
    }

    l_heat <- c(l_heat, list(recovery_time = d_wht))
    l_heat <- l_heat[lengths(l_heat) > 1]


    if (grepl("^MSH[1-8]D", heatid)) {
      # Calculate efficiency for MSH1-8D systems
      k_hvac <- l_perf$addition %>%
        dplyr::filter(grepl("^hvac", item)) %>%
        dplyr::select(total) %>%
        unlist() %>%
        as.numeric()
      k_heat <- m_usge %>%
        dplyr::filter(grepl("^heating|^water", item)) %>%
        dplyr::filter(!grepl("services", subcategory)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::matches("[\\.|\\_]kwh")) %>%
        unlist() %>%
        as.numeric() %>%
        sum()
      i_s_eff <- round(100 * k_hvac / k_heat)

      l_heat <- c(l_heat, list(
        efficiency = list(value = i_s_eff, type = 'heater efficiency')))
    }

    if (grepl("^MSH14|^MSH15", heatid)) {
      # Calculate efficiency for Heat-pump systems
      k_hvac <- l_perf$addition %>%
        dplyr::filter(grepl("^hvac", item)) %>%
        dplyr::select(total) %>%
        unlist() %>%
        as.numeric()
      k_iele <- m_usge %>%
        dplyr::filter(grepl("^heating|^water", item)) %>%
        dplyr::select(dplyr::starts_with("electricity")) %>%
        unlist() %>%
        as.numeric() %>%
        sum()
      i_s_eff <- round(100 * k_hvac / k_iele)

      # Extract coil data
      lbl_block <- "^Heating Coils"
      k_match <- grep(lbl_block, df_sum, useBytes = T)[1]
      lbl_header <- parse_header(df_sum[k_match + 2])
      dfr <- parse_body(df_sum, lbl_block, lbl_header) %>%
        dplyr::select(-ctrl) %>%
        dplyr::mutate(dplyr::across(dplyr::matches(
          "_\\.w"
        ), as.numeric)) %>%
        tibble::tibble()

      # Extract water heater data
      i_n_eff <- dfr %>%
        dplyr::select(dplyr::starts_with("nominal_efficiency_")) %>%
        unlist() %>%
        as.numeric()
      i_coil_c <- dfr %>%
        dplyr::select(dplyr::starts_with("nominal_total_capacity_")) %>%
        unlist() %>%
        as.numeric()
      i_coil_c <- ceiling(i_coil_c / 100) * 100

      l_heat <- c(l_heat, list(
        coil_size__W = i_coil_c,
        coil_efficiency = i_n_eff,
        list(efficiency = list(value = i_s_eff, type = 'seasonal-COP'))
      ))
    }
  }

  return(l_heat)
}



# workflow: post-evaluation ---------------------------------------------------

extract_simulation_outputs <- function(i){
  #' @title Extracts simulation outputs
  #'
  #' @description This function extracts and processes simulation outputs
  #' (ie. `results_eplus.csv.gz` and `results_summary.csv.gz`) stored in
  #' the folder <local>/outcomes/simulation/b_outputs/<id>
  #'
  #' @param i [integer] The index of the evaluated IDF file.
  #'
  #' @return A list containing extracted simulation data, including energy
  #' demand, performance metrics, output cards, raw results, and schedules.
  #'
  #' @family EnergyPlus evaluation
  #' @export


  # define directory pattern of results
  path_idf <- the$.enhub.paths$eval$C

  idf_built_status("Results folder found")

  # collection of results -----------------------------------------------------
  l_idf_summary <- read_input_summary(i)
  l_idf_input <- read_input_file(i)
  txt_summary <- read_idf_outcomes(i)

  df_results <- dig_simulation_outcomes(i)
  df_results_ddy <- dig_simulation_outcomes(i, outSet="ddy")
  df_schedule <- obtain_simulation_schedule(i)

  l_epw <- l_idf_summary$weather
  l_dates <- list(period=df_results$Date.Time, ddy=df_results_ddy$Date.Time)
  l_thermal_zones <- l_idf_summary$house$thermal_zones

  idf_built_status("Loaded raw simulation outcomes")


  # append Part load efficiency & heating status ------------------------------
  df_results <- obtain_heating_status(df_results)


  # design days ----------------------------------------------------------------
  df_design_days <- obtain_ddy_summary(txt_summary, yr = year(l_dates$ddy[[1]]))
  df_results_ddy <- obtain_design_days(df_results_ddy, df_design_days)


  # building dimensions -------------------------------------------------------
  l_zone_summary <- obtain_zone_summary(txt_summary)
  i_tfa <- l_zone_summary$key$area__m2


  # summary of surfaces -------------------------------------------------------
  suppressWarnings(l_surfaces <- obtain_surface_summary(txt_summary))


  # summary of materials ------------------------------------------------------
  suppressWarnings(l_materials <- obtain_material_summary(txt_summary))


  # zone dimensions -----------------------------------------------------------
  l_zones <- summarise_zone_dimensions(l_surfaces$surfaces, l_zone_summary)


  # energy demand / fuel, cost, carbon + source -------------------------------
  l_demand <- obtain_fuel_demand(df_results, i_tfa)
  df_energy_fuel <- l_demand$table
  df_energy_cost <- obtain_fuel_expenditure(l_demand$summary)
  l_energy_demand <- obtain_energy_demand_profile(df_energy_fuel)
  df_energy_fuel_sum <- obtain_carbon_and_primary(l_demand$summary)


  # energy (& water) demand / end-use services --------------------------------
  df_energy_use <- obtain_end_use_summary(df_results, i_tfa, df_energy_fuel_sum)
  df_end_use <- obtain_end_use_fuel(txt_summary)
  df_water_use <- obtain_water_services(df_results, df_end_use)


  # annual temperature --------------------------------------------------------
  l_indoor_temp <- obtain_annual_temperature(df_results)
  df_indoor_temp_ddy <- obtain_zone_temperature_ddy(df_results_ddy)


  # HLP/HDD approximation -----------------------------------------------------
  i_hdd_base <- 15.5
  l_site_air <- obtain_outdoor_profile(df_results, i_hdd_base)
  l_hdd <- l_site_air$hdd
  df_room_air <- obtain_indoor_profile(df_results, i_hdd_base, l_thermal_zones)
  l_room_air <- df_room_air %>% jsonlite::toJSON(pretty = F, auto_unbox = T)
  df_room_air <- lapply(df_room_air,
                        function(x) x[!names(x) %in% 'mean_temperature__C'])


  # energy system operation ---------------------------------------------------
  l_heating_performance <-
    obtain_energy_system_performance(df_results, l_idf_summary)


  # flow_temperatures ---------------------------------------------------------
  df_flow_temp <- obtain_flow_temperatures(df_results, l_idf_summary)


  # obtain airflow summary ----------------------------------------------------
  l_zone_air <- suppressWarnings(obtain_airflow_stats(txt_summary))


  # annual peak heat transfer -------------------------------------------------
  l_performance <- obtain_indoor_performances(txt_summary, dim(df_results)[1])


  # heat transfer in translucent surfaces -------------------------------------
  df_translucent <- obtain_glazing_contribution(df_results)


  # heat transfer per wall approximation --------------------------------------
  l_heat_transfer <- obtain_heat_transfer(df_results, txt_summary)


  # calculate heat loss transfer (simplistic model) ---------------------------
  l_hlc <- estimate_heat_loss_coefficient(
    l_zone_summary, l_surfaces, l_hdd, l_zone_air, df_design_days)


  # DDYs differences ----------------------------------------------------------
  l_hlc_ddys <- obtain_heat_transfer_ddy(df_results_ddy)
  l_hlc_sum <- summarise_heat_transfer_per_design_days(l_hlc_ddys, df_design_days)


  # get preliminary comfort states --------------------------------------------
  l_comfort <- obtain_comfort_parameters(df_results)


  # heating size --------------------------------------------------------------
  l_heat_size <- suppressWarnings(
    obtain_heating_size(txt_summary, l_idf_summary))
  l_heat_size <- obtain_heating_efficiencies(
    l_idf_summary, l_performance, df_end_use, txt_summary, l_idf_input, l_heat_size)


  idf_built_status("Parsed simulation outcomes")


  # summary objects -----------------------------------------------------------
  t_dem <- df_energy_use %>%
    dplyr::select(energy_demand__kWh, contribution__x100) %>%
    tibble::rownames_to_column('system')
  v_int <- colSums(df_energy_use) %>% t %>% data.frame() %>%
    dplyr::select(energy_intensity__kWh_m2) %>% as.numeric()
  v_dem <- colSums(df_energy_use) %>% t %>% data.frame() %>%
    dplyr::select(energy_demand__kWh) %>% as.numeric()
  t_tmp <- l_indoor_temp$monthly
  t_add <- l_performance$addition
  t_los <- l_performance$removal
  t_mats <- l_materials$construction %>%
    dplyr::select(-time_step___hours,-no_ctfs,-item)
  t_srfc <- l_surfaces$surfaces %>%
    dplyr::filter(!grepl('^L|^R', zone))
  t_inth <- plyr::ldply(l_indoor_temp$on_heat, data.frame) %>%
    tibble::tibble() %>%
    dplyr::arrange(Heating.Active.Status) %>%
    dplyr::filter(Heating.Active.Status == TRUE) %>%
    dplyr::select(-Heating.Active.Status)


  # summary card --------------------------------------------------------------
  l_outcard <- list(
    uuid = l_idf_summary$uuid,
    energy = list(
      demand_per_system = t_dem,
      total_demand__kWh = v_dem,
      intensity__kWh_m2 = v_int),
    heat_loss = list(
      building=l_zone_summary$key,
      heat_transfer_simple=l_hlc,
      gain = t_add,
      loss = t_los,
      constructions = t_mats,
      surfaces = t_srfc),
    indoor = list(
      rooms_temperature__C = df_room_air,
      average_temperature_C = t_tmp,
      zone_temperature_C = t_inth,
      comfort_summary = l_comfort$pmv),
    heating_degree_days = list(
      weather_location = l_epw$info,
      total = l_hdd$total,
      threshold = l_hdd$threshold,
      coldest = l_hdd$coldest),
    hot_water = list(
      volume_daily__l = ceiling(df_water_use$water_volume / 365 * 1000),
      dwh_demand__kWh = ceiling(df_water_use$dwh_demand__kWh)
    ),
    water_heater = l_heat_size)

  if(any(df_flow_temp!='-')) l_outcard$flow_temperatures <- df_flow_temp


  # combine results in a list-card --------------------------------------------
  l_out_res <- list(
    energy_demand = df_energy_fuel_sum %>%
      tibble::rownames_to_column(var = "fuel") %>% tibble::as_tibble(),
    performance = l_performance,
    output_card = l_outcard,
    results = df_results,
    schedules = df_schedule
  )


  # dictionaries for further analysis -----------------------------------------
  l_outcard <- l_outcard %>%
    jsonlite::toJSON(pretty = FALSE, auto_unbox = TRUE)
  l_zones <- l_zones %>%
    jsonlite::toJSON(pretty = FALSE, auto_unbox = TRUE)
  l_hlc_sum <- l_hlc_sum %>%
    jsonlite::toJSON(pretty = FALSE, auto_unbox = TRUE)
  l_heat_transfer <- l_heat_transfer %>%
    jsonlite::toJSON(pretty = FALSE, auto_unbox = TRUE)
  l_heating_performance <- l_heating_performance %>%
    jsonlite::toJSON(pretty = FALSE, auto_unbox = TRUE)


  idf_built_status("Summary card generated")

  # export objects ------------------------------------------------------------

  # define some aliases
  wl <- writeLines; ws <- write.csv; yy <- paste0

  # prepare export directory
  r_mkdir(path_out <- file.path(path_idf, i))

  # export data
  ws(l_hdd$table, yy(path_out,"/heating_degree_days.csv"))
  ws(l_indoor_temp$monthly, yy(path_out,"/monthly_indoor_temperature.csv"))
  ws(plyr::ldply(l_indoor_temp$on_heat, data.frame), yy(path_out,"/monthly_indoor_temperature_heating.csv"))
  ws(plyr::ldply(l_indoor_temp$hourly, data.frame), yy(path_out,"/monthly_indoor_temperature_hourly.csv"))
  ws(df_end_use, yy(path_out,"/end_use_summary.csv"))
  ws(l_zone_air$infiltration, yy(path_out,"/annual_heat_loss_infiltration.csv"))
  ws(l_performance$addition, yy(path_out,"/annual_heat_loss_addition.csv"))
  ws(l_performance$removal, yy(path_out,"/annual_heat_loss_removal.csv"))
  ws(df_translucent, yy(path_out,"/annual_heat_loss_translucent.csv"))
  ws(l_zone_summary$summary, yy(path_out,"/zone_summary.csv"))
  ws(l_materials$construction, yy(path_out,"/summary_construction_surfaces.csv"))
  ws(l_materials$materials, yy(path_out,"/summary_construction_materials.csv"))
  ws(l_materials$glazing, yy(path_out,"/summary_construction_glazing.csv"))
  ws(l_materials$opaque, yy(path_out,"/summary_construction_opaque.csv"))
  ws(l_surfaces$surfaces, yy(path_out,"/summary_surfaces.csv"))
  ws(df_design_days, yy(path_out,"/summary_design_days.csv"))
  ws(plyr::ldply(l_hlc_ddys, data.frame), yy(path_out,"/summary_ddy_heat_rate.csv"))
  ws(plyr::ldply(df_indoor_temp_ddy, data.frame), yy(path_out,"/summary_ddy_temperatures.csv"))

  wl(l_energy_demand, yy(path_out,"/profile_energy_demand.json"))
  wl(l_site_air$site, yy(path_out,"/profile_air_site.json"))
  wl(l_room_air, yy(path_out,"/profile_air_room.json"))
  wl(l_heat_transfer, yy(path_out,"/summary_heat_surfaces.json"))
  wl(l_hlc_sum, yy(path_out,"/summary_ddy_hlc.json"))
  wl(l_zones, yy(path_out,"/summary_zones_card.json"))
  wl(l_outcard, yy(path_out,"/summary_out_card.json"))

  if(any(l_heating_performance != '-')){
    wl(l_heating_performance, yy(path_out,"/profile_performance.json"))
  }


  # return summary card -------------------------------------------------------
  return(l_out_res)
}
