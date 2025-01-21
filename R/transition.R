# .. perform additional formatting ---------------------------------------------

obtain_income_bands <- function(income) {
  #' @title Classify income into bands
  #' @description Classifies income values (`income`) into bands: "low", "mid",
  #'   or "high".
  #' @param income A numeric vector containing income values.
  #' @return A character vector of the same length as `income` with income
  #'   bands.

  cut(income,
    breaks = c(-Inf, 1500, 3000, Inf),
    labels = c("low", "mid", "high"),
    right = FALSE,
    include.lowest = TRUE
  )
}

obtain_tenure_bands <- function(tenure_type) {
  #' @title Classify tenure type into bands
  #' @description Classifies tenure type (`tenure_type`) into bands: "Private",
  #'   "Social", or NA.
  #' @param tenure_type A character vector containing tenure type information.
  #' @return A character vector of the same length as `tenure_type` with tenure
  #'   bands.

  dplyr::case_when(
    grepl("own|priv", tenure_type, ignore.case = TRUE) ~ "Private",
    grepl("refuse|not|know|answer", tenure_type, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ "Social"
  )
}

obtain_typology_bands <- function(dwelling_type) {
  #' @title Classify dwelling type into bands
  #' @description Classifies dwelling type (`dwellingType`) into bands: "House"
  #'   or "Apartment".
  #' @param dwelling_type A character vector containing dwelling type
  #'   information.
  #' @return A character vector of the same length as `dwelling_type` with
  #'   dwelling type bands.

  dplyr::case_when(
    grepl("house|other|terrace|detach|bungalow", dwelling_type, ignore.case = TRUE) ~ "House",
    grepl("refuse|not|know|answer", dwelling_type, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ "Apartment"
  )
}

obtain_region_bands <- function(region) {
  #' @title Classify region into bands
  #' @description Classifies region information (`region`) into standard UK
  #'   region bands.
  #' @param region A character vector containing region information.
  #' @return A character vector of the same length as `region` with region
  #'   bands.

  dplyr::case_when(
    grepl("Yorkshire", region, ignore.case = TRUE) ~ "Yorkshire and the Humber",
    grepl("London", region, ignore.case = TRUE) ~ "London",
    grepl("North.*East", region, ignore.case = TRUE) ~ "North East",
    grepl("North.*West", region, ignore.case = TRUE) ~ "North West",
    grepl("East.*Midlands", region, ignore.case = TRUE) ~ "East Midlands",
    grepl("West.*Midlands", region, ignore.case = TRUE) ~ "West Midlands",
    grepl("South.*East", region, ignore.case = TRUE) ~ "South East",
    grepl("South.*West", region, ignore.case = TRUE) ~ "South West",
    grepl("^East.*England$|^East$", region, ignore.case = TRUE) ~ "East of England",
    grepl("Wales", region, ignore.case = TRUE) ~ "Wales",
    grepl("Scotland", region, ignore.case = TRUE) ~ "Scotland",
    grepl("Northern.*Ireland", region, ignore.case = TRUE) ~ "Northern Ireland",
    grepl("refuse|not|know|answer", region, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ "-"
  )
}

obtain_employment_bands <- function(employment_status) {
  #' @title Classify employment status into bands
  #' @description Classifies employment status information (`employment_status`)
  #'   into bands.
  #' @param employment_status A character vector containing employment status
  #'   information.
  #' @return A character vector of the same length as `employment_status` with
  #'   employment bands.

  dplyr::case_when(
    grepl("time work|paid employment|self", employment_status, ignore.case = TRUE) ~ "paid employment",
    grepl("retired", employment_status, ignore.case = TRUE) ~ "retired",
    grepl("refuse|not|know|answer", employment_status, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ "irregular"
  )
}

obtain_household_bands <- function(household_type) {
  #' @title Classify household type into bands
  #' @description Classifies household type information (`household_type`) into
  #'   bands.
  #' @param household_type A character vector containing household type
  #'   information.
  #' @return A character vector of the same length as `household_type` with
  #'   household type bands.

  dplyr::case_when(
    grepl("(one|single) person", household_type, ignore.case = TRUE) ~ "single person household",
    grepl("couple.*with.*child", household_type, ignore.case = TRUE) ~ "couple with dependants",
    grepl("couple.*no .*child", household_type, ignore.case = TRUE) ~ "couple no dependants",
    grepl("(lone|single)*with", household_type, ignore.case = TRUE) ~ "lone parent with dependants",
    grepl("refuse|not|know|answer", household_type, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ "other type of household"
  )
}

assign_age_bands <- function(age) {
  #' @title Assign age bands
  #' @description Assigns age information (`age`) into bands: "child", "young",
  #'   or "adult".
  #' @param age A numeric vector containing age information.
  #' @return A character vector of the same length as `age` with age bands.

  dplyr::case_when(
    age < 12 ~ "child",
    age < 21 ~ "young",
    TRUE ~ "adult"
  )
}


# .. process matrices and indices ----------------------------------------------

obtain_indices <- function(d_tus = s_tus$tbl_individual, path = s_tus$paths$proj) {
  #' @title Prepare TUS data for analysis
  #' @description This function selects relevant variables from a TUS data frame
  #'   (`d_tus`), re-factors some variables for consistency, and removes
  #'   partially empty rows.
  #' @param d_tus A data frame containing TUS data (default:
  #'   s_tus$tbl_individual).
  #' @return A data frame containing prepared TUS data.

  # Select relevant variables
  d_sel <- d_tus %>%
    dplyr::select(serial, NumAdult, NumChild, dtenure, dgorpaf, Accom) %>%
    dplyr::distinct() %>%
    tibble::tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))

  # Re-factor dwelling type
  d_sel <- d_sel %>%
    dplyr::mutate(.dwtype = ifelse(grepl("house|bungalow", Accom, ignore.case = TRUE),
      "House",
      ifelse(grepl("flat|maisonette", Accom, ignore.case = TRUE),
        "Apartment", NA
      )
    ))

  # Re-factor region
  d_sel <- d_sel %>%
    dplyr::mutate(
      .region =
        ifelse(grepl("north west|merseyside", dgorpaf, ignore.case = TRUE),
          "North West",
          ifelse(grepl("yorksh|humber", dgorpaf, ignore.case = TRUE),
            "Yorkshire and the Humber",
            ifelse(grepl("^East.*England$", dgorpaf, ignore.case = TRUE),
              "Eastern England", dgorpaf
            )
          )
        )
    )

  # Re-factor tenure
  d_sel <- d_sel %>%
    dplyr::mutate(.tenure = ifelse(
      grepl("^own|mortgage$", dtenure, ignore.case = TRUE), "Private",
      ifelse(grepl("rent|public", dtenure, ignore.case = TRUE), "Rented", NA)
    ))

  # Create household size code
  d_sel <- d_sel %>%
    dplyr::mutate(.hhsize = paste(
      ifelse(NumAdult > 4, "X", NumAdult),
      ifelse(NumChild > 3, "more", NumChild),
      sep = "."
    ))

  # Remove partially empty rows
  d_sel <- d_sel %>%
    dplyr::filter(!is.na(.dwtype) | !is.na(.tenure)) %>%
    dplyr::select(serial, .dwtype, .region, .tenure, .hhsize) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  return(d_sel)
}

obtain_cross_indices <- function(d_tus = s_tus$tbl_individual,
                                 d_ehs = tbl.EHS.common,
                                 path = path_TUS) {
  #' @title Creates cross-indices for TUS and EHS datasets.
  #'
  #' @description This function processes two datasets (d_tus and d_ehs),
  #'   creates cross-indices for various variables, and saves the results as CSV
  #'   files.
  #'
  #' @param d_tus A data frame containing individual-level data (default:
  #'   s_tus$tbl_individual).
  #' @param d_ehs A data frame containing household-level data (default:
  #'   tbl.EHS.common).
  #' @param path The path to the output directory (default: path_TUS).
  #'
  #' @return A list containing the processed datasets (d_ehs and d_tus).

  # Select relevant variables and create cross-indices
  d_tus <- d_tus %>%
    dplyr::select(serial, Income, dtenure, Accom, dgorpaf, WorkSta, dhhtype, DVAge) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      .cross.income = obtain_income_bands(Income),
      .cross.tenure = obtain_tenure_bands(dtenure),
      .cross.type = obtain_typology_bands(Accom),
      .cross.region = obtain_region_bands(dgorpaf),
      .cross.employment = obtain_employment_bands(WorkSta),
      .cross.hhtype = obtain_household_bands(dhhtype),
      .cross.age = assign_age_bands(DVAge)
    )

  d_ehs <- d_ehs %>%
    dplyr::select(serial, hhincx, tenure2x, dwtype3x, gorehs, emphrpx, hhsizex, hhcompx) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      .cross.income = obtain_income_bands(hhincx / 12),
      .cross.tenure = obtain_tenure_bands(tenure2x),
      .cross.type = obtain_typology_bands(dwtype3x),
      .cross.region = obtain_region_bands(gorehs),
      .cross.employment = obtain_employment_bands(emphrpx),
      .cross.hhtype = obtain_household_bands(hhcompx)
    )

  # Select and format output datasets
  d_ehs <- d_ehs %>%
    dplyr::select(serial, .cross.income, .cross.tenure, .cross.type, .cross.region, .cross.employment, .cross.hhtype) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      serial = as.integer(serial),
      dplyr::across(where(is.character), as.factor)
    )

  d_tus <- d_tus %>%
    dplyr::select(serial, .cross.income, .cross.tenure, .cross.type, .cross.region, .cross.employment, .cross.hhtype, .cross.age) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      serial = as.integer(serial),
      dplyr::across(where(is.character), as.factor)
    )

  # Save output datasets
  write.csv(d_ehs, file.path(the$.enhub.paths$store$profiles, "tbl_EHS_cross.csv"))
  write.csv(d_tus, file.path(the$.enhub.paths$store$profiles, "tbs_tus_cross.csv"))

  # Return a list of the datasets
  list(EHS = d_ehs, TUS = d_tus)
}


# .. process profiles ----

obtain_activity_group <- function(d_ref = s_tus$activities_groups) {
  #' @title Obtain Activity Groups
  #' @description Prepares a data.frame containing activity group information
  #'   from a reference dataset.
  #'
  #' @param d_ref Reference dataset containing activity group information
  #'   (default: s_tus$activities_groups).
  #'
  #' @return A data.frame with the following columns:
  #'   * id: Unique identifier for the activity group.
  #'   * name: Name of the activity group (with underscores replacing hyphens).

  # Prepare and return activity group data
  d_out <- as.data.frame(d_ref)
  colnames(d_out) <- c("id", "name")
  d_out <- d_out[!is.na(d_out$name), ]
  d_out$name <- gsub("-", ".", d_out$name)
  rownames(d_out) <- NULL
  return(d_out)
}

generate_timeslot_table <- function(year = the$GLOBAL_PAR$simulation_year, resolution = "10 min") {
  #' @title Generate Timeslot Table
  #' @description Creates a data.frame defining time slots for a given year.
  #'
  #' @param year The year for which to generate time slots (default:
  #'   the$GLOBAL_PAR$simulation_year)
  #' @param resolution The time slot interval (default: "10 min").
  #'
  #' @return A data.frame with the following columns:
  #'   * tid: Unique identifier for the time slot.
  #'   * tid.from: Starting time of the time slot (HH:MM format).
  #'   * tid.to: Ending time of the time slot (HH:MM format).
  #'   * tid.slot: Combined string representation of starting and ending times (HH:MM-HH:MM).

  # Generate time ranges for each slot
  start_time <- lubridate::ymd_hm(paste0(year, "-01-01 04:00"))
  end_time <- lubridate::ymd_hm(paste0(year, "-01-02 03:59"))
  slotA <- seq(start_time, end_time, by = resolution)
  slotB <- slotA + lubridate::minutes(10)

  # Create data frame with unique identifiers and time slots
  d_out <- data.frame(
    tid = 1:length(slotA),
    tid.from = strftime(slotA, format = "%H:%M"),
    tid.to = strftime(slotB, format = "%H:%M"),
    tid.slot = paste(strftime(slotA, format = "%H:%M"), strftime(slotB, format = "%H:%M"), sep = "-")
  )

  return(d_out)
}

generate_timeslot <- function(kslot) {
  #' @title Generate Time Slots
  #' @description Creates a sequence of time slots within a day based on the
  #'   specified interval.
  #'
  #' @param kslot The desired time slot interval (e.g., "1hour", "10min",
  #'   "30min").
  #' @return A numeric vector representing the starting points of time slots
  #'   within a day.
  #'
  #' @examples
  #' \dontrun{
  #' # Generate hourly time slots
  #' hourly_slots <- generate_timeslot("1hour")
  #'
  #' # Generate 10-minute time slots
  #' ten_minute_slots <- generate_timeslot("10min")
  #' }

  intervals <- list(
    "1hour" = seq(0, 23.99, by = 1),
    "60min" = seq(0, 23.99, by = 1),
    "10min" = trunc(seq(0, 239.9, by = 10 / 6)) / 10,
    "30min" = seq(0, 23.99, by = 0.5)
  )

  d_slot <- intervals[[kslot]]

  if (is.null(d_slot)) {
    message("Please check requested time-slot... 1 hour assigned")
    d_slot <- intervals[["1hour"]]
  }

  return(d_slot)
}

obtain_transition_tables <- function(dtaSource, varSlot = "1hour", varOut = TRUE, tblActs = s_tus$activities) {
  #' @title Obtain Transition Tables
  #'
  #' @description Generates transition tables summarizing activity data,
  #'   including occurrences, proportions, and probabilities.
  #'
  #' @param dtaSource Data frame containing the source activity data.
  #' @param varSlot Character string specifying the time slot resolution
  #'   (default: "1hour").
  #' @param varOut Logical indicating whether to include outside activities
  #'   (default: TRUE).
  #' @param tblActs Data frame containing activity definitions (default:
  #'   s_tus$activities).
  #'
  #' @return A list of data frames and matrices summarizing activity
  #'   transitions, or a message indicating invalid data.

  # Helper function to generate and summarize a subset of activity data
  generate_and_summarise_subset <- function(activity_data, activity_definitions, time_slots, time_window, slot_conversion_factor) {
    # @title Generate and Summarize Subset of Activity Data
    # @description Filters, joins, and summarizes activity data for a specific
    #   context (e.g., location).
    # @param activity_data Original data frame containing activity information.
    # @param activity_definitions Reference data frame with activity
    #   definitions.
    # @param time_slots Reference data frame with time slots.
    # @param time_window Time window parameter for aggregation (e.g., number of
    #   time slots).
    # @param slot_conversion_factor Time slot conversion factor (e.g., minutes
    #   per time slot).
    # @return A data frame containing summarized activity counts for each time
    #   window and activity group.

    activity_data <- activity_data %>%
      tibble::tibble() %>%
      dplyr::filter(WhereWhen == "Home") %>%
      dplyr::select(serial, pnum, tid, whatdoing, DiaryDate_Act, DVAge) %>%
      dplyr::left_join(activity_definitions, by = c("whatdoing" = "name")) %>%
      dplyr::left_join(time_slots, by = c("tid" = "tid.slot")) %>%
      dplyr::mutate(
        group = gsub("-", ".", group),
        tid.from = floor(as.numeric(as.difftime(tid.from, format = "%H:%M", units = "mins")) / slot_conversion_factor) / time_window
      ) %>%
      dplyr::arrange(serial, pnum, tid.from) %>%
      dplyr::group_by(tid.from, group) %>%
      dplyr::summarise(activity_count = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(!is.na(group), !is.na(activity_count))

    return(activity_data)
  }

  # Helper function to convert tables to matrices
  convert_table_to_matrix <- function(data_frame) {
    # @title Convert Data Frame to Matrix
    # @description Reshapes a data frame into a matrix format, suitable for
    #   further analysis.
    # @param data_frame The input data frame to be converted.
    # @return A matrix representation of the input data frame.

    matrix_data <- data_frame %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      tibble::column_to_rownames(var = "tid")

    return(matrix_data)
  }

  # Helper function to validate activity data
  validate_data_acts <- function(dfval) {
    # @title Validate Activity Data
    # @description Prepares and validates activity data for further analysis.
    # @param dfval The raw data frame containing activity data.
    # @return A data frame containing the validated and formatted activity data.

    impute_matrix <- function(time_gap, max_value) {
      # @title Impute Missing Values in Time Series Matrix
      # @description Imputes missing values in a time series matrix based on
      #   previous values.
      # @param time_gap The time gap between the current and previous time
      #   step.
      # @param max_value The maximum allowed value for imputation.
      # @return A matrix containing the imputed values.

      # Identify active time step
      ts_data <- dtaTime
      active_index <-
        as.integer(rownames(ts_data[ts_data$tid == time_gap, ]))
      imputed_data <- NULL

      # Handle cases where time gap is zero or greater than zero
      if (time_gap == 0.0) {
        imputed_data <- max_value
      } else {
        imputed_data <- ts_data[active_index - 1, 2:11]
      }

      # Update the original data with imputed values
      ts_data[active_index, 2:11] <- imputed_data
      assign("dtaTime", ts_data, inherits = TRUE)
      rownames(imputed_data) <- NULL

      # Return the imputed values
      return(imputed_data)
    }

    # Obtain activity group labels
    lbl_acts <- obtain_activity_group()

    # Reshape data to time series format with activity groups as variables
    dtaTime <- dfval %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::rename_with(~ gsub("-", ".", .))
    dtaTimed <- dtaTime
    dtaTime.cols <- colnames(dtaTime)

    # Create reference data frame with activity labels
    dtaTime.Ref <- as.data.frame(
      matrix(NA,
        nrow = dim(dtaTime)[1],
        ncol = 1 + dim(lbl_acts)[1]
      )
    )
    colnames(dtaTime.Ref) <- c("tid", lbl_acts$name)
    dtaTime.Ref[, dtaTime.cols] <- dtaTime
    dtaTime <- dtaTime.Ref

    # Convert IDs to numeric, fill missing values with 0
    dtaTime$tid <- as.numeric(as.character(dtaTime$tid))
    dtaTime[is.na(dtaTime)] <- 0

    # Calculate event occurrence and identify gaps
    dtaTime$events <- rowSums(dtaTime[, 2:11], na.rm = TRUE)
    dtaTime$events <- ifelse(dtaTime$events > 0, TRUE, FALSE)
    dtaTime$occurence <- dtaTime$events * dtaTime$tid
    valMaxEvent <- max(dtaTime$occurence)
    dtaMaxEvent <-
      dtaTime[dtaTime$occurence == valMaxEvent, 2:11]
    dtaTimeGaps <-
      dtaTime$tid[dtaTime$events == FALSE]

    # Impute missing values if gaps exist
    if (length(dtaTimeGaps) > 0) {
      dtaImputated <-
        plyr::ldply(lapply(dtaTimeGaps, impute_matrix, dtaMaxEvent), data.frame)
      dtaImputated$tid <- dtaTimeGaps
      dtaTimed$tid <- as.numeric(as.character(dtaTimed$tid))
      dtaTimed <- dtaTimed[, dtaTime.cols]
      dtaImputated <- dtaImputated[, dtaTime.cols]
      dtaTimed[dtaTimed$tid %in% dtaTimeGaps, ] <- dtaImputated
    } else {
      dtaTimed <- dtaTime
    }

    # Reshape back to wide format and convert IDs to numeric
    dtaTimed <- dtaTimed %>%
      tidyr::pivot_longer(cols = -1, names_to = "variable", values_to = "value") %>%
      dplyr::mutate(tid = as.numeric(tid))

    return(dtaTimed)
  }

  # Helper function to rescale data
  rescale_data <- function(df) {
    # @title Rescale Data Frame
    # @description Rescales the values in a data frame by dividing each row by
    #   its total.
    # @param df The input data frame to be rescaled.
    # @return A rescaled data frame.

    # Reshape data to wide format
    df <- df %>%
      tidyr::pivot_wider(names_from = variable, values_from = value)

    # Extract time ID and remove unnecessary columns
    df_tid <- df$tid
    df$tid <- df$events <- df$occurence <- NULL
    df_cols <- colnames(df)

    # Calculate row totals and rescale
    df$tot <- rowSums(df)
    df <- df / df$tot
    df$tot <- NULL

    # Restore time ID and reshape back to long format
    df$tid <- df_tid
    df <- df[, c("tid", df_cols)]
    df <- df %>%
      tidyr::pivot_longer(cols = -1, names_to = "variable", values_to = "value")

    return(df)
  }

  # Helper function to obtain occurrence table
  get_table_ocurrence <- function(dtm, di) {
    # @title Obtain Occurrence Table for Activity Groups
    # @description Creates a table summarizing activity group occurrences across
    #   time steps.
    # @param dtm Data table containing time information (e.g., tid.from.num).
    # @param di Data frame containing activity group definitions.
    # @return A data frame with columns for time step (tid), activity variable,
    #   and value (occurrence count).

    # Obtain activity group data
    df <- obtain_activity_group()

    # Create grid of time steps and activity groups
    ds <- expand.grid(
      tid.from = as.character(unique(dtm$tid.from.num)),
      group = as.character(unique(df$name))
    )

    # Reformate grid
    ds$tid.from <- as.numeric(as.character(ds$tid.from))
    ds <- ds[order(ds$tid.from), ]
    ds$group <- as.character(ds$group)

    # Join with activity data, fill missing values, and validate
    ds <- plyr::join(ds, di, by = c("tid.from", "group"))
    colnames(ds) <- c("tid", "variable", "value")
    ds$value[is.na(ds$value)] <- 0
    ds <- validate_data_acts(ds)
    return(ds)
  }

  # Helper function to obtain proportion table
  get_table_proportion <- function(di) {
    # @title Obtain Proportion Table for Activity Groups
    # @description Creates a table summarizing the proportion of each activity
    #   group for each time step.
    # @param di Data frame containing activity data.
    # @return A data frame with columns for time step (tid), activity variable,
    #   and proportion.

    # Obtain activity group data
    df <- obtain_activity_group()

    # Reshape data to wide format and calculate proportions
    de <- di %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      tibble::column_to_rownames(var = "tid") %>%
      as.data.frame()

    # Obtain relative totals
    de <- de %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ . / rowSums(de, na.rm = TRUE)))

    # Add time step ID and reshape back to long format
    de <- de %>%
      tibble::rownames_to_column(var = "tid") %>%
      tidyr::pivot_longer(cols = -tid, names_to = "variable", values_to = "value") %>%
      dplyr::mutate(tid = as.numeric(tid)) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::arrange(tid)

    # Create grid of time steps and activity groups
    do <- expand.grid(
      tid = as.numeric(unique(de$tid)),
      variable = as.character(unique(df$name))
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(variable = as.character(variable))

    # Join with proportions, fill missing values, and rescale
    do <- do %>%
      dplyr::left_join(de, by = c("tid", "variable")) %>%
      dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>%
      rescale_data()

    return(do)
  }

  # Helper function to obtain transition probability table
  get_table_probability <- function(activity_logs, activity_groups) {
    # @title Obtain Transition Probability Table for Activity Groups
    # @description Creates a table summarizing the transition probabilities
    #   between activity groups.
    # @param activity_logs Data frame containing activity logs (time series
    #   data).
    # @param activity_groups Data frame containing activity group definitions.
    # @return A data frame with columns for source time step (tid), target time
    #   step (next_tid), activity variable, and transition probability.

    # Obtain activity groups, reshape data, and calculate transition probabilities
    df <- obtain_activity_group()

    # Reshape data to wide format
    dtm <- activity_logs %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      tibble::column_to_rownames(var = "tid") %>%
      as.data.frame() %>%
      as.matrix()

    # Process grid as matrix
    dtmA <- dtmB <- dtm
    dtmA <- sweep(dtmA, 1, rowSums(dtmA, na.rm = TRUE), FUN = "/")
    dtmA <- dtmA[c(dim(dtmA)[1], 1:dim(dtmA)[1] - 1), ]

    dtmB <- t(apply(dtmB, 1, function(x) x / colSums(dtmB, na.rm = TRUE)))

    # Remove rows with missing values
    dtmA[is.na(dtmA)] <- 0
    dtmB[is.na(dtmB)] <- 0

    dtm <- dtmA * dtmB
    dtm <- sweep(dtm, 1, rowSums(dtm, na.rm = TRUE), FUN = "/")
    dtm <- as.data.frame(dtm)
    dtm$tid <- rownames(dtm)

    dtm <- dtm %>%
      tidyr::pivot_longer(cols = -tid, names_to = "variable", values_to = "value") %>%
      dplyr::mutate(tid = as.numeric(tid)) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::arrange(tid)

    # Create grid of time steps and activity groups
    do <- expand.grid(
      tid = as.numeric(unique(activity_logs$tid)),
      variable = as.character(unique(df$name))
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(variable = as.character(variable))

    # Join with probabilities, fill missing values, and return
    do <- do %>%
      dplyr::left_join(dtm, by = c("tid", "variable")) %>%
      dplyr::mutate(value = ifelse(is.na(value), 0, value))

    return(do)
  }

  # Helper function to check and adjust data steadiness
  check_steadiness <- function(df) {
    # @title Check and Adjust Data Steadiness
    # @description Ensures data steadiness by handling potential zero-value
    #   rows.
    # @param df The input data frame to be checked and adjusted.
    # @return A modified data frame with adjusted values for potential
    #   zero-value rows.

    k_labels <- rownames(df)

    # Identify zero rows
    d_zeros <- rowSums(df, na.rm = TRUE)
    d_roll <- rbind(df, df, df)
    d_zeros <- c(d_zeros, d_zeros, d_zeros)
    rownames(d_roll) <- NULL

    # Initialize zero rows
    d_ends <- ifelse(d_zeros[1] == 0 & d_zeros[length(d_zeros)] == 0, T, F)

    # Handle zero rows (1)
    k_sequence <- (dim(df)[1] + 1):(dim(df)[1] * 2)
    for (i in k_sequence) {
      if (d_zeros[i] == 0) {
        d_roll[i, ] <- d_roll[i - 1, ]
      }
    }
    df <- d_roll[k_sequence, ]

    # Handle zero rows (2)
    if (d_ends == TRUE) {
      df[1, ] <- (df[2, ] + df[length(k_labels) - 1, ]) / 2
    }

    # Restore original row names
    rownames(df) <- k_labels

    return(df)
  }


  # Obtain table of activity groups
  lbl_acts <- obtain_activity_group()

  # Initialize list of processed tables and matrices
  l_out <- list()

  # Adjust factors to scale time-slot resolution
  if (varSlot == "1hour" | varSlot == "60min") {
    adj.a <- 1
    adj.b <- 60
  } else if (varSlot == "10min") {
    adj.a <- 10
    adj.b <- 6
  } else if (varSlot == "30min") {
    adj.a <- 2
    adj.b <- 30
  } else {
    adj.a <- 1
    adj.b <- 60
    message("Please check requested time-slot... 1 hour assigned")
  }

  # Load reference table for time slots, and adjust requested resolution
  dtm_slot <- generate_timeslot_table()
  dtm_slot.cols <- colnames(dtm_slot)
  dtm_slot$tid.from.num <-
    strtoi(as.difftime(dtm_slot$tid.from,
      format = "%H:%M",
      units = "mins"
    )) / adj.b
  dtm_slot$tid.from.num <- floor(dtm_slot$tid.from.num) / adj.a

  # Process data based on given subset
  dtaSubset <-
    generate_and_summarise_subset(dtaSource, tblActs, dtm_slot, adj.a, adj.b)

  # Subset data by considering inside conditions only
  dtaSubset_inOnly <- dtaSubset[!dtaSubset$group == "out", ]


  if (dim(dtaSubset)[1] > 1) {
    # prepare data to display number of occurrences
    l_out[[1]] <- get_table_ocurrence(dtm_slot, dtaSubset)

    # prepare data to display proportion of occurrences
    l_out[[2]] <- get_table_proportion(l_out[[1]])

    # prepare data to display probability based on previous state
    l_out[[3]] <- get_table_probability(l_out[[2]], dtaSubset)

    # convert tables to matrices
    l_out[[4]] <- convert_table_to_matrix(l_out[[2]])
    l_out[[5]] <- convert_table_to_matrix(l_out[[3]])
    l_out[[5]] <- check_steadiness(l_out[[5]])

    d_transition <- l_out[[5]]
    d_transition$tid <- row.names(d_transition)
    d_transition <- d_transition %>%
      tidyr::pivot_longer(cols = -tid, names_to = "variable", values_to = "value")
    l_out[[3]] <- d_transition

    # indoor subset only

    # prepare data to display number of occurrences
    l_out[[6]] <- get_table_ocurrence(dtm_slot, dtaSubset_inOnly)

    # prepare data to display proportion of occurrences
    l_out[[7]] <- get_table_proportion(l_out[[6]])

    # prepare data to display probability based on previous state
    l_out[[8]] <- get_table_probability(l_out[[7]], dtaSubset_inOnly)

    # convert tables to matrices
    l_out[[9]] <- convert_table_to_matrix(l_out[[7]])
    l_out[[10]] <- convert_table_to_matrix(l_out[[8]])
    l_out[[10]] <- check_steadiness(l_out[[10]])

    d_transition <- l_out[[10]]
    d_transition$tid <- row.names(d_transition)
    d_transition <- d_transition %>%
      tidyr::pivot_longer(cols = -tid, names_to = "variable", values_to = "value")
    l_out[[8]] <- d_transition

    lblOutData <- c(
      "ocurrences",
      "ocurrences_probability",
      "ocurrences_probability_pre_state",
      "matrix_transitions",
      "matrix_transitions_pre_state"
    )
    names(l_out) <- c(lblOutData, paste0("insideOnly_", lblOutData))
  } else {
    l_out <- "empty or invalid data"
  }

  return(l_out)
}


export_transition_objects <- function(pathId, dtaExport, timeStep, pathToSave) {
  #' @title Export Transition Objects
  #' @description Exports transition tables and matrices to CSV files based on
  #'   the provided data and time step.
  #'
  #' @param pathId Character string specifying the path identifier. If "normal",
  #'   no path identifier is used.
  #' @param dtaExport List containing the data frames and matrices to be
  #'   exported.
  #' @param timeStep Character string specifying the time step (e.g., "1hour",
  #'   "10min").
  #' @param pathToSave Character string specifying the directory path where the
  #'   files will be saved.
  #'
  #' @return None. The function writes CSV files to the specified directory.

  # Check if pathId is "normal" and adjust accordingly
  if (pathId == "normal") {
    pathId <- ""
  } else {
    dtaExport <- dtaExport[[pathId]]
  }

  # Helper function to save data frames to CSV
  expand_table <- function(d_time, d_name) {
    # @title Expand Data Frame into Full Combinations
    # @description Creates a data frame containing all possible combinations of
    #   time steps and variables.
    # @param d_time A vector of time steps.
    # @param d_name A data frame containing variable names.
    # @return A data frame with columns for time step (tid) and variable name.

    d_out <- expand.grid(d_time, d_name$name)
    colnames(d_out) <- c("tid", "variable")
    d_out$variable <- as.character(d_out$variable)
    d_out$tid <- as.factor(d_out$tid)
    return(d_out)
  }

  # Helper function to standardize the data frame
  standardise_table <- function(transition_data, structure_data) {
    # @title Standardize Data Frame
    # @description Standardizes a data frame by joining with a structure data
    #   frame and adjusting variable levels.
    # @param transition_data The input data frame to be standardized.
    # @param structure_data A reference data frame containing structure
    #   information.
    # @return A standardized data frame.

    # Join data with structure information
    standardized_data <- plyr::join(transition_data, structure_data, by = c("tid", "variable"))

    # Adjust variable levels
    standardized_data$variable <- as.factor(standardized_data$variable)
    levels(standardized_data$variable) <- gsub("\\.", "-", levels(standardized_data$variable))
    standardized_data$variable <- factor(standardized_data$variable, levels = s_tus$activities_groups$name)

    # Sort data by variable
    standardized_data <- standardized_data[order(standardized_data$variable), ]

    return(standardized_data)
  }

  # Helper function to generate transition table in long format
  generate_transition_long_table <- function(dfs, step) {
    # @title Generate Transition Table in Long Format
    # @description Creates a data frame summarizing transitions between activity
    #   groups across time steps.
    # @param dfs A data frame containing transition data (potentially from
    #   multiple sources).
    # @param step Time step (e.g., day, hour) for constructing the time slot.
    # @return A data frame with columns for source time step (tid), target time
    #   step (next_tid), activity variable, and transition count (value).

    # Preprocess data frame
    colnames(dfs) <- c("tid", "variable", "value")
    dfs$variable <- as.character(dfs$variable)
    dfs <- dfs[!is.na(dfs$variable), ]

    # Generate time slots and activity group labels
    dtm_slot <- generate_timeslot(step)
    lbl_acts <- obtain_activity_group()

    # Create empty table with all possible combinations
    dot <- expand_table(dtm_slot, lbl_acts)

    # Standardize table and fill missing values
    dot <- standardise_table(dot, dfs)
    dot$value[is.na(dot$value)] <- 0
    return(dot)
  }

  # Helper function to generate transition matrix
  generate_transition_matrix <- function(dfs, step) {
    # @title Generate Transition Matrix
    # @description Creates a transition matrix summarizing the counts of
    #   transitions between activity groups across time steps.
    # @param dfs A data frame containing transition data (potentially from
    #   multiple sources).
    # @param step Time step (e.g., day, hour) for constructing the time slot.
    # @return A transition matrix with rows and columns representing activity
    #   groups, and values representing transition counts between them.

    # Preprocess data frame
    dtaMatrix.tid <- rownames(dfs)
    dtaMatrix <- dfs %>%
      dplyr::mutate(tid = dtaMatrix.tid) %>%
      tidyr::pivot_longer(cols = -tid, names_to = "variable", values_to = "value") %>%
      dplyr::filter(!is.na(variable)) %>%
      dplyr::mutate(
        variable = as.character(variable),
        tid = as.numeric(tid)
      )

    # Generate time slots and activity group labels
    dtm_slot <- generate_timeslot(step)
    lbl_acts <- obtain_activity_group()

    # Create empty table with all possible combinations
    dot <- expand_table(dtm_slot, lbl_acts)

    # Standardize table, fill missing values, and reshape to wide format
    dot <- standardise_table(dot, dtaMatrix)
    dot$value[is.na(dot$value)] <- 0
    dot <- dot %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::select(-tid)

    return(dot)
  }


  # Define/locate folder
  pathToSave <- paste(pathToSave, pathId, sep = "/")

  # Check if the tables have been estimated
  var.existence <- paste0(timeStep, "_MtrxTimeDpndnt")
  var.existence <-
    list.files(path = pathToSave, pattern = var.existence, ignore.case = TRUE)

  # Check if tables are not empty or present in folder already
  if (is.list(dtaExport) & length(var.existence) == 0) {
    # Generate transition data
    da <- generate_transition_long_table(dtaExport$ocurrences, timeStep)
    db <- generate_transition_long_table(dtaExport$ocurrences_probability, timeStep)
    dc <- generate_transition_long_table(dtaExport$ocurrences_probability_pre_state, timeStep)
    dd <- generate_transition_matrix(dtaExport$matrix_transitions, timeStep)
    de <- generate_transition_matrix(dtaExport$matrix_transitions_pre_state, timeStep)
    df <- generate_transition_long_table(dtaExport$insideOnly_ocurrences, timeStep)
    dg <- generate_transition_long_table(dtaExport$insideOnly_ocurrences_probability, timeStep)
    dh <- generate_transition_long_table(dtaExport$insideOnly_ocurrences_probability_pre_state, timeStep)
    di <- generate_transition_matrix(dtaExport$insideOnly_matrix_transitions, timeStep)
    dj <- generate_transition_matrix(dtaExport$insideOnly_matrix_transitions_pre_state, timeStep)

    # Combine tables
    d_occurences <- da %>%
      tibble::tibble() %>%
      dplyr::mutate(prob = db$value, prob_pre_state = dc$value)
    d_occurences_pre <- df %>%
      tibble::tibble() %>%
      dplyr::mutate(prob = dg$value, prob_pre_state = dh$value)


    # Export data

    dir.create(pathToSave, recursive = TRUE, showWarnings = FALSE)

    fileExport <- file.path(pathToSave, paste0("/a_", timeStep, "_ocurrences.csv"))
    write.csv(d_occurences, fileExport)

    fileExport <- file.path(pathToSave, paste0("/d_", timeStep, "_matrix_transitions.csv"))
    write.csv(dd, fileExport)

    fileExport <- file.path(pathToSave, paste0("/e_", timeStep, "_matrix_transitions_pre_state.csv"))
    write.csv(de, fileExport)

    fileExport <- file.path(pathToSave, paste0("/f_", timeStep, "_ocurrences.csv"))
    write.csv(d_occurences_pre, fileExport)

    fileExport <- file.path(pathToSave, paste0("/i_", timeStep, "_matrix_transitions.csv"))
    write.csv(di, fileExport)

    fileExport <- file.path(pathToSave, paste0("/j_", timeStep, "_matrix_transitions_pre_state.csv"))
    write.csv(dj, fileExport)
  } else if (length(var.existence) > 0) {
    # If tables already exist
    message(cat(paste("Tables already exist \u2192", pathToSave)))
  } else {
    # If no data found
    message(cat(paste("No data found here \u2192", pathToSave)))
  }
}


# .. perform Markov Chain simulation -------------------------------------------

obtain_occupants_presence <- function(df_cmp, df_sum, df_inf, df_ind, df_dry, df_occ, k_sel, k_met, path_proj = the$.enhub.paths) {
  #' @title Obtains occupant presence data
  #'
  #' @description Extracts household characteristics, matches a profile,
  #'   generates presence matrices, and merges the resulting presence data for
  #'   all occupants.
  #'
  #' @param df_cmp A data frame containing comprehensive household data.
  #' @param df_sum A data frame containing summary household data.
  #' @param df_inf A data frame containing individual information.
  #' @param df_ind A data frame containing individual data.
  #' @param df_dry A data frame containing diary data.
  #' @param df_occ A data frame containing occupant information.
  #' @param k_sel A character vector specifying the selection criteria.
  #' @param k_met A character vector specifying the method.
  #' @param path_proj The path to the project directory (default:
  #'   the$.enhub.paths).
  #' @return A data frame containing presence data for all occupants.
  
  # Helper function to match a user profile to reference data
  match_profile <- function(tblSel, varRes, lstRef = s_tus_ehs) {
    # @title Match a user profile to reference data
    # @description This function matches a user profile (`tblSel`) defined by
    #   variables to the closest reference data (`lstRef`). The function performs
    #   case-insensitive matching on selected variables (tenure, type, income,
    #   hhtype, region, employment).
    #
    # @param tblSel A data frame containing the user profile.
    # @param varRes A variable related to the response. (Currently unused)
    # @param lstRef A list containing reference data (default: s_tus_ehs).
    # @return A list containing the user's used variables and close matches from
    #   the reference data.

    # Remove rows with missing values
    tblSel <- tblSel %>%
      tibble::tibble() %>%
      dplyr::select(dplyr::where(~ !all(is.na(.x))))
    # tblSel <- tblSel %>% dplyr::select_if(~ !any(is.na(.)))

    # Identify relevant columns in user profile
    lblCols <- c("tenure", "type", "income", "hhtype", "region", "employment")
    lstCols <- unlist(lapply(lblCols, function(x) {
      ifelse(
        any(grepl(x, colnames(tblSel))), TRUE, FALSE
      )
    }))
    names(lstCols) <- lblCols
    lblCols <- names(lstCols[lstCols == TRUE])
    (tblSel <- tblSel[, lblCols])

    optAge <- c("-", "adult", "kids", "young")

    # Prepare tblName for matching
    tblName <- tblSel %>%
      as.matrix() %>%
      as.character() %>%
      tolower()

    # Convert user profile to lowercase characters
    tblName <- gsub(" ", "-", (tolower(paste(tblName, collapse = "__"))))
    tblName <- paste0("cross__", gsub("[[:punct:]]", "_", tblName))

    varSearch <- tblSel %>%
      as.matrix() %>%
      as.character() %>%
      tolower()

    # Prepare reference data
    colnames(lstRef$TUS) <- gsub(".cross.", "", colnames(lstRef$TUS))

    m_events <- tblEvents <- lstRef$TUS
    m_events$serial <- m_events$age <- NULL
    m_events <- m_events %>%
      tibble::tibble() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) %>%
      dplyr::select(dplyr::all_of(colnames(tblSel))) %>%
      as.matrix()
    m_events[is.na(m_events)] <- "-"

    # Find matching rows in reference data
    varMatch <- NULL
    for (iter in rev(2:length(varSearch))) {
      varMatch <- apply(
        m_events[, 1:iter], 1,
        function(x) all(x == varSearch[1:iter])
      )
      if (any(varMatch)) break
    }

    # Analyze matching results
    if (any(varMatch)) {
      tblEvents$match <- varMatch
      varEffvMatch <- tblSel[, 1:iter]
      varNearMatch <- subset(tblEvents, match == TRUE)
      varNearMatch$match <- NULL
    } else {
      varNearMatch <- varEffvMatch <- "-"
      message("No transition matrix found!") # "no match with these conditions"
    }

    # Return results
    lstMatch <- list(used_variables = varEffvMatch, close_matches = varNearMatch)

    return(lstMatch)
  }

  # Helper function to estimate household presence
  estimate_household_presence <- function(d_activity, step) {
    # @title Estimates household presence over time
    #
    # @description Converts activity data into a detailed format with timestamps,
    #   presence status, and other relevant information.
    #
    # @param d_activity The input data frame containing household activity data.
    # @param step The time step for presence estimation (e.g., "5 min").
    #
    # @return A data frame with detailed presence information for each household
    #   member.

    # Get simulation year
    varYr <- the$GLOBAL_PAR$simulation_year

    # Prepare data for analysis
    rownames(d_activity) <- NULL
    d_activity$idx <- as.integer(rownames(d_activity))
    d_activity_melt <- d_activity %>%
      tidyr::pivot_longer(cols = -c(tid, day, idx), names_to = "variable", values_to = "value") %>%
      dplyr::mutate(presence = as.factor(ifelse(value == "out", "out",
        ifelse(value %in% c("sleep", "passive"), "passive", "active")
      )))

    # Add timestamps and extract relevant information
    d_activity_melt <- d_activity_melt %>%
      dplyr::mutate(
        timestamp = seq(
          from = ISOdate(varYr, 1, 1, 0, 0),
          by = gsub("min", " min", step),
          length.out = dplyr::n()
        )
      ) %>%
      dplyr::mutate(months = lubridate::month(timestamp))

    # Select, convert to tibble and return
    d_activity_melt <- d_activity_melt %>%
      dplyr::select(timestamp, months, day, tid, idx, variable, value, presence) %>%
      dplyr::rename(month = months) %>%
      tibble::as_tibble()

    return(d_activity_melt)
  }

  # Helper function to generate activity chains
  perform_chains <- function(ppl, days, v_prob) {
    # @title Performs activity chain generation
    #
    # @description Generates activity chains for a specified number of people and
    #   days.
    #
    # @param ppl The number of people.
    # @param days The number of days.
    # @param v_prob The probability matrix for activities.
    # @return A data frame representing the generated activity chains.

    # Obtain activity labels
    lbl_acts <- obtain_activity_group()
    k_labels <- colnames(v_prob)

    iterate_hour <- function(i_hr, lbl = k_labels) {
      d_out <- sample(lbl, 1, replace = TRUE, prob = i_hr)
      return(d_out)
    }

    iterate_days <- function(i_dy, df) {
      d_out <- as.character(apply(df, 1, iterate_hour))
      return(d_out)
    }

    # Generate activity chains
    d_chain <- lapply(1:days, iterate_days, v_prob)
    d_chain <- plyr::ldply(d_chain, data.frame)

    # Format and return the output
    colnames(d_chain) <- "Events"
    d_chain$Events <- as.character(d_chain$Events)
    d_chain$Events <- factor(d_chain$Events, levels = lbl_acts$name)
    colnames(d_chain) <- ppl

    return(d_chain)
  }

  # Helper function to generate events
  generate_events <- function(matrixPath, method, peopleCount, daysCount) {
    # @title Generates events based on transition matrix and simulation
    #   parameters
    #
    # @description Simulates events for a specified number of people and days
    #   using a transition matrix.
    #
    # @param matrixPath The path to the CSV file containing the transition
    #   matrix.
    # @param method The event simulation method (implementation not provided).
    # @param peopleCount The number of people to simulate events for.
    # @param daysCount The number of days to simulate events for.
    # @return A data frame containing simulated events with person ID, day, and
    #   activity.

    # Load pre-defined matrix of transition
    dtaToSim <- read.csv(matrixPath, header = TRUE, row.names = 1)
    lblTid <- rownames(dtaToSim)

    # Simulate Markov-chain of events using specified method
    d_res <- as.data.frame(lapply(1:peopleCount, perform_chains, daysCount, dtaToSim))
    d_res$tid <- as.integer(lblTid)
    d_res$day <- rep(1:daysCount, each = length(lblTid))
    d_res <- tibble::tibble(d_res)

    invisible(d_res)
  }

  # Helper function to obtain presence data
  obtain_presence_per_member <- function(i_o, tblNum, m_tus_events, varRes = var.restep, varMethod = var.method) {
    # @title Obtain presence probabilities for household members
    # @description This function retrieves and processes presence probability
    #   data for a specific household member or member group.
    #
    # @param i_o The household member identifier or group name (e.g., 'adult',
    #   'kids').
    # @param tblNum A data frame containing household member information (assumed
    #   to have member count in a column).
    # @param m_tus_events A matrix containing presence probability data.
    # @param varRes The temporal resolution of the data (default: var.restep).
    # @param varMethod The method used for generating presence events (default:
    #   var.method).
    #
    # @return A data frame containing presence probabilities for the specified
    #   member(s) or "-" if data is unavailable.

    # Initialize logging messages
    lbl_process <- "Processing profiles for..."
    lbl_res <- "Resolution ... @ 60 min"

    # Extract number of occupants
    n_occ <- as.numeric(tblNum[i_o])

    # Standardize member identifier
    i_o <- ifelse(i_o == "child" | i_o == "kids", "kids", i_o)
    i_o <- ifelse(i_o == "generic" | i_o == "gnric", "-", i_o)

    # Find matching data based on identifier and resolution
    if (any(grepl(paste0(i_o, "/[[:alpha:]]_", varRes), m_tus_events))) {
      tbl.sel <- paste0(varMatch <- i_o, "/[[:alpha:]]_", varRes)
      mtx.sel <- m_tus_events[grep(tbl.sel, m_tus_events)]
    } else {
      tbl.sel <- paste0(varMatch <- "-", "/[[:alpha:]]_", varRes)
      mtx.sel <- m_tus_events[grep(tbl.sel, m_tus_events)]
    }

    # Process data only if occupants exist, data is available, and data is not empty
    if (n_occ > 0 & !is.na(n_occ) & !identical(mtx.sel, character(0))) {
      # Define presence labels for output
      lbl_prsnt <- c("timestamp", sprintf("%s_%d", i_o, 1:n_occ))

      # Read presence probability data
      df_prob <- read.csv(mtx.sel, header = TRUE, row.names = 1)

      # Generate presence events
      m_events <- suppressMessages(
        generate_events(mtx.sel, varMethod, n_occ, 365)
      )

      # Estimate household presence
      m_events_melt <- estimate_household_presence(m_events, varRes)

      # Prepare presence data for aggregation
      dfop <- subset(m_events_melt,
        select = c(variable, timestamp, month, day, presence)
      )

      # Append time parameters
      dfop$presence <- ifelse(dfop$presence == "passive", 0.5,
        ifelse(dfop$presence == "out", 0, 1)
      )
      dfop$hour <- lubridate::hour(dfop$timestamp)
      dfop$min <- lubridate::minute(dfop$timestamp)
      dfop$day <- format(dfop$timestamp, "%d")
      dfop$timestamp <- NULL

      varYr <- the$GLOBAL_PAR$simulation_year

      if (sd(dfop$min) == 0 & dim(dfop)[1] > 8000) {
        dfop <- dfop %>%
          dplyr::select(variable, month, day, hour, presence)
      } else {
        dfop <- dfop %>%
          dplyr::group_by(variable, month, day, hour) %>%
          dplyr::summarise(presence = round(mean(presence), 2)) %>%
          dplyr::ungroup()
      }

      # Aggregate presence probabilities by time window
      dfop <- dfop %>%
        dplyr::mutate(timestamp = paste0(
          paste(varYr, month, day, sep = "-"),
          " ", dfop$hour, ":00"
        )) %>%
        dplyr::select(timestamp, presence, variable)

      dfop <- dfop %>%
        tidyr::pivot_wider(names_from = variable, values_from = presence, values_fn = list(presence = mean)) %>%
        dplyr::mutate(timestamp = lubridate::ymd_hm(timestamp)) %>%
        dplyr::arrange(timestamp)

      # Rename columns according to presence labels
      colnames(dfop) <- lbl_prsnt
    } else {
      dfop <- "---"
    }

    return(dfop)
  }

  # Helper function to merge presence data
  merge_presence_by_people <- function(l_p, df_time = the$l_days$period) {
    # @title Merges presence data for multiple people
    #
    # @description Combines multiple presence data frames into a single data
    #   frame by timestamp.
    #
    # @param l_p A list of data frames, each representing presence data for a
    #   person.
    # @param df_time A data frame containing a timestamp column (default:
    #   the$l_days$period).
    # @return A merged data frame with presence data for all people and a
    #   timestamp column.

    l_p <- l_p[lapply(l_p, length) > 1]
    l_p <- plyr::join_all(l_p, by = "timestamp", match = "first")
    l_p$timestamp <- df_time$Hour
    names(l_p)[names(l_p) == "timestamp"] <- "Hour"
    l_p <- tibble::tibble(l_p)
    return(l_p)
  }

  # Helper to export transition matrices
  export_transition_single <- function(l_sets, l_data, fac, res = "60min", age = FALSE) {
    # @title Export Single Household Transition Data
    # @description Exports transition data for a single household.
    #
    # @param l_sets A character string specifying the household ID.
    # @param l_data A data frame or list of data frames containing household
    #   data.
    # @param fac A character string specifying the variable used for creating the
    #   output folder.
    # @param res A character string specifying the time resolution (e.g.,
    #   "60min").
    # @param age A logical indicating whether to process data by age group
    #   (currently not used).
    # @return NULL

    # Create output path
    pathToExport <- file.path(the$.enhub.paths$store$household, fac, l_sets)

    # Filter data based on age selector
    if (age == FALSE) {
      d_factor <- l_data
    } else {
      d_factor <- l_data[[l_sets]]
    }

    # Prepare data for transition matrix
    d_states <- obtain_transition_tables(d_factor, res)

    # Export transition objects
    if (is.list(d_states)) {
      export_transition_objects("normal", d_states, res, pathToExport)
    } else {
      message(paste(d_states, pathToExport, sep = " // "))
    }

    # Return null and print export message
    message(paste("Exported to :", pathToExport))
    return(NULL)
  }

  # Helper to obtain activities by household
  obtain_activities_by_household <- function(valHhd,
                                             d_indiv = s_tus$tbl_individual,
                                             d_diary = s_tus$tbl_diary,
                                             k_extra = FALSE,
                                             per_age = s_tus$age_filter) {
    # @title Obtain Activities for a Subset of Individuals
    # @description Selects activities performed by a subset of individuals based
    #   on specific criteria.
    #
    # @param valHhd A vector of household identifiers to filter individuals.
    # @param d_indiv Reference dataset containing individual information
    #   (default: s_tus$tbl_individual).
    # @param d_diary Reference dataset containing diary data (default:
    #   s_tus$tbl_diary).
    # @param k_extra A logical indicating whether to add extra factors based on
    #   age categories (default: FALSE).
    # @param per_age A character vector specifying age category labels (used only
    #   if k_extra is TRUE, default: s_tus$age_filter).
    #
    # @return A data.frame containing activity information for the selected
    #   individuals, potentially with additional age category information if
    #   k_extra is TRUE.

    # Filter individual data
    d_res <- d_diary[d_diary$serial %in% valHhd, ]

    # Filter diary data and handle missing data
    if (dim(d_res)[1] < 1) {
      message("Warning: No exact match could be found in subset")
      valHhd <- gsub(".{1}$", "", as.character(valHhd))
      d_res <- d_diary[grep(
        paste0("(", paste(valHhd, collapse = "|"), ")"),
        d_diary$serial
      ), ]
    }

    # Add age category information (if requested)
    if (k_extra == TRUE) {
      d_res$ExtraFactor <-
        factor(ifelse(d_res$DVAge < 17, ifelse(d_res$DVAge < 10, 1, 2), 3),
          levels = 1:3, labels = per_age
        )
      lstSubset <- list()
      for (i in 1:3) {
        lstSubset[[i]] <- subset(d_res, ExtraFactor == per_age[i])
        lstSubset[[i]]$ExtraFactor <- NULL
      }
      names(lstSubset) <- per_age
      d_res <- lstSubset
    }

    return(d_res)
  }

  # Helper to generate transition matrices from matched profiles
  generate_matrix_from_match <- function(tblMatches, tblInputs, varResol,
                                         d_indiv = s_tus$tbl_individual,
                                         d_diary = s_tus$tbl_diary) {
    # @title Generate transition matrices from matched profiles
    # @description This function generates transition matrices based on matched
    #   profiles and household activity data. It iterates over matched household
    #   IDs and age factors (adult or all-ages) to extract relevant activity data
    #   and create separate matrices.
    #
    # @param tblMatches A data frame containing matched profiles (default: output
    #   from match_profile).
    # @param tblInputs A data frame containing the user's input data.
    # @param varResol A variable related to the response (currently unused).
    # @param d_indiv A data frame containing individual data (default:
    #   s_tus$tbl_individual).
    # @param d_diary A data frame containing diary data (default:
    #   s_tus$tbl_diary).
    #
    # @return A string representing the path to the generated transition matrix.

    # Extract household IDs from matches
    tblName <- as.character(tblInputs)
    tblName <- gsub(" ", "-", (tolower(paste(tblName, collapse = "__"))))
    tblName <- paste0("sel__", gsub("[[:punct:]]", "_", tblName))

    var.hhd <- tblMatches$close_matches$serial

    # Process households with valid IDs in individual data
    if (length(var.hhd) > 0 &
      (ifelse(identical(var.hhd, character(0)), "", var.hhd) %in% d_indiv$serial)) {
      for (ageFactor in c(TRUE, FALSE)) {
        # Obtain household activity data
        dtaHousehold <-
          obtain_activities_by_household(var.hhd, d_indiv, d_diary, ageFactor)

        # Check for valid data and extract activity variables
        if (is.data.frame(dtaHousehold) | is.list(dtaHousehold)) {
          if (ageFactor == TRUE) {
            var.factors <-
              names(dtaHousehold)[c(
                dim(dtaHousehold$kids)[1] > 0,
                dim(dtaHousehold$young)[1] > 0,
                dim(dtaHousehold$adult)[1] > 0
              )]
          } else {
            var.factors <- "-"
          }

          # Generate transition matrices for each activity variable
          suppressMessages(invisible(
            lapply(
              var.factors, export_transition_single, dtaHousehold,
              tblName, varResol, ageFactor
            )
          ))
        }
      }
    }

    # Construct the path to the transition matrix
    tblName <- file.path(the$.enhub.paths$store$household, tblName)
    return(tblName)
  }


  # Extract household characteristics
  (d_selection <- data.frame(
    income = obtain_income_bands(df_cmp$V525_IncomeCombined / 12),
    tenure = obtain_tenure_bands(df_cmp$V512_Tenure_8x),
    type = obtain_typology_bands(df_sum$.hubtyp),
    hhtype = obtain_household_bands(df_cmp$V516_HouseholdComposition),
    region = obtain_region_bands(df_inf$D003_Region),
    employment = obtain_employment_bands(df_cmp$V521_HRPEmployment)
  ))

  # Match household profile
  (d_match <- match_profile(d_selection, k_sel))

  # Generate presence matrices for each occupant
  m_match <- generate_matrix_from_match(d_match, d_selection, k_sel,
    d_indiv = df_ind, d_diary = df_dry
  )

  # Obtain presence data for each occupant
  l_nearest <- list.files(
    path = m_match, pattern = "^e.*.csv",
    recursive = TRUE, full.names = TRUE
  )

  # Merge presence data
  d_res <- lapply(
    names(df_occ), obtain_presence_per_member,
    df_occ, l_nearest, k_sel, k_met
  )
  d_res <- merge_presence_by_people(d_res)

  return(d_res)
}
