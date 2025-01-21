
get_input_set <- function(data_frame = combine_ehs()) {
  #' @title Creates a List of Variables for Subsetting EHS Data
  #' @description This function identifies potential variables for subsetting EHS data based on their types (categorical, numeric, logical).
  #' It retrieves available levels for categorical variables, lists possible values for reference variables,
  #' and provides summary statistics for numeric variables.
  #'
  #' @param data_frame A data frame containing EHS data (defaults to the output of `combine_ehs()`).
  #' @return A list containing information on potential subsetting variables.

  # Identify numeric and logical variables
  numeric_vars <- data_frame %>% dplyr::select(where(is.numeric))
  logical_vars <- data_frame %>% dplyr::select(where(is.logical))

  # Retrieve reference data for categorical subsetting variables
  heating_system_types <- s_chm$CHM_MainHeatingSystemType$type
  dhw_system_types <- s_chm$CHM_HotDHWSystem$dhw
  age_bands <- s_chm$CHM_EHS_ageBand$ehs_period

  # Summarize numeric variables
  l_sum_numeric <- lapply(colnames(numeric_vars), function(x) {
    c(
      mean = mean(pull(numeric_vars[,x]), na.rm = TRUE),
      sd = sd(pull(numeric_vars[,x]), na.rm = TRUE),
      min = min(pull(numeric_vars[,x]), na.rm = TRUE),
      max = max(pull(numeric_vars[,x]), na.rm = TRUE),
      median = median(pull(numeric_vars[,x]), na.rm = TRUE),
      n = sum(!is.na(pull(numeric_vars[,x])))
    )
  })
  names(l_sum_numeric) <- colnames(numeric_vars)
  
  # Initialize list for variable information
  l_set <- list(
    D002_DwellingType = levels(data_frame$D002_DwellingType),
    D003_Region = levels(data_frame$D003_Region),
    D001_DwellingAge = age_bands,
    D004_TenureType = levels(data_frame$D004_TenureType),
    V573_Urbanity = levels(data_frame$V573_Urbanity),
    V542_FloorPosition = levels(data_frame$V542_FloorPosition),
    D082_MainHeatingSystemType = heating_system_types,
    D097_DHWSystemType = dhw_system_types,
    binary.variables = colnames(logical_vars),
    numeric.variables = l_sum_numeric
  )

  # Return the list invisibly (avoiding printing)
  invisible(l_set)
}

combine_ehs <- function(l_set = s_ehs_2011_ext) {
  #' @title Combines EHS Data Sets into a Single Data Frame
  #' @description This function merges various EHS data sets (assumed to be part of a larger list) into a single data frame.
  #' It also performs some data cleaning and transformation steps.
  #'
  #' @param l_set A list containing EHS data sets (e.g., dwelling information, ventilation, etc.).
  #' @return A data frame containing the combined EHS data.

  # Identify active EHS data set, and join relevant data sets based on housing code
  d_set <- l_set$dwelling.and.household.information %>%
    plyr::join(l_set$geometry, by = "V001_HousingCode") %>%
    plyr::join(l_set$ventilation, by = "V001_HousingCode") %>%
    plyr::join(l_set$other.heat.loss.elements, by = "V001_HousingCode") %>%
    plyr::join(l_set$space.heating, by = "V001_HousingCode") %>%
    plyr::join(l_set$hot.water.system, by = "V001_HousingCode") %>%
    plyr::join(l_set$low.energy.lighting, by = "V001_HousingCode") %>%
    plyr::join(l_set$complementary, by = "V001_HousingCode") %>%
    plyr::join(l_set$summarised, by = "V001_HousingCode") %>%
    # Create derived variables: region group, age band, and heating system ID
    dplyr::mutate(.region_red = ifelse(grepl("North|York", D003_Region), "North",
      ifelse(grepl("Midland", D003_Region), "Central", "South")
    )) %>%
    dplyr::mutate(.ageband_red = ifelse(grepl("pre", .hubage), "Heritage",
      ifelse(grepl("war", .hubage), "PreIndustrial", "PostIndustrial")
    )) %>%
    dplyr::mutate(.heatid = paste0("MSH", D082_MainHeatingSystemType, "DHW", D097_DHWSystemType)) %>%
    tibble::tibble()

  return(d_set)
}

use_custom_file_subset <- function(file_custom = system.file("extdata", "custom_input.dcf", package = "enhub"), d_ehs = combine_ehs(), k_input = get_input_set()) {
  #' @title Subset EHS data based on an external file with user-defined criteria
  #' 
  #' @description This function allows users to subset EHS data based on user-defined 
  #' criteria stored in an external file (e.g., `custom_input.dcf`, `custom_input.json`).
  #' 
  #' The file should contain a list of variables and their corresponding values for 
  #' subsetting the data. The purpose of this approach is to allow users to define 
  #' custom subsets without modifying the code, with that definition stored in a 
  #' separate file, potentially in the cloud.
  #' 
  #' @details
  #' The file specifies various attributes related to dwellings, regions, tenure types, 
  #' and other survey variables, for example: 
  #'
  #' \describe{
  #'   \item{D002_DwellingType}{detached, semi detached, mid terrace, end terrace, purpose built, converted, non residential}
  #'   \item{D003_Region}{does not apply, no answer, North East, Yorkshire and the Humber, North West, East Midlands, West Midlands, South West, Eastern England, South East, London}
  #'   \item{D004_TenureType}{housing association, local authority, private owner occupied, private rented}
  #'   \item{V573_Urbanity}{City centre, Urban, Suburban residential, Rural residential, Village centre, Rural, Question Not Applicable, Unknown}
  #'   \item{V542_FloorPosition}{House/Bungalow, Top Floor Flat, Mid Floor Flat, Ground floor flat, Basement Flat, Unknown}
  #'   \item{V534_TFAsurvey}{<50, 200-250, >500}
  #'   \item{D001_DwellingAge}{>1,< 11}
  #'   \item{D082_MainHeatingSystemType}{1-15}
  #'   \item{D097_DHWSystemType}{1-11}
  #'   \item{V576_CuboidAttic}{TRUE, FALSE}
  #'   \item{D030_WallThickness}{0.15-0.64}
  #'   \item{V563_GlazingRatio}{0-1}
  #'   \item{D109_SolarDHWSystem}{TRUE, FALSE}
  #' }
  #' 
  #' An example of the provided template (retrievable via `system.file("extdata", "custom_input.dcf", package = "enhub")`) contains the following entries:
  #' ```sh
  #' D002_DwellingType: detached, semi detached, mid terrace, end terrace
  #' D003_Region: Yorkshire and the Humber, North East
  #' D004_TenureType: private owner occupied
  #' V542_FloorPosition: House/Bungalow
  #' V534_TFAsurvey: >150
  #' D001_DwellingAge: < 10
  #' D082_MainHeatingSystemType: 1-15
  #' D097_DHWSystemType: 1-11
  #' V576_CuboidAttic: TRUE
  #' ```
  #'
  #' @param file_custom A file containing user-defined criteria for subsetting EHS data. A default file is provided.
  #' @param d_ehs A data frame containing EHS data (assumed to be combined from multiple sources).
  #' @param k_input A list containing initial indices for subsetting (optional, defaults to all rows).
  #' 
  #' @return A data frame containing the subset of EHS data based on user-defined criteria.

  detect_file_type <- function(file_custom) {
    # Read the first line of the file
    first_line <- readLines(file_custom, n = 1)

    # Check if the first line starts with '{' indicating a JSON file
    if (startsWith(first_line, "{")) return("JSON")
    
    # Check if the first line contains ':' indicating a DCF file
    if (grepl(":", first_line)) return("DCF")

    # If neither condition is met, return unknown
    return("Unknown")
  }

  # Helper function to extract variables from user-defined entries
  obtain_subset_variables <- function(idSubset, lstSubset) {
    l_res <- unlist(strsplit(lstSubset[[idSubset]], split = ", "))
    return(l_res)
  }

  # Helper function to obtain indices for subsetting based on variable values
  obtain_subset_ids <- function(k_sel, l_ref, d_ref, k_vars) {
    d_subset <- l_ref[[k_sel]]

    var.cat <- names(k_vars)[1:8]
    var.bin <- k_vars$binary.variables
    var.num <- names(k_vars$numeric.variables)
    var.special <- c("D001_DwellingAge", "D082_MainHeatingSystemType", "D097_DHWSystemType")

    # Handle special variables, numeric variables, and categorical/binary variables differently
    if (k_sel %in% var.special | k_sel %in% var.num) {
      k_options <- as.integer(unlist(d_ref[, eval(k_sel)]))
      d_subset <- gsub("<", paste0(min(k_options) - 1, "-"), d_subset)
      d_subset <- gsub("([>])+(.*)", "\\2\\1", d_subset)
      d_subset <- gsub(">", paste0("-", max(k_options)), d_subset)
      d_subset <- gsub("\\s", "", d_subset)
      d_subset <- as.character(unlist(strsplit(d_subset, split = ",")))
      d_subset <- paste(paste0("[", d_subset, "]"), collapse = "|")
      d_res <- grep(d_subset, as.character(k_options))
    }

    if (k_sel %in% var.cat | k_sel %in% var.bin) {
      d_res <- grep(
        paste(d_subset, collapse = "|"),
        as.character(unlist(d_ref[, eval(k_sel)]))
      )
    }

    return(d_res)
  }

  
  # Detect the file type
  file_type <- detect_file_type(file_custom)

  if (file_type == "DCF") {
    # Read user-defined variables and values from a DCF file
    l_user_file <- as.list(as.data.frame(read.dcf(file_custom), stringsAsFactors = FALSE))
    l_user_file <- l_user_file[-grep("^#", names(l_user_file))]

  } else if (file_type == "JSON") {
    l_user_file <- jsonlite::fromJSON(file_custom, simplifyDataFrame = TRUE, simplifyVector = TRUE)
  }

  # Extract user-defined variables from each entry in the file
  l_selection <- lapply(names(l_user_file), obtain_subset_variables, l_user_file)
  names(l_selection) <- names(l_user_file)

  # Initialize list of indices for subsetting
  l_indices <- k_input

  # Loop through user-defined variables and obtain matching indices
  l_indices <- lapply(names(l_selection), 
    obtain_subset_ids, l_selection, d_ehs, l_indices)

  # Combine indices using intersection
  l_indices <- Reduce(intersect, l_indices)

  # Subset the EHS data based on combined indices
  d_subset <- subset(combine_ehs()[l_indices, ])

  return(d_subset)
}
