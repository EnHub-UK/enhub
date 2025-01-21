#' @title Data from TUS
#'
#' @description This object contains the data retrieved from the Time Use Survey (TUS) and English Housing Survey (EHS) sources.
#'
#' @format A list containing various datasets related to the Time Use Survey (TUS) and the English Housing Survey (EHS).
#' - EHS: A tibble containing data from the English Housing Survey with 163,304 rows and 7 columns.
#'   - serial: Serial number of the survey entry.
#'   - .cross.income: Income category (low, mid, high).
#'   - .cross.tenure: Tenure type (Private, Social).
#'   - .cross.type: Dwelling type (Apartment, House).
#'   - .cross.region: Region of the dwelling.
#'   - .cross.employment: Employment status (irregular, paid employment, etc.).
#'   - .cross.hhtype: Household type (couple no dependants, etc.).
#' - TUS: A data frame containing data from the Time Use Survey with 7,041 observations and 8 variables.
#'   - serial: Serial number of the survey entry.
#'   - .cross.income: Income category (low, mid, high).
#'   - .cross.tenure: Tenure type (Private, Social).
#'   - .cross.type: Dwelling type (Apartment, House).
#'   - .cross.region: Region of the dwelling.
#'   - .cross.employment: Employment status (irregular, paid employment, etc.).
#'   - .cross.hhtype: Household type (couple no dependants, etc.).
#'   - .cross.age: Age category (adult, child, etc.).
#' - households: A tibble containing household data with 7,041 rows and 8 columns.
#'   - serial: Serial number of the survey entry.
#'   - .cross.income: Income category (high, mid, low).
#'   - .cross.tenure: Tenure type (Private, Social).
#'   - .cross.type: Dwelling type (House, Apartment).
#'   - .cross.region: Region of the dwelling.
#'   - .cross.employment: Employment status (paid employment, irregular, etc.).
#'   - .cross.hhtype: Household type (other type of household, couple no dependants, etc.).
#'   - .cross.age: Age category (adult, young, etc.).
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_tus_ehs
#' }
"s_tus_ehs"
