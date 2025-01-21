#' @title Core Index Data Frame
#' @description This data frame contains core index data with three columns: aacode, weight, and index.
#' @format A data frame with 14,951 rows and 3 columns:
#' \describe{
#'   \item{aacode}{Character: Unique identifier codes (e.g., "I0011105", "I0011109", "I0011110").}
#'   \item{weight}{Integer: Weights associated with each aacode (e.g., 1200, 1218, 944).}
#'   \item{index}{Integer: Index values corresponding to each aacode (e.g., 1, 2, 3).}
#' }
#' @examples
#' \dontrun{
#' # Accessing the first few rows of the data frame
#' head(d_core_index)
#'
#' # Summary statistics of the weight column
#' summary(d_core_index$weight)
#' }
"d_core_index"
