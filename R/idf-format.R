#' @title Make IDF block
#'
#' @description This function generates an IDF block based on provided data.
#' It is used to create a block of text that can be inserted into an EnergyPlus.
#' The function is used as a lamda function in other functions, and reads the
#' EnergyPlus dictionary of IDF-comments to create a block of text.
#'
#' @param l_in: A list of input data.
#' @param i_act: The active block.
#' @param i_rgm: The active regime.
#' @param i_rad: The active radiator.
#' @param d_ref: A data frame containing reference data.
#' @param l_zon: A list of zone data.
#' @param l_com: A list of IDF-comments
#'
#' @name make_idf_block
#' @family idf-formatting
NULL


#.. auxiliary functions & definitions -----------------------------------------

make_idf_block_title <- function(lbl) {
  #' @title Create IDF Block Title
  #' @description This function creates a formatted title for an IDF (Input Data File) block.
  #'
  #' @param lbl The label for the title.
  #' @return A character string representing the IDF block title.
  #' @examples
  #' # Generate a title for a class:
  #' make_idf_block_title("Example Class")
  #' @family idf-formatting
  return(paste(
    "!-   =========== ALL OBJECTS IN CLASS:", toupper(lbl), "==========="
  ))
}

make_idf_block_header <- function(txt, typ = "class", k_max = 65) {
  #' @title Create IDF Block Header
  #' @description This function generates a formatted header for an IDF (Input Data File) block.
  #'
  #' @param txt The text for the header.
  #' @param typ The type of header (default: 'class').
  #' @param k_max The maximum length of the header (default: 65).
  #' @return A character string representing the IDF block header.
  #' @examples
  #' # Generate a header for a class:
  #' make_idf_block_header("Example Header")
  #' 
  #' # Generate a header using an alternative format:
  #' make_idf_block_header("Example Header", typ = 'other', k_max = 70)
  #' @family idf-formatting

  # Create left-side padding based on type
  txt_l <-
    ifelse(typ == "class",
      paste0("  ", paste(replicate(11, "="), collapse = "")), ""
    )

  # Calculate length of right-side padding
  txt_len <- k_max - nchar(txt) - nchar(txt_l) - 2

  # Create right-side padding
  txt_r <- paste(replicate(txt_len, "="), collapse = "")

  # Combine elements to form the header
  res <- paste0("!- ", txt_l, " ", txt, " ", txt_r)

  return(res)
}

add_idf_comment_custom <- function(txt, n_spaces = 5, is_end = FALSE) {
  #' @title Add IDF Comment with Custom Formatting
  #' @description This function adds a comment to an IDF (Input Data File) with custom formatting.
  #'
  #' @param txt The comment text.
  #' @param n_spaces The number of spaces before the comment (default: 5).
  #' @param is_end Whether this is the end of a comment block (default: FALSE).
  #' @return A character string representing the IDF comment.
  #' @examples
  #' # Append a comment with default comma separator:
  #' add_idf_comment_custom("Example comment")
  #' 
  #' # Append a comment with a semicolon separator:
  #' add_idf_comment_custom("End of block", is_end = TRUE)
  #' @family idf-formatting

  mark <- ifelse(is_end == FALSE, ",", ";")
  out <- paste(mark, paste(replicate(n_spaces, " "), collapse = ""),
    "!- ", txt,
    sep = "")

  return(out)
}

add_idf_comment <- function(val_par, val_com) {
  #' @title Add IDF Comments
  #' @description This function adds IDF comments to a list of values and corresponding comments.
  #'
  #' @param val_par A vector of parameter values.
  #' @param val_com A vector of corresponding comments.
  #' @return A character vector of IDF comments.
  #' @examples
  #' add_idf_comment(c("param1", "param2"), c("comment1", "comment2"))
  #' @family idf-formatting
    
  # Helper to format idf
  add_mark <- function(lbl, mark, gap = 5) {
    # @title Create Formatted Comment String
    # @description This helper function creates a formatted comment string.
    #
    # @param lbl The comment text.
    # @param mark The ending character (e.g., "," or ";").
    # @param gap The number of spaces before the comment (default: 5).
    # 
    # @return A formatted comment string.
    gap_post <- paste(replicate(gap, " "), collapse = "")
    return(paste(mark, gap_post, "!- ", lbl, sep = ""))
  }

  # Use pre-defined indentation
  indent <- ifelse(exists("the$.indent"), 
    the$.indent, paste(rep(" ", 4), collapse = ""))

  # Define idf blocks
  idf <- data.frame(val = val_par, cmn = val_com)
  idf$end <- c(rep(",", length(val_par) - 1), ";")

  # Precompute the indented values and separator lengths
  idf$val <- c(idf$val[1], paste0(indent, idf$val[-1]))
  idf$sep <- max(nchar(idf$val)) - nchar(idf$val) + 1

  # Use vectorized operation to create the txt column
  idf$txt <- paste0(idf$val, 
    mapply(add_mark, idf$cmn, idf$end, idf$sep))

  # Append gaps
  out <- c(idf$txt, "")

  return(out)
}

iterate_on_list <- function(lpar, com) {
  #' @title Iterate Over List of Parameters
  #' @description This function iterates over a list of parameters and creates IDF comments.
  #'
  #' @param lpar A list of parameters.
  #' @param com A vector of comments.
  #' @return A character vector of IDF comments.
  #' @examples
  #' \dontrun{
  #' iterate_on_list(list("param1", "param2"), c("comment1", "comment2"))
  #' }
  #' @family idf-formatting
  
  obj <- unlist(lpar) %>% as.character()
  res <- add_idf_comment(obj, c("", com[1:length(obj) - 1]))

  return(res)
}

iterate_on_output_list <- function(ip, lp, sup) {
  #' @title Iterate Over Output List
  #' @description This function iterates over an output list and creates IDF comments.
  #' 
  #' Here is a usage example:
  #' ```r 
  #' iterate_on_output_list(
  #'  "Site:Location", 
  #'  list("Site:Location" = c("a location", 51.15, -0.18, 0.0, 62.0)), 
  #'  l_idf_blocks)
  #' ```
  #' 
  #' And the output will be:
  #' ```fortran
  #' 
  #'  !-   =========== ALL OBJECTS IN CLASS: SITE:LOCATION ==========="
  #'  
  #'  Site:Location,  !- "
  #'      a location, !- Name"
  #'      51.15,      !- Latitude"
  #'      -0.18,      !- Longitude"
  #'      0,          !- Time Zone"
  #'      62;         !- Elevation"
  #' 
  #'  ```
  #' 
  #' @param ip Input parameter.
  #' @param lp List of parameters.
  #' @param sup Supplementary information.
  #' @return A character vector of IDF comments.
  #' @examples
  #' # Example 1: Iterate over an output list
  #' iterate_on_output_list("class1", 
  #'  list("class1" = c("comment2", "comment2")), 
  #'  list("class1" = c("comment2", "comment2")))
  #' 
  #' # Example 2: Add IDF comments to a list of values and corresponding comments
  #' iterate_on_output_list("Site:Location", 
  #'  list("Site:Location" = c("a location", 51.15, -0.18, 0.0, 62.0)), 
  #'  l_idf_blocks)
  #' @family idf-formatting
  
  l_section <- make_idf_block_title(ip)
  l_com <- sup[[ip]]
  l_ins <- c(ip, lp[[ip]])
  l_obj <- iterate_on_list(l_ins, l_com)
  l_res <- c(l_section, "", unlist(l_obj), "")

  return(l_res)
}

determine_end_of_idf_block <- function(ini, txt) {
  #' @title Determine End of IDF Block
  #' @description This function determines the end of an IDF (Input Data File) block.
  #'
  #' @param ini The starting index of the IDF block.
  #' @param txt The text to search.
  #' @return A list containing the start index, end index, and block label.
  #' @examples
  #' \dontrun{
  #' determine_end_of_idf_block(1, c("line1", "line2", ";"))
  #' }
  #' @family idf-formatting

  # Define idf block
  txt_ini <- txt[ini:length(txt)]

  # Clean string using regular expressions
  txt_label <- txt[ini + 1] %>%
    gsub("^\\s+", "", .) %>%
    gsub("\\s+\\!.*$", "", .) %>%
    gsub(",", "", .)

  # Initialize iteration
  j <- 1
  end <- FALSE

  # Iterate over idf string until end of block is found
  while (end == FALSE) {
    k_str <- txt_ini[j] %>%
      gsub("^\\s+", "", .) %>%
      gsub("\\s+\\!.*$", "", .)
    end <- grepl(";", k_str)
    j <- j + 1
  }

  # Correct iteration end-index
  end <- j - 1

  # Return the start index, end index, and block label
  return(list(ini = ini, end = ini + end - 1, label = txt_label))
}
