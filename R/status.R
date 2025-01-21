
list_evaluation_files <- function(relative_path, file_pattern){
  #' @title Lists files matching a pattern in a directory.
  #'
  #' @description This function retrieves a list of files 
  #' within a directory that match a specified regular 
  #' expression pattern.
  #'
  #' @param relative_path The relative path to the directory.
  #' @param file_pattern The regular expression pattern for file names.
  #'
  #' @return A sorted vector of integer file IDs extracted from 
  #' the matched filenames.
  #' @export

  # List matching files recursively within the path
  f <- list.files(path=relative_path, recursive = T, pattern = file_pattern)

  # Extract integer IDs from filenames (assuming numeric format)
  f <- gsub("\\D","", f, perl=T)
  f <- suppressWarnings(as.vector(as.integer(f)))

  # Remove non-numeric elements and sort in descending order
  f <- f[!is.na(f)]
  f <- sort(f, decreasing = FALSE)

  return(f)
}

check_simulation_status <- function(f_sel=NULL){
  #' @title Checks the status of simulations.
  #'
  #' @description This function checks the status of simulations based on the presence of
  #' specific files in predefined directories.
  #'
  #' @param f_sel Optional simulation ID to check.
  #' If NULL, checks all simulations.
  #'
  #' @return A list containing simulation status information.
  #' @export

  # Helper function to create bullet points for file lists
  display_info_bullets <- function(path, label){
    res <- paste(
      paste0("  ", "\033[1;36m", label, "\033[0m ", arrow_icon),
      paste(list.files(path = path),
        collapse = " / "
      ), "\n"
    )
    return(res)
  }

  # Some visual elements
  pass_icon <- "\u2714"
  error_icon <- "\u2716"
  arrow_icon <- "\u2192"

  # Define path constants
  path_pre <- the$.enhub.paths$eval$A
  path_evl <- the$.enhub.paths$eval$B
  path_sum <- the$.enhub.paths$eval$C

  cat("\014")

  if(is.null(f_sel)){

    # Check status for all simulations
    d_idf <- list_evaluation_files(path_pre, "model_sim.idf")
    d_schedule <- list_evaluation_files(path_pre, "default_schedules.csv*")
    d_inputs <- d_idf[d_idf %in% d_schedule]

    d_simulation <- list_evaluation_files(path_pre, "model_sim.idf")
    d_results <- list_evaluation_files(path_evl, "results_eplus*")
    d_outputs <- d_simulation[d_simulation %in% d_results]

    d_summaries <- list_evaluation_files(path_sum, "summary_out_card.json")
    d_post <- d_summaries[d_summaries %in% d_simulation]

    d_res <- d_inputs[!(d_inputs %in% d_outputs)]

    l_res <- list(
      queued=d_inputs,
      completed=d_outputs,
      evaluated=d_post)

    return(l_res)
    
  }else{

    # Check status for a specific simulation
    path_pre_sel <- file.path(path_pre, f_sel) %>%
      normalizePath(winslash = "/", mustWork = F)
    path_sim_sel <- file.path(path_evl, f_sel) %>%
      normalizePath(winslash = "/", mustWork = F)
    path_sum_sel <- file.path(path_sum, f_sel) %>%
      normalizePath(winslash = "/", mustWork = F)

    d_res <- f_sel

    file_pre_sel <- file.exists(path_pre_sel)
    file_sim_sel <- file.exists(path_sim_sel)
    file_sum_sel <- file.exists(path_sum_sel)

    if((file_pre_sel == TRUE) & (file_sim_sel == TRUE) & (file_sum_sel == TRUE)){
      txt_s <- paste("\033[1;35m", pass_icon, "\033[0m ", " generated + simulated + summarised\n")
      txt_a <- display_info_bullets(path_pre_sel,'A')
      txt_b <- display_info_bullets(path_sim_sel,'B')
      txt_c <- display_info_bullets(path_sum_sel,'C')
      message(c(txt_s,txt_a, txt_b, txt_c))

    }else if((file_pre_sel == TRUE) & (file_sim_sel == TRUE)){
      txt_s <- paste("\033[1;35m", pass_icon, "\033[0m ", " generated + simulated \n")
      txt_a <- display_info_bullets(path_pre_sel,'A')
      txt_b <- display_info_bullets(path_sim_sel,'B')
      message(c(txt_s,txt_a, txt_b))

    }else if((file_pre_sel == TRUE) & (file_sim_sel == FALSE)){
      txt_s <- paste("\033[1;35m", pass_icon, "\033[0m ", " generated \n")
      txt_a <- display_info_bullets(path_pre_sel,'A')
      message(c(txt_s,txt_a))
    }else{
      message(error_icon, " No data found with this ID.")
    }

    l_res <- f_sel
  }

}

idf_built_status <- function(label, extra_info = NULL) {
  #' @title IDF Built Status
  #' @description This function generates a formatted message 
  #' indicating the status of an IDF project configuration.
  #' 
  #' @param label A character string representing the label 
  #' for the status message.
  #' @param extra_info An optional character string for 
  #' additional information to be appended to the status message.
  #' 
  #' @return The function returns the formatted status message.
  #' @examples
  #' \dontrun{
  #' idf_built_status("IDF project configuration")
  #' idf_built_status("IDF project configuration", "Additional info")
  #' }

  # Some visual elements
  pass_icon <- "\u2714"
  error_icon <- "\u2716"

  # Generate the status message
  text_main <- paste0(
    "\033[1;35m", pass_icon, "\033[0m ",
    "\033[1;36m", "[done]", "\033[0m ",
    "\033[0;32m", label, "\033[0m\n"
  )

  # Return, appending extra information if provided
  return(cat(text_main, extra_info))
}

display_epw_info <- function(l_info) {
  #' @title Display Energy Plus Weather (EPW) Info
  #' @description This function checks if the EPW file exists and, 
  #' if so, converts the EPW info to JSON format and prints it.
  #' @param l_info A list containing the EPW information.
  #' @return The function prints the formatted JSON information 
  #' of the EPW file.
  #' @export

  # Check if the EPW file exists  
  if (file.exists(l_info$epw)) {
    l_info$info %>%
      dplyr::mutate(file_path = l_info$epw) %>%
      jsonlite::toJSON(pretty = TRUE) %>%
      message(cat())
  }
}
