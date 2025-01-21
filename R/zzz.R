.onLoad <- function(...) {

  # Disable scientific notation
  base::options(scipen = 999)

  # Enable internal state environment
  the <<- new.env(parent = emptyenv())

  # Clear the console
  packageStartupMessage("\014")

  # Display a welcome message
  packageStartupMessage("The", " \033[1;35m", "EnHub",
    "\033[0m ", "library is set and ready!\n")

  invisible()
}
