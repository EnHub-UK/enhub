# Load the testthat package
library(testthat)
library(tibble)


# Define the test cases for run_energy_plus
test_that("locate_weather_files completes succesfully", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Define the parameters
    fname <- "<from survey>"
    l.par <- list(use_weather_file = "<from survey>", region_in_survey = "South West")

    # Expected structures
    example_info <- tibble(
        id = 1:5,
        name = c("Location1", "Location2", "Location3", "Location4", "Location5"),
        temperature = c(23.5, 25.0, 22.8, 24.1, 23.9),
        humidity = c(60, 65, 58, 62, 64)
    )

    l_schema <- list(
        epw = "string with epw file",
        ddy = "string with ddy file",
        clm = "string with clm file",
        rain = "string with rain file",
        stat = "string with stat file",
        wea = "string with wea file",
        info = example_info
    )

    # Run the function
    results <- locate_weather_files(fname, l.par)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the structure
    expect_equal(names(results), names(l_schema))
    expect_equal(lapply(results, class), lapply(l_schema, class))
})
