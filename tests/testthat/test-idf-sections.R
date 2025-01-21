# Load the testthat package
library(testthat)
library(dplyr)


# Define the test cases for run_energy_plus
test_that("assign_house_orientation correctly parse the orientation", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Sample input
    idf <- 1
    id_ehs <- get_aacode(idf)
    l_ehs <- s_ehs_2011_ext
    d_compl <- l_ehs$compleme %>%
        filter(V001_HousingCode == id_ehs)

    l_bldg <- list(
        house_front_orientation = 0,
        house_back_orientation = 180,
        house_left_orientation = 270,
        house_right_orientation = 90
    )

    # Run the function
    i_north <- assign_house_orientation(l_bldg, d_compl)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the structure
    expect_equal(lapply(idf, class), lapply(i_north, class))
    expect_equal(i_north, 0)
})


# Define the test cases for run_energy_plus
test_that("populate_idf_section_A returns a list with the correct structure", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Sample input
    idf <- 1

    # Expected structures
    l_section <- list(
        sum = list(),
        idf = "a long string with IDF section"
    )
    l_sum <- list(
        aacode = "string with aacode",
        orientation = 0,
        terrain = "string with terrain",
        time_step = as.integer(6)
    )

    # Run the function
    results <- populate_idf_section_A(idf)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the structure
    expect_equal(names(results), names(l_section))
    expect_equal(lapply(results, class), lapply(l_section, class))

    expect_equal(names(results$sum), names(l_sum))
    expect_equal(lapply(results$sum, class), lapply(l_sum, class))
})


# Define the test cases for run_energy_plus
test_that("populate_idf_section_B returns a list with the correct structure", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Sample input
    idf <- 1

    # Expected structures
    l_section <- list(
        sum = list(),
        idf = "a long string with IDF section"
    )
    l_sum <- list(
        epw = list(),
        ddy = tibble(data.frame(a = 1:2, b = 1:2)),
        sim_lapse = list(),
        sim_year = as.integer(6)
    )

    # Define weather file source and locate weather files
    l_epw <- define_weather_file_source(idf) %>%
        modifyList(locate_weather_files(.$use_weather_file, .))

    # Run the function
    results <- populate_idf_section_B(idf, l_epw)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the structure
    expect_equal(names(results), names(l_section))
    expect_equal(lapply(results, class), lapply(l_section, class))

    expect_equal(names(results$sum), names(l_sum))
    expect_equal(lapply(results$sum, class), lapply(l_sum, class))
})


# Define the test cases for run_energy_plus
test_that("populate_idf_section_E returns a list with the correct structure", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Sample input
    idf <- 1

    # # Define global settings for fixed schedule and simulation resolution
    # l_global <- list(fixed_schedule = TRUE, simulation_resolution = 'h')
    # the$GLOBAL_PAR <- modifyList(the$GLOBAL_PAR, l_global)

    # Expected structures
    l_section <- list(
        card = list(),
        idf = "a long string with IDF section"
    )
    l_names <- c("cuboid", "zones")


    # Run the function
    results <- populate_idf_section_E(idf)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the structure
    expect_equal(names(results), names(l_section))
    expect_equal(lapply(results, class), lapply(l_section, class))

    expect_equal(names(results$card), l_names)
})
