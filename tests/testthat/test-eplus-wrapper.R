# Load the testthat package
library(testthat)


# Define the test cases for convert_idf_to_json
test_that("convert_idf_to_json completes successfully", {
    # Define a mock file path
    mock_filepath <- "data/inputs/model_sim.idf"
    mock_converted <- "data/inputs/model_sim.json"

    # Call the function
    convert_idf_to_json(mock_filepath)

    # Check if the function completes without errors
    expect_true(TRUE)

    # Remove converted file
    # file.remove(mock_converted)
})



# Define the test cases for generate_idf
test_that("generate_idf creates the correct files", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Define index-model and directory path
    id_model <- 2
    the$.enhub.paths$eval$A <- "data/inputs"
    dir_path <- file.path(the$.enhub.paths$eval$A, id_model)

    # Expected files
    list_dir <- c(
        "default_schedules.csv.gz",
        "model_sim.idf",
        "model_sim.json",
        "summary_card.json",
        "typical_appliances.csv"
    )

    # Run the function
    generate_idf(idf = 2, fixed_schedule = FALSE, resolution = "h")

    # Check the contents of the directory
    created_files <- list.files(dir_path)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the contents with the expected list of files
    expect_equal(sort(created_files), sort(list_dir))
})



# Define the test cases for run_energy_plus
test_that("run_energy_plus runs the simulation succesfully", {
    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Define index-model and directory path
    id_model <- 1
    the$.enhub.paths$eval$A <- "data/inputs"
    the$.enhub.paths$eval$B <- "data/outputs"
    dir_path <- file.path(the$.enhub.paths$eval$B, id_model)

    # Expected files
    list_dir <- c(
        "3d_model.wrl",
        "results_eplus.csv.gz",
        "results_summary.csv.gz"
    )

    # Run the function
    run_energy_plus(
        id_model,
        file.path(
            "/Users/gustavo/Library/Application Support",
            "org.R-project.R/R/enhub/data_sources/_weather/stations",
            "GBR_ENG_Plymouth-Mount.Batten.038270_TMYx/data.epw"
        )
    )

    # Check the contents of the directory
    created_files <- list.files(dir_path)

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)
    unlink(dir_path, recursive = TRUE)

    # Compare the contents with the expected list of files
    expect_equal(sort(created_files), sort(list_dir))
})
