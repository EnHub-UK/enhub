# Load the testthat package
library(testthat)

# Define the test cases for validate_sim_period
test_that("validate_sim_period works correctly", {
    # Test valid simulation period
    expect_silent(validate_sim_period(2024, c(1, 1, 12, 31)))

    # Test invalid simulation period (February with 31 days)
    expect_error(
        validate_sim_period(2024, c(1, 2, 2, 31)),
        "Invalid simulation period. Please verify start and end dates."
    )

    # Test invalid simulation period (negative day)
    expect_error(
        validate_sim_period(2024, c(1, -1, 12, 31)),
        "Invalid simulation period. Please verify start and end dates."
    )

    # Test invalid simulation period (month out of range)
    expect_error(
        validate_sim_period(2024, c(1, 13, 31, 12)),
        "Invalid simulation period. Please verify start and end dates."
    )

    # Test valid leap year period
    expect_silent(validate_sim_period(2024, c(1, 2, 2, 29)))
})
