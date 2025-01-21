# Load the testthat package
library(testthat)


# Define the test cases for run_energy_plus
test_that("locate_weather_files completes succesfully", {

    # Initialize the project
    check_external_dependencies(redefine = FALSE)
    init_project(core_data = "ehs", core_mode = "def", fresh_setup = TRUE)

    # Define input parameters
    l_d_info <- list(
        "width" = 6.9549,
        "depth" = 4.6265,
        "floor_height" = 2.8,
        "window_to_wall" = 0.05,
        "cuboid" = "MOD-2FWANBBA",
        "flat_roof" = FALSE,
        "access" = "front",
        "origin" = 0, 0, 0,
        "layouts" = c("lay_064", "lay_070", "lay_077")
    )

    l_dwell_default <- list(
        "access" = "front",
        "flat_roof" = FALSE,
        "floor_height" = 2.9,
        "origin" = c(0, 0, 0),
        "window_to_wall_per_side" = list(
            "front" = 25,
            "left" = 25,
            "rear" = 25,
            "right" = 25
            ))

    # Expected output structures
    res_cuboid <- list(
        "access" = "front",
        "attachment" = "both",
        "cuboid" = "MOD-2FWANBBA",
        "depth" = 4.6265,
        "flat_roof" = FALSE,
        "floor_height" = 2.8,
        "layouts" = c("lay_064", "lay_070", "lay_077"),
        "n_storeys" = as.integer(2),
        "origin" = c(0, 0, 0),
        "pitched_roof" = TRUE,
        "pitched_roof_window" = TRUE,
        "storeys" = c("GF", "1F", "AT"),
        "width" = 6.9549,
        "window_to_wall_per_side" = list(
            "front" = 0.5,
            "left" = 0,
            "rear" = 0.5,
            "right" = 0
        )
    )
    res_room_layouts <- list(
      `GF` = list(
        room_location = matrix(),
        room_assignment = matrix(),
        conjoined_zones = data.frame(),
        room_floor_links_to = matrix(),
        room_ceiling_links_to = matrix(),
        room_front_links_to = matrix(),
        room_rear_links_to = matrix(),
        room_right_links_to = matrix(),
        room_left_links_to = matrix(),
        faces = list(),
        origins = data.frame()),
      `1F`= list(),
      `AT`= list()
    )

    # Run the function
    l_cuboid <- decode_cuboid_inputs(l_d_info, l_dwell_default)
    l_dim <- obtain_dimensions(l_cuboid)
    l_room_layouts <- lapply(l_cuboid$storeys, generate_room_layout, l_cuboid, l_dim)
    names(l_room_layouts) <- l_cuboid$storeys

    # Remove created directories
    unlink("outcomes", recursive = TRUE)
    unlink("config", recursive = TRUE)

    # Compare the structure
    expect_equal(names(l_cuboid), names(res_cuboid))
    expect_equal(lapply(l_cuboid, class), lapply(res_cuboid, class))

    expect_equal(names(l_room_layouts), names(res_room_layouts))
    expect_equal(lapply(l_room_layouts$GF, class), lapply(res_room_layouts$GF, class))
})
