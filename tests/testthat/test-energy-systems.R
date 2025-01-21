# Load the testthat package
library(testthat)
library(tibble)


test_that("define_heating_components returns correct values for given inputs", {

    # Mock global parameters
    the <<- list(
        GLOBAL_PAR = list(
            heating_system_efficiency = NULL,
            heat_pump_cop = NULL,
            heating_ashp = "standard",
            underfloor_heat = FALSE,
            heating_hp_variation = "standard",
            heating_setpoints = "constant",
            heating_availability = "constant"
        )
    )

    # Test case 1: k_msh = 1, k_dhw = 1
    result <- define_heating_components(1, 1)
    expect_equal(result$main_fuel, "NaturalGas")
    expect_equal(result$main_heater, "Boiler:HotWater")
    expect_equal(result$main_device, "hw-standard-boiler")
    expect_equal(result$main_emitter, "ZoneHVAC:Baseboard:RadiantConvective:Water")
    expect_equal(result$main_efficiency, 0.845)
    expect_equal(result$main_cylinder, 180)
    expect_equal(result$main_regime, "constant")
    expect_equal(result$main_emitters, "constant")
    expect_equal(result$dhw_fuel, "NaturalGas")
    expect_equal(result$dhw_heater, "WaterHeater:Mixed")
    expect_equal(result$dhw_device, "dhw-water-heater")
    expect_equal(result$radiators_temperature, 75.7025)
    expect_equal(result$curves, "Boiler Efficiency")

    # Test case 2: k_msh = 14, k_dhw = 10
    result <- define_heating_components(14, 10)
    expect_equal(result$main_fuel, "Electricity")
    expect_equal(result$main_heater, "HeatPump:WaterToWater:EquationFit:Heating")
    expect_equal(result$main_device, "hw-heat-pump")
    expect_equal(result$main_emitter, "ZoneHVAC:Baseboard:RadiantConvective:Water")
    expect_equal(result$main_efficiency, 3.2)
    expect_equal(result$main_cylinder, 120)
    expect_equal(result$main_regime, "constant")
    expect_equal(result$main_emitters, "constant")
    expect_equal(result$dhw_fuel, "DistrictHeatingWater")
    expect_equal(result$dhw_heater, "WaterHeater:Mixed")
    expect_equal(result$dhw_device, "dhw-zero-storage")
    expect_equal(result$radiators_temperature, 75.7025)
    expect_equal(result$curves, c("WWHPHeatCapCurve", "WWHPHeatPowCurve"))

    # Test case 3: k_msh = 0, k_dhw = 0 (DHW exception)
    result <- define_heating_components(0, 0)
    expect_equal(result$main_fuel, "DistrictHeatingWater")
    expect_equal(result$main_heater, "DistrictHeating:Water")
    expect_equal(result$main_device, "hw-district-heating")
    expect_equal(result$main_emitter, "ZoneHVAC:IdealLoadsAirSystem")
    expect_equal(result$main_efficiency, NA)
    expect_equal(result$main_cylinder, NA)
    expect_equal(result$main_regime, "constant")
    expect_equal(result$main_emitters, "constant")
    expect_equal(result$dhw_fuel, "DistrictHeatingWater")
    expect_equal(result$dhw_heater, "WaterHeater:Mixed")
    expect_equal(result$dhw_device, "dhw-zero-storage")
    expect_equal(result$radiators_temperature, NA)
    expect_equal(result$curves, "")

    # Test case 4: k_msh = 15, k_dhw = 11
    result <- define_heating_components(15, 11)
    expect_equal(result$main_fuel, "Electricity")
    expect_equal(result$main_heater, "WaterHeater:HeatPump:PumpedCondenser")
    expect_equal(result$main_device, "hw-heat-pump")
    expect_equal(result$main_emitter, "ZoneHVAC:Baseboard:RadiantConvective:Water")
    expect_equal(result$main_efficiency, 3.2)
    expect_equal(result$main_cylinder, 120)
    expect_equal(result$main_regime, "constant")
    expect_equal(result$main_emitters, "constant")
    expect_equal(result$dhw_fuel, "DistrictHeatingWater")
    expect_equal(result$dhw_heater, "WaterHeater:Mixed")
    expect_equal(result$dhw_device, "dhw-zero-storage")
    expect_equal(result$radiators_temperature, 75.7025)
    expect_equal(result$curves, c("ASHP HighT CAPFT", "ASHP HighT COPFT", "HPWHPLFFPLR", "DefaultFanPowerRatioCurve", "DefaultFanEffRatioCurve"))
})


# Define the test context
test_that("obtain_heating_components works correctly", {

    # Test case 1: id = 0
    result <- obtain_heating_components('MSH1DHW1', 1, l_energy_systems)
    expect_equal(result$loop, "zone-loop-double")
    expect_equal(result$emitter, "zone-radiator-water")
    expect_equal(result$energy_system_code, 'MSH1DHW1')
    expect_equal(result$space_heating, "gas standard")
    expect_equal(result$hot_water, "gas standard")
    expect_equal(result$insulation, 1)

})


# Define the test context
test_that("define_loop_parameters works correctly", {

  # Test case 1: 'zone-loop-double'
  result <- define_loop_parameters('zone-loop-double')
  expect_equal(result$main_loop$name, "main-water-loop")
  expect_equal(result$main_loop$type, "heating")
  expect_equal(result$main_loop$temperature_exit, 71.151)
  expect_type(result$main_loop$temperature_exit, "double")
  expect_equal(result$dhw_loop$name, "hot-water-loop")
  expect_equal(result$dhw_loop$type, "heating")
  expect_equal(result$dhw_loop$temperature_exit, 67.15)
  expect_type(result$dhw_loop$temperature_exit, "double")

  # Test case 1: 'zone-loop-double'
  result <- define_loop_parameters('zone-loop-triple')
  expect_equal(result$main_loop$name, "main-water-loop")
  expect_equal(result$main_loop$type, "heating")
  expect_type(result$main_loop$temperature_exit, "double")
  expect_equal(result$dhw_loop$name, "hot-water-loop")
  expect_equal(result$dhw_loop$type, "heating")
  expect_type(result$dhw_loop$temperature_exit, "double")
  expect_equal(result$triple_loop$name, "hp-water-loop")
  expect_equal(result$triple_loop$type, "heating")
  expect_type(result$triple_loop$temperature_exit, "double")

})
