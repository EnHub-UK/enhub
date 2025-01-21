#' @title Dictionary with comfort states information
#'
#' @description This object contains a dictionary with comfort states information.
#'
#' @format A list containing datasets related to thermal sensation categories.
#' - fanger: A character vector representing Fanger's thermal sensation scale.
#'   - cold: Feeling cold.
#'   - cool: Feeling cool.
#'   - slightly cool: Feeling slightly cool.
#'   - neutral: Feeling neutral.
#'   - slightly warm: Feeling slightly warm.
#'   - warm: Feeling warm.
#'   - hot: Feeling hot.
#' - general: A character vector representing general thermal sensation categories.
#'   - a bit too cold: Feeling a bit too cold.
#'   - ok: Feeling comfortable.
#'   - a bit too hot: Feeling a bit too hot.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_comfort_states
#' }
#' @family supporting dictionaries
"l_comfort_states"



#' @title Dictionary with schedule information
#'
#' @description This object contains a dictionary with schedule information.
#'
#' @format A list containing datasets related to IDF schedules.
#' - schedule:thermostat-control-type: A list containing blocks of data related to thermostat control types.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Any Number"
#'   - name: "schedule:thermostat-control-type"
#' - schedule:heating-availability-\{intermittent\}: A list containing blocks of data related to intermittent heating availability.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Any Number"
#'   - name: "schedule:heating-availability-\{intermittent\}"
#' - schedule:heating-availability-\{constant\}: A list containing blocks of data related to constant heating availability.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Any Number"
#'   - name: "schedule:heating-availability-\{constant\}"
#' - schedule:water-heater-ambient-temperature: A list containing blocks of data related to water heater ambient temperature.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Any Number"
#'   - name: "schedule:water-heater-ambient-temperature"
#' - schedule:hot-water-services-warm: A list containing blocks of data related to warm hot water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Any Number"
#'   - name: "schedule:hot-water-services-warm"
#' - schedule:hot-water-services-hot: A list containing blocks of data related to hot water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Any Number"
#'   - name: "schedule:hot-water-services-hot"
#' - schedule:water-services:\{KitchenSink\}: A list containing blocks of data related to kitchen sink water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{KitchenSink\}"
#' - schedule:water-services:\{KitchenOther\}: A list containing blocks of data related to other kitchen water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{KitchenOther\}"
#' - schedule:water-services:\{KitchenWasher\}: A list containing blocks of data related to kitchen washer water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{KitchenWasher\}"
#' - schedule:water-services:\{BathroomShower\}: A list containing blocks of data related to bathroom shower water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{BathroomShower\}"
#' - schedule:water-services:\{BathroomSink\}: A list containing blocks of data related to bathroom sink water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{BathroomSink\}"
#' - schedule:water-services:\{BathroomBath\}: A list containing blocks of data related to bathroom bath water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{BathroomBath\}"
#' - schedule:water-services:\{BathroomService\}: A list containing blocks of data related to bathroom service water services.
#'   - blocks: A list containing specific blocks of data.
#'   - limits: "Fraction"
#'   - name: "schedule:water-services:\{BathroomService\}"
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_sched_compact
#' }
#' @family supporting dictionaries
"l_sched_compact"



#' @title Dictionary with IDF modelling curves
#'
#' @description This object contains a dictionary with IDF modelling curves
#'
#' @format A list containing various curve datasets used for different types of performance and efficiency calculations.
#' - Curve:Biquadratic: A list containing biquadratic curve datasets.
#'   - ASHP HighT CAPFT: A list containing coefficients and parameters for the ASHP High Temperature Capacity Factor.
#'     - coefficient1_constant: 0.975
#'     - coefficient2_x: 0.0204
#'     - coefficient3_x_2: 0.0003
#'     - coefficient4_y: -0.0049
#'     - coefficient5_y_2: 0
#'     - coefficient6_x_y: 0
#'     - input_unit_type_for_x: "Temperature"
#'     - input_unit_type_for_y: "Temperature"
#'     - maximum_value_of_x: 20
#'     - maximum_value_of_y: 80
#'     - minimum_value_of_x: -15
#'     - minimum_value_of_y: 35
#'     - output_unit_type: "Dimensionless"
#'   - ASHP HighT COPFT: A list containing coefficients and parameters for the ASHP High Temperature Coefficient of Performance Factor.
#'     - coefficient1_constant: 1.9
#'     - coefficient2_x: 0.0359
#'     - coefficient3_x_2: 0.0003
#'     - coefficient4_y: -0.0343
#'     - coefficient5_y_2: 0.0002
#'     - coefficient6_x_y: -0.0003
#'     - input_unit_type_for_x: "Temperature"
#'     - input_unit_type_for_y: "Temperature"
#'     - maximum_value_of_x: 20
#'     - maximum_value_of_y: 80
#'     - minimum_value_of_x: -15
#'     - minimum_value_of_y: 35
#'     - output_unit_type: "Dimensionless"
#'   - Boiler Efficiency: A list containing coefficients and parameters for boiler efficiency.
#'     - coefficient1_constant: 1
#'     - coefficient2_x: 0
#'     - coefficient3_x_2: 0
#'     - coefficient4_y: 0
#'     - coefficient5_y_2: 0
#'     - coefficient6_x_y: 0
#'     - maximum_value_of_x: 1
#'     - maximum_value_of_y: 1
#'     - minimum_value_of_x: 0
#'     - minimum_value_of_y: 0
#'   - CondensingBoilerEff: A list containing coefficients and parameters for condensing boiler efficiency.
#'     - coefficient1_constant: 1.12
#'     - coefficient2_x: 0.015
#'     - coefficient3_x_2: -0.026
#'     - coefficient4_y: 0
#'     - coefficient5_y_2: -1.4e-06
#'     - coefficient6_x_y: -0.0015
#'     - maximum_value_of_x: 1
#'     - maximum_value_of_y: 85
#'     - minimum_value_of_x: 0.1
#'     - minimum_value_of_y: 30
#'   - DefrostEIR: A list containing coefficients and parameters for defrost energy input ratio.
#'     - coefficient1_constant: 0.153
#'     - coefficient2_x: 0
#'     - coefficient3_x_2: 0
#'     - coefficient4_y: 0
#'     - coefficient5_y_2: 0
#'     - coefficient6_x_y: 0
#'     - maximum_value_of_x: 100
#'     - maximum_value_of_y: 100
#'     - minimum_value_of_x: -100
#'     - minimum_value_of_y: -100
#' - Curve:Cubic: A list containing cubic curve datasets.
#'   - DefaultFanEffRatioCurve: A list containing coefficients and parameters for the default fan efficiency ratio curve.
#'     - coefficient1_constant: 0.339
#'     - coefficient2_x: 1.73
#'     - coefficient3_x_2: -1.49
#'     - coefficient4_x_3: 0.428
#'     - maximum_curve_output: 1
#'     - maximum_value_of_x: 1.5
#'     - minimum_curve_output: 0.3
#'     - minimum_value_of_x: 0.5
#'   - New style low temperature boiler: A list containing coefficients and parameters for the new style low temperature boiler.
#'     - coefficient1_constant: 0.839
#'     - coefficient2_x: 0.133
#'     - coefficient3_x_2: -0.17
#'     - coefficient4_x_3: 0.0475
#'     - maximum_value_of_x: 1
#'     - minimum_value_of_x: 0.1
#' - Curve:Exponent: A list containing exponential curve datasets.
#'   - DefaultFanPowerRatioCurve: A list containing coefficients and parameters for the default fan power ratio curve.
#'     - coefficient1_constant: 0
#'     - coefficient2_constant: 1
#'     - coefficient3_constant: 3
#'     - maximum_curve_output: 1.5
#'     - maximum_value_of_x: 1.5
#'     - minimum_curve_output: 0.01
#'     - minimum_value_of_x: 0
#' - Curve:QuadLinear: A list containing quadlinear curve datasets.
#'   - WWHPHeatCapCurve: A list containing coefficients and parameters for the water-to-water heat pump heat capacity curve.
#'     - coefficient1_constant: -1.6
#'     - coefficient2_w: -0.983
#'     - coefficient3_x: 3.25
#'     - coefficient4_y: 0.1
#'     - coefficient5_z: 0.0798
#'     - maximum_value_of_w: 100
#'     - maximum_value_of_x: 100
#'     - maximum_value_of_y: 100
#'     - maximum_value_of_z: 100
#'     - minimum_value_of_w: -100
#'     - minimum_value_of_x: -100
#'     - minimum_value_of_y: -100
#'     - minimum_value_of_z: -100
#'   - WWHPHeatPowCurve: A list containing coefficients and parameters for the water-to-water heat pump heat power curve.
#'     - coefficient1_constant: -2.68
#'     - coefficient2_w: 2.69
#'     - coefficient3_x: 0.393
#'     - coefficient4_y: 0.0109
#'     - coefficient5_z: 0.0149
#'     - maximum_value_of_w: 100
#'     - maximum_value_of_x: 100
#'     - maximum_value_of_y: 100
#'     - maximum_value_of_z: 100
#'     - minimum_value_of_w: -100
#'     - minimum_value_of_x: -100
#'     - minimum_value_of_y: -100
#'     - minimum_value_of_z: -100
#' - Curve:Quadratic: A list containing quadratic curve datasets.
#'   - HPWHHeatingCOPFAirFlow: A list containing coefficients and parameters for the heat pump water heater heating COP as a function of air flow.
#'     - coefficient1_constant: 1
#'     - coefficient2_x: 0
#'     - coefficient3_x_2: 0
#'     - input_unit_type_for_x: "Dimensionless"
#'     - maximum_value_of_x: 2
#'     - minimum_value_of_x: 0
#'     - output_unit_type: "Dimensionless"
#'   - HPWHHeatingCOPFWaterFlow: A list containing coefficients and parameters for the heat pump water heater heating COP as a function of water flow.
#'     - coefficient1_constant: 1
#'     - coefficient2_x: 0
#'     - coefficient3_x_2: 0
#'     - input_unit_type_for_x: "Dimensionless"
#'     - maximum_value_of_x: 2
#'     - minimum_value_of_x: 0
#'     - output_unit_type: "Dimensionless"
#'   - HPWHHeatingCapFAirFlow: A list containing coefficients and parameters for the heat pump water heater heating capacity as a function of air flow.
#'     - coefficient1_constant: 1
#'     - coefficient2_x: 0
#'     - coefficient3_x_2: 0
#'     - input_unit_type_for_x: "Dimensionless"
#'     - maximum_value_of_x: 2
#'     - minimum_value_of_x: 0
#'     - output_unit_type: "Dimensionless"
#'   - HPWHHeatingCapFWaterFlow: A list containing coefficients and parameters for the heat pump water heater heating capacity as a function of water flow.
#'     - coefficient1_constant: 1
#'     - coefficient2_x: 0
#'     - coefficient3_x_2: 0
#'     - input_unit_type_for_x: "Dimensionless"
#'     - maximum_value_of_x: 2
#'     - minimum_value_of_x: 0
#'     - output_unit_type: "Dimensionless"
#'   - HPWHPLFFPLR: A list containing coefficients and parameters for the heat pump water heater part load fraction as a function of part load ratio.
#'     - coefficient1_constant: 0.75
#'     - coefficient2_x: 0.25
#'     - coefficient3_x_2: 0
#'     - input_unit_type_for_x: "Dimensionless"
#'     - maximum_value_of_x: 1
#'     - minimum_value_of_x: 0
#'     - output_unit_type: "Dimensionless"
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_htg_curves
#' }
#' @family supporting dictionaries
"l_htg_curves"



#' @title Dictionary with pre-selected output variables
#'
#' @description This object contains a dictionary with pre-selected output variables
#'
#' @format Lists containing various categories of output meters and output variables used in energy modelling.
#' - Output_Meter: A list containing categories of output meters.
#'   - Renewables_PV: A character vector representing photovoltaic-related output meters.
#'     - "Photovoltaic:ElectricityProduced"
#'     - "ElectricitySurplusSold:Facility"
#'     - "ElectricityProduced:Facility"
#'     - "ElectricityNet:Facility"
#'     - ...
#'   - Renewables_Wind: A character vector representing wind turbine-related output meters.
#'     - "WindTurbine:ElectricityProduced"
#'     - "ElectricitySurplusSold:Facility"
#'     - "ElectricityProduced:Facility"
#'     - "ElectricityNet:Facility"
#'     - ...
#'   - Systems: A character vector representing various system-related output meters.
#'     - "Water:Facility"
#'     - "CarbonEquivalentEmissions:Carbon Equivalent"
#'     - "Electricity:Facility"
#'     - "NaturalGas:Facility"
#'     - ...
#' - Output_Variable: A list containing categories of output variables.
#'   - Comfort: A character vector representing comfort-related output variables.
#'     - "Zone Thermal Comfort Fanger Model PMV"
#'   - Performance: A character vector representing performance-related output variables.
#'     - "Surface Window Heat Loss Rate"
#'     - "Surface Window Heat Gain Rate"
#'     - "Surface Window Net Heat Transfer Rate"
#'     - "Surface Inside Face Conduction Heat Transfer Rate"
#'     - ...
#'   - Renewables_PV: A character vector representing photovoltaic-related output variables.
#'     - "Inverter AC Output Electric Energy"
#'     - "Inverter AC Output Electric Power"
#'     - "Generator PV Array Efficiency"
#'     - "Generator PV Cell Temperature"
#'     - ...
#'   - Renewables_Wind: A character vector representing wind turbine-related output variables.
#'     - "Inverter AC Output Electric Energy"
#'     - "Inverter AC Output Electric Power"
#'     - "Generator Produced Electric Power"
#'     - "Generator Produced Electric Energy"
#'     - ...
#'   - Zonal: A character vector representing zonal-related output variables.
#'     - "Zone Air Temperature"
#'     - "Zone Air Humidity Ratio"
#'     - "Zone Air Relative Humidity"
#'     - "Zone Mean Air Temperature"
#'     - ...
#'   - Heating_District: A character vector representing district heating-related output variables.
#'     - "District Heating Hot Water Energy"
#'     - "District Heating Hot Water Rate"
#'     - "Water Heater DistrictHeating Energy"
#'     - "Water Heater DistrictHeating Rate"
#'     - ...
#'   - Heating_HP: A character vector representing heat pump-related output variables.
#'     - "Baseboard Air Inlet Temperature"
#'     - "Baseboard Air Mass Flow Rate"
#'     - "Baseboard Air Outlet Temperature"
#'     - "Fan Air Mass Flow Rate"
#'     - ...
#'   - Heating_Warm: A character vector representing warm heating-related output variables.
#'     - "Cooling Coil Water Heating Electric Energy"
#'     - "Cooling Coil Water Heating Electric Power"
#'     - "Heating Coil Electric Energy"
#'     - "Heating Coil Electric Power"
#'     - ...
#'   - Heating_General: A character vector representing general heating-related output variables.
#'     - "Baseboard Convective Heating Rate"
#'     - "Baseboard Hot Water Mass Flow Rate"
#'     - "Baseboard Radiant Heating Rate"
#'     - "Baseboard Total Heating Rate"
#'     - ...
#'   - Site: A character vector representing site-related output variables.
#'     - "Site Outdoor Air Drybulb Temperature"
#'     - "Site Outdoor Air Dewpoint Temperature"
#'     - "Site Outdoor Air Wetbulb Temperature"
#'     - "Site Outdoor Air Relative Humidity"
#'     - ...
#'   - Excluded: A character vector representing excluded output variables.
#'     - "Zone Ventilation Total Heat Gain Energy"
#'     - "Zone Ventilation Total Heat Loss Energy"
#'     - "Zone Ventilation Air Change Rate"
#'     - "Zone Ventilation Sensible Heat Loss Energy"
#'     - ...
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_idf_outputs
#' }
#' @family supporting dictionaries
"l_idf_outputs"



#' @title Dictionary with energy systems information
#'
#' @description This object contains a dictionary with energy systems information
#'
#' @format A list containing categories of heating systems.
#' - space_heating: A character vector representing different types of space heating systems.
#'   |       |                                         |
#'   |-------|-----------------------------------------|
#'   |   1   |   Gas standard                          |
#'   |   2   |   Gas ‐ combi                           |
#'   |   3   |   Gas back boiler                       |
#'   |   4   |   Oil standard                          |
#'   |   5   |   Solid boiler (house coal/anthracite)  |
#'   |   6   |   Electric boiler                       |
#'   |   7   |   Electric storage                      |
#'   |   8   |   Electric room heater                  |
#'   |   9   |   Warm air ‐ gas fired                  |
#'   |   10  |   Warm air ‐ electric                   |
#'   |   11  |   Community heating without CHP         |
#'   |   12  |   Community heating with CHP            |
#'   |   13  |   Biomass boiler                        |
#'   |   14  |   Ground source heat pump (GSHP)        |
#'   |   15  |   Air source heat pump (ASHP)           |
#'   
#' - hot_water: A character vector representing different types of hot water systems.
#'   |       |                                         |
#'   |-------|-----------------------------------------|
#'   |   1   |   Gas standard                          |
#'   |   2   |   Gas - combi (storage)                 |
#'   |   3   |   Gas - combi (instantaneous)           |
#'   |   4   |   Gas back boiler                       |
#'   |   5   |   Oil standard                          |
#'   |   6   |   Solid boiler (house coal/anthracite)  |
#'   |   7   |   Biomass boiler                        |
#'   |   8   |   Electric boiler                       |
#'   |   9   |   Other electric                        |
#'   |   10  |   Community heating without CHP         |
#'   |   11  |   Community heating with CHP            |
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_energy_systems
#' }
#' @references Hughes, M., Armitage, P., Palmer, J., & Stone, A. (2012). 
#' Converting English Housing Survey Data for Use in Energy Models.
#' @family supporting dictionaries
"l_energy_systems"



#' @title Dictionary with IDF commented blocks
#'
#' @description This object contains a dictionary with IDF commented blocks.
#'
#' @format A list containing the following commented IDF blocks:
#' - AirLoopHVAC: Represents the primary air loop in an HVAC system.
#' - AirLoopHVAC:ControllerList: A list of controllers associated with an AirLoopHVAC.
#' - AirLoopHVAC:OutdoorAirSystem: Defines the outdoor air system for an AirLoopHVAC.
#' - AirLoopHVAC:OutdoorAirSystem:EquipmentList: A list of equipment in the outdoor air system.
#' - AirLoopHVAC:ReturnPath: Defines the return air path in an AirLoopHVAC.
#' - AirLoopHVAC:SupplyPath: Defines the supply air path in an AirLoopHVAC.
#' - AirLoopHVAC:ZoneMixer: Represents a zone mixer in an AirLoopHVAC.
#' - AirLoopHVAC:ZoneSplitter: Represents a zone splitter in an AirLoopHVAC.
#' - AirTerminal:SingleDuct:ConstantVolume:NoReheat: A single duct air terminal with constant volume and no reheat.
#' - AirflowNetwork:MultiZone:Surface: Defines a surface in a multi-zone airflow network.
#' - AirflowNetwork:MultiZone:Zone: Defines a zone in a multi-zone airflow network.
#' - AvailabilityManager:Scheduled: Manages the availability of equipment based on a schedule.
#' - AvailabilityManagerAssignmentList: A list of availability managers assigned to equipment.
#' - Boiler:HotWater: Represents a hot water boiler.
#' - Branch: Defines a branch in a fluid loop.
#' - BranchList: A list of branches in a fluid loop.
#' - Building: Defines the building properties.
#' - BuildingSurface:Detailed: Represents a detailed building surface.
#' - Coil:Heating:Fuel: A heating coil that uses fuel.
#' - Coil:WaterHeating:AirToWaterHeatPump:Pumped: A water heating coil using an air-to-water heat pump.
#' - CondenserEquipmentList: A list of condenser equipment.
#' - CondenserEquipmentOperationSchemes: Defines the operation schemes for condenser equipment.
#' - CondenserLoop: Represents a condenser loop.
#' - Connector:Mixer: A mixer connector.
#' - Connector:Splitter: A splitter connector.
#' - ConnectorList: A list of connectors.
#' - Construction: Defines a construction element.
#' - Construction:AirBoundary: Represents an air boundary construction.
#' - ConstructionProperty:InternalHeatSource: Defines internal heat sources in a construction.
#' - Controller:MechanicalVentilation: A controller for mechanical ventilation.
#' - Controller:OutdoorAir: A controller for outdoor air.
#' - ConvergenceLimits: Defines the convergence limits for simulations.
#' - CurrencyType: Specifies the type of currency used.
#' - Curve:Bicubic: A bicubic curve.
#' - Curve:Biquadratic: A biquadratic curve.
#' - Curve:Cubic: A cubic curve.
#' - Curve:Exponent: An exponential curve.
#' - Curve:Linear: A linear curve.
#' - Curve:QuadLinear: A quad-linear curve.
#' - Curve:Quadratic: A quadratic curve.
#' - Curve:QuadraticLinear: A quadratic-linear curve.
#' - Curve:Quartic: A quartic curve.
#' - Curve:Triquadratic: A triquadratic curve.
#' - DesignSpecification:OutdoorAir: Specifies the design for outdoor air.
#' - DesignSpecification:ZoneAirDistribution: Specifies the design for zone air distribution.
#' - DistrictCooling: Represents district cooling.
#' - DistrictHeating:Water: Represents district heating using water.
#' - ElectricEquipment: Defines electric equipment.
#' - ElectricLoadCenter:Distribution: Represents the distribution centre for electric loads.
#' - ElectricLoadCenter:Generators: A list of generators in the electric load centre.
#' - ElectricLoadCenter:Inverter:Simple: A simple inverter in the electric load centre.
#' - ElectricLoadCenter:Storage:Simple: Simple storage in the electric load centre.
#' - Equipment: General equipment definition.
#' - Exterior:FuelEquipment: Defines exterior fuel equipment.
#' - Exterior:Lights: Defines exterior lighting.
#' - Exterior:WaterEquipment: Defines exterior water equipment.
#' - Fan:ConstantVolume: A constant volume fan.
#' - Fan:OnOff: An on/off fan.
#' - FenestrationSurface:Detailed: Represents a detailed fenestration surface.
#' - GasEquipment: Defines gas equipment.
#' - Generator:Photovoltaic: Represents a photovoltaic generator.
#' - Generator:WindTurbine: Represents a wind turbine generator.
#' - GlobalGeometryRules: Defines global geometry rules.
#' - GroundHeatExchanger:ResponseFactors: Defines response factors for a ground heat exchanger.
#' - GroundHeatExchanger:Slinky: Represents a slinky ground heat exchanger.
#' - GroundHeatExchanger:System: Represents a ground heat exchanger system.
#' - GroundHeatExchanger:Vertical:Properties: Defines properties for a vertical ground heat exchanger.
#' - HVACTemplate:Thermostat: Represents an HVAC template for a thermostat.
#' - HVACTemplate:Zone:IdealLoadsAirSystem: Represents an HVAC template for an ideal loads air system in a zone.
#' - HeatBalanceAlgorithm: Defines the heat balance algorithm.
#' - HeatExchanger:FluidToFluid: Represents a fluid-to-fluid heat exchanger.
#' - HeatPump:WaterToWater:EquationFit:Heating: A water-to-water heat pump for heating using an equation fit model.
#' - HeatPump:WaterToWater:ParameterEstimation:Heating: A water-to-water heat pump for heating using parameter estimation.
#' - HotWaterEquipment: Defines hot water equipment.
#' - Lights: Defines lighting.
#' - Material: Defines a material.
#' - Material:AirGap: Represents an air gap material.
#' - Material:NoMass: Represents a no-mass material.
#' - OtherEquipment: Defines other equipment.
#' - OutdoorAir:Mixer: Represents an outdoor air mixer.
#' - OutdoorAir:Node: Represents an outdoor air node.
#' - OutdoorAir:NodeList: A list of outdoor air nodes.
#' - Output:Constructions: Defines output for constructions.
#' - Output:Diagnostics: Defines output for diagnostics.
#' - Output:Meter: Defines output for meters.
#' - Output:Surfaces:Drawing: Defines output for surface drawings.
#' - Output:Surfaces:List: Defines output for surface lists.
#' - Output:Table:SummaryReports: Defines output for summary reports in table format.
#' - Output:Variable: Defines output variables.
#' - Output:VariableDictionary: Defines the dictionary for output variables.
#' - OutputControl:ReportingTolerances: Defines reporting tolerances for output control.
#' - OutputControl:Table:Style: Defines the style for output tables.
#' - People: Defines people in the simulation.
#' - PhotovoltaicPerformance:Simple: Represents simple photovoltaic performance.
#' - Pipe:Adiabatic: Represents an adiabatic pipe.
#' - PlantEquipmentList: A list of plant equipment.
#' - PlantEquipmentOperation:HeatingLoad: Defines operation for plant equipment based on heating load.
#' - PlantEquipmentOperation:Uncontrolled: Defines uncontrolled operation for plant equipment.
#' - PlantEquipmentOperationSchemes: Defines operation schemes for plant equipment.
#' - PlantLoop: Represents a plant loop.
#' - Pump:ConstantSpeed: A constant speed pump.
#' - Pump:VariableSpeed: A variable speed pump.
#' - RunPeriod: Defines the run period for the simulation.
#' - Schedule:Compact: Represents a compact schedule.
#' - Schedule:File: Represents a schedule from a file.
#' - Schedule:Year: Represents a yearly schedule.
#' - ScheduleTypeLimits: Defines limits for schedule types.
#' - SetpointManager:FollowGroundTemperature: A setpoint manager that follows ground temperature.
#' - SetpointManager:MixedAir: A setpoint manager for mixed air.
#' - SetpointManager:Scheduled: A setpoint manager based on a schedule.
#' - SetpointManager:SingleZone:Reheat: A setpoint manager for single zone reheat.
#' - Shading:Building:Detailed: Represents detailed building shading.
#' - ShadowCalculation: Defines shadow calculation parameters.
#' - SimulationControl: Defines control parameters for the simulation.
#' - Site:GroundReflectance: Defines ground reflectance for the site.
#' - Site:GroundReflectance:SnowModifier: Defines snow modifier for ground reflectance.
#' - Site:GroundTemperature:BuildingSurface: Defines ground temperature for building surface.
#' - Site:GroundTemperature:Deep: Defines deep ground temperature.
#' - Site:GroundTemperature:FCfactorMethod: Defines ground temperature using the FC factor method.
#' - Site:GroundTemperature:Shallow: Defines shallow ground temperature.
#' - Site:GroundTemperature:Undisturbed:KusudaAchenbach: Defines undisturbed ground temperature using the Kusuda-Achenbach method.
#' - Site:Location: Defines the site location.
#' - Site:WaterMainsTemperature: Defines water mains temperature for the site.
#' - Sizing:Parameters: Defines sizing parameters.
#' - Sizing:Plant: Defines plant sizing.
#' - Sizing:System: Defines system sizing.
#' - Sizing:Zone: Defines zone sizing.
#' - SteamEquipment: Defines steam equipment.
#' - SurfaceConvectionAlgorithm:Inside: Defines the inside surface convection algorithm.
#' - SurfaceConvectionAlgorithm:Outside: Defines the outside surface convection algorithm.
#' - ThermostatSetpoint:DualSetpoint: Represents a dual setpoint thermostat.
#' - Timestep: Defines the simulation timestep.
#' - Version: Defines the version of the simulation.
#' - WaterHeater:HeatPump:PumpedCondenser: Represents a heat pump water heater with a pumped condenser.
#' - WaterHeater:Mixed: Represents a mixed water heater.
#' - WaterHeater:Sizing: Defines water heater sizing.
#' - WaterHeater:Stratified: Represents a stratified water heater.
#' - WaterUse:Connections: Defines water use connections.
#' - WaterUse:Equipment: Defines water use equipment.
#' - WindowProperty:FrameAndDivider: Defines window frame and divider properties.
#' - Windowmaterial:Blind: Represents a window blind material.
#' - Windowmaterial:Gas: Represents a window gas material.
#' - Windowmaterial:Glazing: Represents a window glazing material.
#' - Windowmaterial:Screen: Represents a window screen material.
#' - Windowmaterial:Shade: Represents a window shade material.
#' - Zone: Defines a zone in the building.
#' - ZoneAirContaminantBalance: Defines the air contaminant balance for a zone.
#' - ZoneAirHeatBalanceAlgorithm: Defines the air heat balance algorithm for a zone.
#' - ZoneControl:Thermostat: Represents a thermostat for zone control.
#' - ZoneHVAC:AirDistributionUnit: Represents an air distribution unit for zone HVAC.
#' - ZoneHVAC:Baseboard:Convective:Electric: Represents an electric convective baseboard for zone HVAC.
#' - ZoneHVAC:Baseboard:Convective:Water: Represents a water convective baseboard for zone HVAC.
#' - ZoneHVAC:Baseboard:RadiantConvective:Electric: Represents an electric radiant convective baseboard for zone HVAC.
#' - ZoneHVAC:Baseboard:RadiantConvective:Water: Represents a water radiant convective baseboard for zone HVAC.
#' - ZoneHVAC:Baseboard:RadiantConvective:Water:Design: Represents the design for a water radiant convective baseboard for zone HVAC.
#' - ZoneHVAC:EquipmentConnections: Defines equipment connections for zone HVAC.
#' - ZoneHVAC:EquipmentList: A list of equipment for zone HVAC.
#' - ZoneHVAC:IdealLoadsAirSystem: Represents an ideal loads air system for zone HVAC.
#' - ZoneHVAC:LowTemperatureRadiant:VariableFlow: Represents a low temperature radiant system with variable flow for zone HVAC.
#' - ZoneHVAC:LowTemperatureRadiant:VariableFlow:Design: Represents the design for a low temperature radiant system with variable flow for zone HVAC.
#' - ZoneInfiltration:DesignFlowRate: Defines the design flow rate for zone infiltration.
#' - ZoneInfiltration:EffectiveLeakageArea: Defines the effective leakage area for zone infiltration.
#' - ZoneInfiltration:FlowCoefficient: Defines the flow coefficient for zone infiltration.
#' - ZoneMixing: Defines zone mixing.
#' - ZoneVentilation:DesignFlowRate: Defines the design flow rate for zone ventilation.
#' - ZoneVentilation:WindandStackOpenArea: Defines the wind and stack open area for zone ventilation.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_idf_blocks
#' }
#' @family supporting dictionaries
"l_idf_blocks"



#' @title Activity Groups Data
#' @description This dataset contains information about different activity groups.
#' Each activity group is represented by an ID and a name.
#' @format A list of one data frame with 10 observations on the following 2 variables:
#' \describe{
#'   \item{id}{Integer. The unique identifier for each activity group.}
#'   \item{name}{Character. The name of the activity group.}
#' }
#' 
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_activity_groups
#' }
#' @family supporting dictionaries
"l_activity_groups"



#' @title Airflow Schedules Data
#' @description This dataset contains various airflow schedules used in simulations.
#' Each schedule includes blocks of data, limits, and a descriptive name.
#' @format A list of 8 elements, each containing the following components:
#' \describe{
#'   \item{blocks}{A list of blocks, where each block is a list of varying length.}
#'   \item{limits}{Character. The type of limits applied to the schedule (e.g., "Fraction", "Any Number", "On/Off").}
#'   \item{name}{Character. The descriptive name of the schedule.}
#' }
#' @examples
#' \dontrun{
#' # Access the first schedule's name
#' l_schedules_airflow[[1]]$name
#' 
#' # Access the limits of the second schedule
#' l_schedules_airflow[[2]]$limits
#' 
#' # Access the blocks of the third schedule
#' l_schedules_airflow[[3]]$blocks
#' }
#' @family supporting dictionaries
"l_schedules_airflow"


#' @title Schedule Limits Data
#' @description This dataset contains various schedule limits used in simulations.
#' Each limit includes a set of values that define the constraints for the schedule.
#' @format A list of 21 elements, each containing the following components:
#' \describe{
#'   \item{values}{A list of values, where each value can be a character, integer, or numeric.}
#' }
#' @examples
#' \dontrun{
#' # Access the first limit's values
#' l_schedules_limit[[1]]$values
#' 
#' # Access the values of the fourth limit
#' l_schedules_limit[[4]]$values
#' 
#' # Access the type of the sixth limit
#' l_schedules_limit[[6]]$values[[1]]
#' }
#' @family supporting dictionaries
"l_schedules_limit"


#' @title Appliance Rating Data
#' @description This dataset contains information about appliance ratings, including their carbon and energy colours, IDs, and ratings.
#' @format A list of one data frame with 11 observations on the following 4 variables:
#' \describe{
#'   \item{colour_carbon}{Character. The color code representing the carbon rating of the appliance.}
#'   \item{colour_energy}{Character. The color code representing the energy rating of the appliance.}
#'   \item{id}{Integer. The unique identifier for each appliance rating.}
#'   \item{rating}{Character. The rating of the appliance (e.g., "A++", "A+", "A", etc.).}
#' }
#' @examples
#' \dontrun{
#' # Access the first appliance's carbon color
#' l_appliance_rating$colour_carbon[1]
#' 
#' # Access the energy color of the second appliance
#' l_appliance_rating$colour_energy[2]
#' 
#' # Access the ID of the third appliance
#' l_appliance_rating$id[3]
#' 
#' # Access the rating of the fourth appliance
#' l_appliance_rating$rating[4]
#' }
#' @family supporting dictionaries
"l_appliance_rating" 


#' @title EPW Header Units Data
#' @description This dataset contains the header units for EPW (EnergyPlus Weather) files.
#' Each header unit includes a description, name, and unit of measurement.
#' @format A list of one data frame with 35 observations on the following 3 variables:
#' \describe{
#'   \item{description}{Character. The description of the header unit (e.g., "Year", "Month", "Day").}
#'   \item{name}{Character. The name of the header unit (e.g., "Year", "Month", "Day").}
#'   \item{units}{Character. The unit of measurement for the header unit (e.g., "year", "month", "day").}
#' }
#' @examples
#' \dontrun{
#' # Access the description of the first header unit
#' l_epw_header_units$description[1]
#' 
#' # Access the name of the second header unit
#' l_epw_header_units$name[2]
#' 
#' # Access the unit of the third header unit
#' l_epw_header_units$units[3]
#' 
#' # Access the description, name, and unit of the fourth header unit
#' l_epw_header_units$description[4]
#' l_epw_header_units$name[4]
#' l_epw_header_units$units[4]
#' }
#' @family supporting dictionaries
"l_epw_header_units" 


#' @title Housing Groups Data
#' @description This dataset contains various housing groups, each represented by a data frame with specific attributes.
#' The groups include activities, categories, compositions, dwelling tenures, dwelling types, fuel poverty statuses, household compositions, HRP ages, HRP employment statuses, individual ages, individual genders, lights, relationships, rooms, and typologies.
#' @format A list of 15 elements, each containing a data frame with the following variables:
#' \describe{
#'   \item{activity}{A data frame with 12 observations on 3 variables: \code{id} (character), \code{name} (character), \code{AC} (integer).}
#'   \item{category}{A data frame with 12 observations on 3 variables: \code{id} (character), \code{name} (character), \code{CA} (integer).}
#'   \item{composition}{A data frame with 3 observations on 3 variables: \code{id} (character), \code{name} (character), \code{CH} (integer).}
#'   \item{dwellingTenure}{A data frame with 2 observations on 3 variables: \code{id} (character), \code{name} (character), \code{TN} (integer).}
#'   \item{dwellingType}{A data frame with 5 observations on 3 variables: \code{id} (character), \code{name} (character), \code{TY} (integer).}
#'   \item{fuelPoverty}{A data frame with 3 observations on 3 variables: \code{id} (character), \code{name} (character), \code{FP} (integer).}
#'   \item{householdComposition}{A data frame with 5 observations on 3 variables: \code{id} (character), \code{name} (character), \code{HH} (integer).}
#'   \item{HRPAge}{A data frame with 3 observations on 3 variables: \code{id} (character), \code{name} (character), \code{AX} (integer).}
#'   \item{HRPEmploymentStatus}{A data frame with 3 observations on 3 variables: \code{id} (character), \code{name} (character), \code{WS} (integer).}
#'   \item{individualAge}{A data frame with 5 observations on 3 variables: \code{id} (character), \code{name} (character), \code{YR} (integer).}
#'   \item{individualGender}{A data frame with 2 observations on 3 variables: \code{id} (character), \code{name} (character), \code{GR} (integer).}
#'   \item{lights}{A data frame with 9 observations on 4 variables: \code{id} (character), \code{name} (character), \code{power} (numeric), \code{LT} (integer).}
#'   \item{relationships}{A data frame with 24 observations on 2 variables: \code{id} (integer), \code{name} (character).}
#'   \item{rooms}{A data frame with 33 observations on 3 variables: \code{id} (character), \code{name} (character), \code{ZN} (integer).}
#'   \item{typology}{A data frame with 4 observations on 3 variables: \code{id} (character), \code{name} (character), \code{LY} (integer).}
#' }
#' @examples
#' \dontrun{
#' # Access the first activity's name
#' l_housing_groups$activity$name[1]
#' 
#' # Access the category ID of the second category
#' l_housing_groups$category$id[2]
#' 
#' # Access the composition name of the third composition
#' l_housing_groups$composition$name[3]
#' 
#' # Access the dwelling tenure name of the first dwelling tenure
#' l_housing_groups$dwellingTenure$name[1]
#' 
#' # Access the power of the first light
#' l_housing_groups$lights$power[1]
#' }
#' @family supporting dictionaries
"l_housing_groups"



#' @title Base Schedules Data
#' @description This dataset contains various base schedules used in simulations.
#' Each schedule includes blocks of data, limits, and a descriptive name.
#' @format A list of 5 elements, each containing the following components:
#' \describe{
#'   \item{blocks}{A list of blocks, where each block is a list of varying length.}
#'   \item{limits}{Character. The type of limits applied to the schedule (e.g., "On/Off", "Fraction", "Any Number").}
#'   \item{name}{Character. The descriptive name of the schedule.}
#' }
#' @examples
#' \dontrun{
#' # Access the name of the first schedule
#' l_schedules_base[[1]]$name
#' 
#' # Access the limits of the second schedule
#' l_schedules_base[[2]]$limits
#' 
#' # Access the blocks of the third schedule
#' l_schedules_base[[3]]$blocks
#' 
#' # Access the name and limits of the fourth schedule
#' l_schedules_base[[4]]$name
#' l_schedules_base[[4]]$limits
#' 
#' # Access the blocks of the fifth schedule
#' l_schedules_base[[5]]$blocks
#' }
#' @family supporting dictionaries
"l_schedules_base"



#' @title People Schedules Data
#' @description This dataset contains various schedules related to people, including activity, work efficiency, clothing, air velocity, and base activity schedules.
#' Each schedule includes blocks of data, limits, and a descriptive name.
#' @format A list of 5 elements, each containing the following components:
#' \describe{
#'   \item{blocks}{A list of blocks, where each block is a list of varying length.}
#'   \item{limits}{Character. The type of limits applied to the schedule (e.g., "Any Number").}
#'   \item{name}{Character. The descriptive name of the schedule.}
#' }
#' @examples
#' \dontrun{
#' # Access the name of the first schedule
#' l_schedules_people[[1]]$name
#' 
#' # Access the limits of the second schedule
#' l_schedules_people[[2]]$limits
#' 
#' # Access the blocks of the third schedule
#' l_schedules_people[[3]]$blocks
#' 
#' # Access the name and limits of the fourth schedule
#' l_schedules_people[[4]]$name
#' l_schedules_people[[4]]$limits
#' 
#' # Access the blocks of the fifth schedule
#' l_schedules_people[[5]]$blocks
#' }
#' @family supporting dictionaries
"l_schedules_people" 



#' @title IDF Algorithm Data
#' @description This dataset contains a list of algorithms used in IDF (Input Data File) simulations.
#' Each algorithm is represented by a name.
#' @format A list of one element containing a character vector with 4 observations:
#' \describe{
#'   \item{algorithm}{Character. The name of the algorithm (e.g., "Simple", "TARP", "CeilingDiffuser", "AdaptiveConvectionAlgorithm").}
#' }
#' @examples
#' \dontrun{
#' # Access the first algorithm
#' l_idf_algorithm$algorithm[1]
#' 
#' # Access the second algorithm
#' l_idf_algorithm$algorithm[2]
#' 
#' # Access the third algorithm
#' l_idf_algorithm$algorithm[3]
#' 
#' # Access the fourth algorithm
#' l_idf_algorithm$algorithm[4]
#' }
#' @family supporting dictionaries
"l_idf_algorithm" 



#' @title Activity Data List
#' @description A list containing three data frames related to activities, their types, and intensity measures.
#' @format A list of 3 data frames:
#' \describe{
#'   \item{types}{A data frame with 290 observations of 6 variables:
#'     \describe{
#'       \item{id}{Integer: Activity ID}
#'       \item{name}{Character: Activity name}
#'       \item{in2000}{Logical: Indicator if the activity was present in the year 2000}
#'       \item{in2015}{Logical: Indicator if the activity was present in the year 2015}
#'       \item{inboth}{Logical: Indicator if the activity was present in both years}
#'       \item{group}{Character: Activity group}
#'     }
#'   }
#'   \item{intensity_abs}{A data frame with 25 observations of 3 variables:
#'     \describe{
#'       \item{hgh}{Numeric: High intensity measure}
#'       \item{nom}{Integer: Nominal intensity measure}
#'       \item{low}{Numeric: Low intensity measure}
#'     }
#'   }
#'   \item{intensity_rel}{A data frame with 24 observations of 3 variables:
#'     \describe{
#'       \item{hgh}{Integer: High relative intensity measure}
#'       \item{nom}{Numeric: Nominal relative intensity measure}
#'       \item{low}{Numeric: Low relative intensity measure}
#'     }
#'   }
#' }
#' @examples
#' \dontrun{
#' # Access the list
#' l_activities
#' 
#' # Access the 'types' data frame
#' l_activities$types
#' 
#' # Access the 'intensity_abs' data frame
#' l_activities$intensity_abs
#' 
#' # Access the 'intensity_rel' data frame
#' l_activities$intensity_rel
#' }
#' @family supporting dictionaries
"l_activities"


#' @title Low and zero carbon technology (LZC) descriptions
#' @description A list containing descriptions of different LZC system implementation into *EnergyPlus IDFs*, including biomass and solar collectors.
#' @format A list of 2 character vectors:
#' \describe{
#'   \item{biomass}{A character vector with 6 elements describing the biomass energy system.}
#'   \item{solar_collector}{A character vector with 26 elements describing the solar collector system for hot water.}
#' }
#' @examples
#' \dontrun{
#' # Access the list
#' l_idf_lzc
#' 
#' # Access the biomass description
#' l_idf_lzc$biomass
#' 
#' # Access the solar collector description
#' l_idf_lzc$solar_collector
#' }
#' @family supporting dictionaries
"l_idf_lzc"
