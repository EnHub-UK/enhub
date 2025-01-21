#' @title Data from CARLtd-CHM
#'
#' @description This object contains the data retrieved from the Cambridge Housing Model sources
#'
#' @format A list containing various datasets related to construction, heating, and meteorological parameters:
#' - CHM_BaseGFFloorConstruction: Data on base ground floor construction.
#' - CHM_CookingSystemTypeFuel: Data on cooking system types and their fuel usage.
#' - CHM_CoolingSystemRatioParameters: Data on cooling system ratio parameters.
#' - CHM_DraughtLobby: Data on draught lobbies.
#' - CHM_DwellingLatitude: Data on dwelling latitudes.
#' - CHM_DwellingType: Data on dwelling types.
#' - CHM_EERandSEER: Data on energy efficiency ratings (EER) and seasonal energy efficiency ratings (SEER).
#' - CHM_EHS_ageBand: Data on age bands from the English Housing Survey (EHS).
#' - CHM_EHS_wallConstructionA: Data on wall construction types from the EHS (Type A).
#' - CHM_EHS_wallConstructionB: Data on wall construction types from the EHS (Type B).
#' - CHM_EHSRegion: Data on regions from the EHS.
#' - CHM_ElectricityFansPumps: Data on electricity usage for fans and pumps.
#' - CHM_ElectricityMechanicalVentilation: Data on electricity usage for mechanical ventilation.
#' - CHM_EnvelopeHeatTransferCoefficients: Data on envelope heat transfer coefficients.
#' - CHM_ExposedFloorConstruction: Data on exposed floor construction.
#' - CHM_ExposedFloorKvalue: Data on K-values for exposed floors.
#' - CHM_ExposedFloorUvalue: Data on U-values for exposed floors.
#' - CHM_FloorInsulationThickness: Data on floor insulation thickness.
#' - CHM_FloorUParameters: Data on U-parameters for floors.
#' - CHM_GroundFloorKvalue: Data on K-values for ground floors.
#' - CHM_HeatEmitter: Data on heat emitters.
#' - CHM_HeatingCodes: Data on heating codes.
#' - CHM_HeatingNames: Data on heating names.
#' - CHM_HeatingPeriodsLiving: Data on heating periods for living areas.
#' - CHM_HotDHWCircuitLosses: Data on hot domestic hot water (DHW) circuit losses.
#' - CHM_HotDHWCombiLoss: Data on combi boiler losses for DHW.
#' - CHM_HotDHWCommunityHeatFuel: Data on community heat fuel for DHW.
#' - CHM_HotDHWCommunityHeatTariff: Data on community heat tariffs for DHW.
#' - CHM_HotDHWCylinderInsulation: Data on insulation for DHW cylinders.
#' - CHM_HotDHWElectricSystem: Data on electric systems for DHW.
#' - CHM_HotDHWElectricTariff: Data on electric tariffs for DHW.
#' - CHM_HotDHWSolarCircuitLosses: Data on solar circuit losses for DHW.
#' - CHM_HotDHWSystem: Data on DHW systems.
#' - CHM_HotDHWSystemForm: Data on forms of DHW systems.
#' - CHM_HotDHWTemperatureFactor: Data on temperature factors for DHW.
#' - CHM_HotWaterUse: Data on hot water usage.
#' - CHM_InfiltrationParameters: Data on infiltration parameters.
#' - CHM_InternalCeilingKvalue: Data on K-values for internal ceilings.
#' - CHM_InternalFloorKvalue: Data on K-values for internal floors.
#' - CHM_InternalWallConstruction: Data on internal wall construction.
#' - CHM_InternalWallKvalue: Data on K-values for internal walls.
#' - CHM_LoftThickness: Data on loft insulation thickness.
#' - CHM_MainHeatingCommunityFuelType: Data on community fuel types for main heating.
#' - CHM_MainHeatingCommunityTariff: Data on community tariffs for main heating.
#' - CHM_MainHeatingControlAdj: Data on adjustments for main heating controls.
#' - CHM_MainHeatingControlCharging: Data on charging for main heating controls.
#' - CHM_MainHeatingControlType: Data on types of main heating controls.
#' - CHM_MainHeatingEfficiencyAdjustment: Data on efficiency adjustments for main heating.
#' - CHM_MainHeatingEfficiencySummer: Data on summer efficiency for main heating.
#' - CHM_MainHeatingElectricTariff: Data on electric tariffs for main heating.
#' - CHM_MainHeatingFlueType: Data on flue types for main heating.
#' - CHM_MainHeatingOilBoiler: Data on oil boilers for main heating.
#' - CHM_MainHeatingRequirement: Data on requirements for main heating.
#' - CHM_MainHeatingSystemForm: Data on forms of main heating systems.
#' - CHM_MainHeatingSystemResponse: Data on responses of main heating systems.
#' - CHM_MainHeatingSystemSurvey: Data on surveys of main heating systems.
#' - CHM_MainHeatingSystemType: Data on types of main heating systems.
#' - CHM_MonthlyAverageWind: Data on monthly average wind speeds.
#' - CHM_MonthlyExternalTemperature: Data on monthly external temperatures.
#' - CHM_MonthlyHorizSolarRadiation: Data on monthly horizontal solar radiation.
#' - CHM_MonthlySolarRadiation: Data on monthly solar radiation.
#' - CHM_OrientationCollector: Data on orientations of solar collectors.
#' - CHM_PartyInternalCeilingConstruction: Data on construction of party internal ceilings.
#' - CHM_PartyInternalFloorConstruction: Data on construction of party internal floors.
#' - CHM_PartyWallConstruction: Data on construction of party walls.
#' - CHM_PartyWallUKvalue: Data on U-values for party walls.
#' - CHM_RoofKvalue: Data on K-values for roofs.
#' - CHM_RoofType: Data on types of roofs.
#' - CHM_RoofUvalue: Data on U-values for roofs.
#' - CHM_RoofUvalueInsulation: Data on insulation U-values for roofs.
#' - CHM_RoofWindowUvalue: Data on U-values for roof windows.
#' - CHM_RoomInRoofType: Data on types of rooms in roofs.
#' - CHM_RoomInRoofUvalue: Data on U-values for rooms in roofs.
#' - CHM_SecondaryHeatingFraction: Data on fractions of secondary heating.
#' - CHM_SecondaryHeatingSystem: Data on secondary heating systems.
#' - CHM_Solar: Data on solar parameters.
#' - CHM_SolarDeclination: Data on solar declination.
#' - CHM_SolarHotWater: Data on solar hot water systems.
#' - CHM_SpaceCoolingParameters: Data on space cooling parameters.
#' - CHM_TemperatureHeatingPeriodLiving: Data on temperatures during heating periods for living areas.
#' - CHM_Tenure: Data on tenure types.
#' - CHM_Terrain: Data on terrain types.
#' - CHM_Type: Data on general types.
#' - CHM_VentilationParameters: Data on ventilation parameters.
#' - CHM_VentilationRate: Data on ventilation rates.
#' - CHM_VentilationSystem: Data on ventilation systems.
#' - CHM_WallKvalue: Data on K-values for walls.
#' - CHM_WallType: Data on types of walls.
#' - CHM_WallUvalueEngWal_semi: Data on U-values for semi-detached walls in England.
#' - CHM_WallUvalueEngWal: Data on U-values for walls in England.
#' - CHM_WallUvalueSco_semi: Data on U-values for semi-detached walls in Scotland.
#' - CHM_WallUvalueSco: Data on U-values for walls in Scotland.
#' - CHM_WindowFrameFactor: Data on factors for window frames.
#' - CHM_WindowFrameType: Data on types of window frames.
#' - CHM_WindowOrientation: Data on orientations of windows.
#' - CHM_WindowOvershading: Data on overshading of windows.
#' - CHM_WindowSolarAccess: Data on solar access for windows.
#' - CHM_WindowTransmittance: Data on transmittance of windows.
#' - CHM_WindowType: Data on types of windows.
#' - CHM_WindowUvalue: Data on U-values for windows.
#' - CHMtbl_EHS_CHM_ageBand_cal: Data on age bands from the EHS (calculated).
#' - CHMtbl_EHS_CHM_LivingAreaFraction: Data on fractions of living areas from the EHS.
#' - CHMtbl_EHS_CHM_wallThick: Data on wall thickness from the EHS.
#' - CHMtbl_EHS_CHM_wallType: Data on wall types from the EHS.
#' - CHM_LatitudeAdjustment: Data on latitude adjustments.
#' - CHM_MeanMonthlyVertRad: Data on mean monthly vertical radiation.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_chm
#' }
#' @family core data sources
"s_chm"



#' @title CHM Reference Data
#' @description This list contains various data frames related to construction materials, insulation, floor construction, wall types, windows, and other building components.
#' @format A list of 21 data frames:
#' \describe{
#'   \item{_ConsList}{Data frame with 5 observations and 16 variables: V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16.}
#'   \item{_MatList}{Data frame with 21 observations and 6 variables: Name, Roughness, Thickness..m., Conductivity..W.mK., Density..kg.m3., SpecificHeat..J.kgK..}
#'   \item{BasementGroundFloorInsulation}{Data frame with 11 observations and 3 variables: EngWal, Scot, Nire.}
#'   \item{ExposedFloorConstruction}{Data frame with 3 observations and 2 variables: Exposed.Floor.Construction, Exposed.Floor.K.value..kJ.m2K..}
#'   \item{ExposedFloorUvalue}{Data frame with 12 observations and 3 variables: AgeBand, X1.Exposed.Floor.U.value..W.m2K., X2.Exposed.Floor.U.value..W.m2K..}
#'   \item{FloorUParameters}{Data frame with 10 observations and 3 variables: name, value, units.}
#'   \item{GroundFloorConstruction}{Data frame with 4 observations and 2 variables: Type, Basement.Ground.Floor.K.Value..kJ.m2K..}
#'   \item{InternalConstructionK}{Data frame with 4 observations and 2 variables: Internal.Wall.Construction.and.K.value..kJ.m2K....SAP.Table.1e, K.value.}
#'   \item{PartyCeiling}{Data frame with 2 observations and 2 variables: name, value.}
#'   \item{PartyFloor}{Data frame with 7 observations and 2 variables: name, value.}
#'   \item{PartyWallConstruction}{Data frame with 7 observations and 3 variables: Party.Wall.Construction.U.value..W.m2K..and.K.value..kJ.m2K....SAP.Table.3.6.and.Table.1e, K.value, U.value.}
#'   \item{Roof}{Data frame with 12 observations and 1 variable: RoofValue.}
#'   \item{RoofConstruction}{Data frame with 4 observations and 2 variables: name, value.}
#'   \item{RoofRoom}{Data frame with 3 observations and 1 variable: name.}
#'   \item{RoofU}{Data frame with 11 observations and 7 variables: UvKn.Pitched, UvKn.Thatched, UvUn.Pitched, UvUn.Pitched.1, UvUn.Flat, UvAt.Pitched, UvAt.Thatched.}
#'   \item{WallType}{Data frame with 16 observations and 59 variables: id, Wall.Construction.Type, Wall.K.value..kJ.m2K....calculated.based.on.assumptions.in.construction.details, X.p...density..c...specific.heat.capacity., wllThick_m_1, wllThick_m_2, wllThick_m_3, wllThick_m_4, wllThick_m_5, wllThick_m_6, wllThick_m_7, wllThick_m_8, wllThick_m_9, wllThick_m_10, wllThick_m_11, wllUv_Wm2K_EngWal_01, wllUv_Wm2K_EngWal_02, wllUv_Wm2K_EngWal_03, wllUv_Wm2K_EngWal_04, wllUv_Wm2K_EngWal_05, wllUv_Wm2K_EngWal_06, wllUv_Wm2K_EngWal_07, wllUv_Wm2K_EngWal_08, wllUv_Wm2K_EngWal_09, wllUv_Wm2K_EngWal_10, wllUv_Wm2K_EngWal_11, wllUv_Wm2K_Sco_01, wllUv_Wm2K_Sco_02, wllUv_Wm2K_Sco_03, wllUv_Wm2K_Sco_04, wllUv_Wm2K_Sco_05, wllUv_Wm2K_Sco_06, wllUv_Wm2K_Sco_07, wllUv_Wm2K_Sco_08, wllUv_Wm2K_Sco_09, wllUv_Wm2K_Sco_10, wllUv_Wm2K_Sco_11, wllUvSem_Wm2K_EngWal_01, wllUvSem_Wm2K_EngWal_02, wllUvSem_Wm2K_EngWal_03, wllUvSem_Wm2K_EngWal_04, wllUvSem_Wm2K_EngWal_05, wllUvSem_Wm2K_EngWal_06, wllUvSem_Wm2K_EngWal_07, wllUvSem_Wm2K_EngWal_08, wllUvSem_Wm2K_EngWal_09, wllUvSem_Wm2K_EngWal_10, wllUvSem_Wm2K_EngWal_11, wllUvSem_Wm2K_Sco_01, wllUvSem_Wm2K_Sco_02, wllUvSem_Wm2K_Sco_03, wllUvSem_Wm2K_Sco_04, wllUvSem_Wm2K_Sco_05, wllUvSem_Wm2K_Sco_06, wllUvSem_Wm2K_Sco_07, wllUvSem_Wm2K_Sco_08, wllUvSem_Wm2K_Sco_09, wllUvSem_Wm2K_Sco_10, wllUvSem_Wm2K_Sco_11.}
#'   \item{Window}{Data frame with 9 observations and 14 variables: Window.Type, Type, Uv_Wood_NoC, Uv_Metal_NoC, Uv_uPVC_NoC, Uv_Wood_Eff, Uv_Metal_Eff, Uv_uPVC_Eff, Roof_Uv_Wood_NoC, Roof_Uv_Metal_NoC, Roof_Uv_uPVC_NoC, Roof_Uv_Wood_Eff, Roof_Uv_Metal_Eff, Roof_Uv_uPVC_Eff.}
#'   \item{WindowFrameType}{Data frame with 4 observations and 1 variable: name.}
#'   \item{WindowOrientation}{Data frame with 7 observations and 3 variables: Ori, grads, rads.}
#'   \item{WindowOvershading}{Data frame with 5 observations and 1 variable: Window.Overshading.}
#'   \item{WindowTransmittance}{Data frame with 9 observations and 3 variables: Window.Transmittance, Solar, Light.}
#' }
#' @examples
#' \dontrun{
#' # Accessing the _ConsList data frame
#' s_chm_reference$`_ConsList`
#'
#' # Summary of the _MatList data frame
#' summary(s_chm_reference$`_MatList`)
#' }
#' @family core data sources
"s_chm_reference"
