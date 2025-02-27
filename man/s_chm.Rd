% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-chm.R
\docType{data}
\name{s_chm}
\alias{s_chm}
\title{Data from CARLtd-CHM}
\format{
A list containing various datasets related to construction, heating, and meteorological parameters:
\itemize{
\item CHM_BaseGFFloorConstruction: Data on base ground floor construction.
\item CHM_CookingSystemTypeFuel: Data on cooking system types and their fuel usage.
\item CHM_CoolingSystemRatioParameters: Data on cooling system ratio parameters.
\item CHM_DraughtLobby: Data on draught lobbies.
\item CHM_DwellingLatitude: Data on dwelling latitudes.
\item CHM_DwellingType: Data on dwelling types.
\item CHM_EERandSEER: Data on energy efficiency ratings (EER) and seasonal energy efficiency ratings (SEER).
\item CHM_EHS_ageBand: Data on age bands from the English Housing Survey (EHS).
\item CHM_EHS_wallConstructionA: Data on wall construction types from the EHS (Type A).
\item CHM_EHS_wallConstructionB: Data on wall construction types from the EHS (Type B).
\item CHM_EHSRegion: Data on regions from the EHS.
\item CHM_ElectricityFansPumps: Data on electricity usage for fans and pumps.
\item CHM_ElectricityMechanicalVentilation: Data on electricity usage for mechanical ventilation.
\item CHM_EnvelopeHeatTransferCoefficients: Data on envelope heat transfer coefficients.
\item CHM_ExposedFloorConstruction: Data on exposed floor construction.
\item CHM_ExposedFloorKvalue: Data on K-values for exposed floors.
\item CHM_ExposedFloorUvalue: Data on U-values for exposed floors.
\item CHM_FloorInsulationThickness: Data on floor insulation thickness.
\item CHM_FloorUParameters: Data on U-parameters for floors.
\item CHM_GroundFloorKvalue: Data on K-values for ground floors.
\item CHM_HeatEmitter: Data on heat emitters.
\item CHM_HeatingCodes: Data on heating codes.
\item CHM_HeatingNames: Data on heating names.
\item CHM_HeatingPeriodsLiving: Data on heating periods for living areas.
\item CHM_HotDHWCircuitLosses: Data on hot domestic hot water (DHW) circuit losses.
\item CHM_HotDHWCombiLoss: Data on combi boiler losses for DHW.
\item CHM_HotDHWCommunityHeatFuel: Data on community heat fuel for DHW.
\item CHM_HotDHWCommunityHeatTariff: Data on community heat tariffs for DHW.
\item CHM_HotDHWCylinderInsulation: Data on insulation for DHW cylinders.
\item CHM_HotDHWElectricSystem: Data on electric systems for DHW.
\item CHM_HotDHWElectricTariff: Data on electric tariffs for DHW.
\item CHM_HotDHWSolarCircuitLosses: Data on solar circuit losses for DHW.
\item CHM_HotDHWSystem: Data on DHW systems.
\item CHM_HotDHWSystemForm: Data on forms of DHW systems.
\item CHM_HotDHWTemperatureFactor: Data on temperature factors for DHW.
\item CHM_HotWaterUse: Data on hot water usage.
\item CHM_InfiltrationParameters: Data on infiltration parameters.
\item CHM_InternalCeilingKvalue: Data on K-values for internal ceilings.
\item CHM_InternalFloorKvalue: Data on K-values for internal floors.
\item CHM_InternalWallConstruction: Data on internal wall construction.
\item CHM_InternalWallKvalue: Data on K-values for internal walls.
\item CHM_LoftThickness: Data on loft insulation thickness.
\item CHM_MainHeatingCommunityFuelType: Data on community fuel types for main heating.
\item CHM_MainHeatingCommunityTariff: Data on community tariffs for main heating.
\item CHM_MainHeatingControlAdj: Data on adjustments for main heating controls.
\item CHM_MainHeatingControlCharging: Data on charging for main heating controls.
\item CHM_MainHeatingControlType: Data on types of main heating controls.
\item CHM_MainHeatingEfficiencyAdjustment: Data on efficiency adjustments for main heating.
\item CHM_MainHeatingEfficiencySummer: Data on summer efficiency for main heating.
\item CHM_MainHeatingElectricTariff: Data on electric tariffs for main heating.
\item CHM_MainHeatingFlueType: Data on flue types for main heating.
\item CHM_MainHeatingOilBoiler: Data on oil boilers for main heating.
\item CHM_MainHeatingRequirement: Data on requirements for main heating.
\item CHM_MainHeatingSystemForm: Data on forms of main heating systems.
\item CHM_MainHeatingSystemResponse: Data on responses of main heating systems.
\item CHM_MainHeatingSystemSurvey: Data on surveys of main heating systems.
\item CHM_MainHeatingSystemType: Data on types of main heating systems.
\item CHM_MonthlyAverageWind: Data on monthly average wind speeds.
\item CHM_MonthlyExternalTemperature: Data on monthly external temperatures.
\item CHM_MonthlyHorizSolarRadiation: Data on monthly horizontal solar radiation.
\item CHM_MonthlySolarRadiation: Data on monthly solar radiation.
\item CHM_OrientationCollector: Data on orientations of solar collectors.
\item CHM_PartyInternalCeilingConstruction: Data on construction of party internal ceilings.
\item CHM_PartyInternalFloorConstruction: Data on construction of party internal floors.
\item CHM_PartyWallConstruction: Data on construction of party walls.
\item CHM_PartyWallUKvalue: Data on U-values for party walls.
\item CHM_RoofKvalue: Data on K-values for roofs.
\item CHM_RoofType: Data on types of roofs.
\item CHM_RoofUvalue: Data on U-values for roofs.
\item CHM_RoofUvalueInsulation: Data on insulation U-values for roofs.
\item CHM_RoofWindowUvalue: Data on U-values for roof windows.
\item CHM_RoomInRoofType: Data on types of rooms in roofs.
\item CHM_RoomInRoofUvalue: Data on U-values for rooms in roofs.
\item CHM_SecondaryHeatingFraction: Data on fractions of secondary heating.
\item CHM_SecondaryHeatingSystem: Data on secondary heating systems.
\item CHM_Solar: Data on solar parameters.
\item CHM_SolarDeclination: Data on solar declination.
\item CHM_SolarHotWater: Data on solar hot water systems.
\item CHM_SpaceCoolingParameters: Data on space cooling parameters.
\item CHM_TemperatureHeatingPeriodLiving: Data on temperatures during heating periods for living areas.
\item CHM_Tenure: Data on tenure types.
\item CHM_Terrain: Data on terrain types.
\item CHM_Type: Data on general types.
\item CHM_VentilationParameters: Data on ventilation parameters.
\item CHM_VentilationRate: Data on ventilation rates.
\item CHM_VentilationSystem: Data on ventilation systems.
\item CHM_WallKvalue: Data on K-values for walls.
\item CHM_WallType: Data on types of walls.
\item CHM_WallUvalueEngWal_semi: Data on U-values for semi-detached walls in England.
\item CHM_WallUvalueEngWal: Data on U-values for walls in England.
\item CHM_WallUvalueSco_semi: Data on U-values for semi-detached walls in Scotland.
\item CHM_WallUvalueSco: Data on U-values for walls in Scotland.
\item CHM_WindowFrameFactor: Data on factors for window frames.
\item CHM_WindowFrameType: Data on types of window frames.
\item CHM_WindowOrientation: Data on orientations of windows.
\item CHM_WindowOvershading: Data on overshading of windows.
\item CHM_WindowSolarAccess: Data on solar access for windows.
\item CHM_WindowTransmittance: Data on transmittance of windows.
\item CHM_WindowType: Data on types of windows.
\item CHM_WindowUvalue: Data on U-values for windows.
\item CHMtbl_EHS_CHM_ageBand_cal: Data on age bands from the EHS (calculated).
\item CHMtbl_EHS_CHM_LivingAreaFraction: Data on fractions of living areas from the EHS.
\item CHMtbl_EHS_CHM_wallThick: Data on wall thickness from the EHS.
\item CHMtbl_EHS_CHM_wallType: Data on wall types from the EHS.
\item CHM_LatitudeAdjustment: Data on latitude adjustments.
\item CHM_MeanMonthlyVertRad: Data on mean monthly vertical radiation.
}
}
\usage{
s_chm
}
\description{
This object contains the data retrieved from the Cambridge Housing Model sources
}
\examples{
\dontrun{
# Displaying the whole list
s_chm
}
}
\seealso{
Other core data sources: 
\code{\link{s_bredem}},
\code{\link{s_chm_reference}},
\code{\link{s_ehs_2011_ext}},
\code{\link{s_ehs_2011_hhd}},
\code{\link{s_ehs_2011_rom}},
\code{\link{s_sap}},
\code{\link{s_tus}}
}
\concept{core data sources}
\keyword{datasets}
