#' @title Dictionary with Heat Loss Parameters (HLP) information
#'
#' @description This object contains a dictionary with Heat Loss Parameters (HLP) information
#'
#' @format List of tibbles containing construction and material data for building energy modelling.
#' - HLP_constructionsOpaque: Tibble containing data on opaque constructions. This tibble has 38 rows and 8 columns. Each row represents a different type of opaque construction.
#'   - name: The name of the construction (e.g., "stone: granite or whin (as built)").
#'   - outside_layer: The material used for the outside layer (e.g., "200 mm granite block").
#'   - layer_2: The material used for the second layer (e.g., "13 mm dense plaster").
#'   - layer_3: The material used for the third layer (e.g., "-").
#'   - layer_4: The material used for the fourth layer (e.g., "-").
#'   - layer_5: The material used for the fifth layer (e.g., "-").
#'   - code: A unique code for the construction (e.g., 10001).
#'   - source: The source of the data (e.g., "CIBSE Guide A").
#' - HLP_constructionsTranslucent: Tibble containing data on translucent constructions. This tibble has 219 rows and 10 columns. Each row represents a different type of translucent construction.
#'   - name: The name of the construction (e.g., "Composite 2-Core Filled Concrete Block Insulated").
#'   - outside_layer: The material used for the outside layer (e.g., "Composite 2-Core Filled Concrete Block Insulated #3").
#'   - layer_2: The material used for the second layer (e.g., "Composite 2-Core Filled Concrete Block Insulated #2").
#'   - layer_3: The material used for the third layer (e.g., "Composite 2-Core Filled Concrete Block Insulated #1").
#'   - layer_4: The material used for the fourth layer (e.g., "-").
#'   - layer_5: The material used for the fifth layer (e.g., "-").
#'   - layer_6: The material used for the sixth layer (e.g., "-").
#'   - layer_7: The material used for the seventh layer (e.g., "-").
#'   - code: A unique code for the construction (e.g., 20001).
#'   - source: The source of the data (e.g., "EnergyPlus Datasets").
#' - HLP_materialsOpaque: Tibble containing data on opaque materials. This tibble has 298 rows and 13 columns. Each row represents a different type of opaque material.
#'   - type: The type of material (e.g., "Material:AirGap").
#'   - name: The name of the material (e.g., "F04 Wall air space resistance").
#'   - roughness: The roughness of the material (e.g., "-").
#'   - thickness__m_: The thickness of the material in meters (e.g., "-").
#'   - conductivity__w_m_k_: The thermal conductivity of the material in W/m·K (e.g., "-").
#'   - density__kg_m3_: The density of the material in kg/m³ (e.g., "-").
#'   - specific_heat__j_kg_k_: The specific heat of the material in J/kg·K (e.g., "-").
#'   - thermal_resistance__m2_k_w_: The thermal resistance of the material in m²·K/W (e.g., "0.15").
#'   - thermal_absorptance: The thermal absorptance of the material (e.g., "-").
#'   - solar_absorptance: The solar absorptance of the material (e.g., "-").
#'   - visible_absorptance: The visible absorptance of the material (e.g., "-").
#'   - code: A unique code for the material (e.g., 1).
#'   - source: The source of the data (e.g., "EnergyPlus Datasets").
#' - HLP_materialsTranslucent: Tibble containing data on translucent materials. This tibble has 128 rows and 72 columns. Each row represents a different type of translucent material.
#'   - type: The type of material (e.g., "Material").
#'   - name: The name of the material (e.g., "Composite 2-Core Filled Concrete Block Insulated #1").
#'   - airflow_permeability__dimensionless_: The airflow permeability of the material (e.g., "-").
#'   - angle_of_resolution_for_screen_transmittance_output_map__deg_: The angle of resolution for screen transmittance output map in degrees (e.g., "-").
#'   - back_side_infrared_hemispherical_emissivity: The back side infrared hemispherical emissivity of the material (e.g., "-").
#'   - back_side_slat_beam_solar_reflectance: The back side slat beam solar reflectance of the material (e.g., "-").
#'   - back_side_slat_beam_visible_reflectance: The back side slat beam visible reflectance of the material (e.g., "-").
#'   - back_side_slat_diffuse_solar_reflectance__dimensionless_: The back side slat diffuse solar reflectance of the material (e.g., "-").
#'   - back_side_slat_diffuse_visible_reflectance__dimensionless_: The back side slat diffuse visible reflectance of the material (e.g., "-").
#'   - back_side_slat_infrared_hemispherical_emissivity: The back side slat infrared hemispherical emissivity of the material (e.g., "-").
#'   - back_side_solar_reflectance_at_normal_incidence: The back side solar reflectance at normal incidence of the material (e.g., "-").
#'   - back_side_visible_reflectance_at_normal_incidence: The back side visible reflectance at normal incidence of the material (e.g., "-").
#'   - blind_bottom_opening_multiplier: The blind bottom opening multiplier of the material (e.g., "-").
#'   - blind_left_side_opening_multiplier: The blind left side opening multiplier of the material (e.g., "-").
#'   - blind_right_side_opening_multiplier: The blind right side opening multiplier of the material (e.g., "-").
#'   - blind_to_glass_distance__m_: The blind to glass distance in meters (e.g., "-").
#'   - blind_top_opening_multiplier: The blind top opening multiplier of the material (e.g., "-").
#'   - bottom_opening_multiplier__dimensionless_: The bottom opening multiplier of the material (e.g., "-").
#'   - conductivity__w_m_k_: The thermal conductivity of the material in W/m·K (e.g., "0.88").
#'   - density__kg_m3_: The density of the material in kg/m³ (e.g., "1774.75").
#'   - diffuse_solar_reflectance: The diffuse solar reflectance of the material (e.g., "-").
#'   - diffuse_visible_reflectance: The diffuse visible reflectance of the material (e.g., "-").
#'   - front_side_infrared_hemispherical_emissivity: The front side infrared hemispherical emissivity of the material (e.g., "-").
#'   - front_side_slat_beam_solar_reflectance: The front side slat beam solar reflectance of the material (e.g., "-").
#'   - front_side_slat_beam_visible_reflectance: The front side slat beam visible reflectance of the material (e.g., "-").
#'   - front_side_slat_diffuse_solar_reflectance: The front side slat diffuse solar reflectance of the material (e.g., "-").
#'   - front_side_slat_diffuse_visible_reflectance: The front side slat diffuse visible reflectance of the material (e.g., "-").
#'   - front_side_slat_infrared_hemispherical_emissivity: The front side slat infrared hemispherical emissivity of the material (e.g., "-").
#'   - front_side_solar_reflectance_at_normal_incidence: The front side solar reflectance at normal incidence of the material (e.g., "-").
#'   - front_side_visible_reflectance_at_normal_incidence: The front side visible reflectance at normal incidence of the material (e.g., "-").
#'   - gas_type: The type of gas used in the material (e.g., "-").
#'   - infrared_hemispherical_emissivity__dimensionless_: The infrared hemispherical emissivity of the material (e.g., "-").
#'   - infrared_transmittance__dimensionless_: The infrared transmittance of the material (e.g., "-").
#'   - infrared_transmittance_at_normal_incidence: The infrared transmittance at normal incidence of the material (e.g., "-").
#'   - left_side_opening_multiplier__dimensionless_: The left side opening multiplier of the material (e.g., "-").
#'   - optical_data_type: The optical data type of the material (e.g., "-").
#'   - reflected_beam_transmittance_accounting_method: The reflected beam transmittance accounting method of the material (e.g., "-").
#'   - right_side_opening_multiplier__dimensionless_: The right side opening multiplier of the material (e.g., "-").
#'   - roughness: The roughness of the material (e.g., "Mediumrough").
#'   - screen_material_diameter__m_: The screen material diameter in meters (e.g., "-").
#'   - screen_material_spacing__m_: The screen material spacing in meters (e.g., "-").
#'   - screen_to_glass_distance__m_: The screen to glass distance in meters (e.g., "-").
#'   - shade_to_glass_distance: The shade to glass distance of the material (e.g., "-").
#'   - slat_angle__deg_: The slat angle in degrees (e.g., "-").
#'   - slat_beam_solar_transmittance: The slat beam solar transmittance of the material (e.g., "-").
#'   - slat_beam_visible_transmittance: The slat beam visible transmittance of the material (e.g., "-").
#'   - slat_conductivity__w_m_k_: The slat conductivity of the material in W/m·K (e.g., "-").
#'   - slat_diffuse_solar_transmittance: The slat diffuse solar transmittance of the material (e.g., "-").
#'   - slat_diffuse_visible_transmittance: The slat diffuse visible transmittance of the material (e.g., "-").
#'   - slat_infrared_hemispherical_transmittance: The slat infrared hemispherical transmittance of the material (e.g., "-").
#'   - slat_orientation: The slat orientation of the material (e.g., "-").
#'   - slat_separation__m_: The slat separation in meters (e.g., "-").
#'   - slat_thickness__m_: The slat thickness in meters (e.g., "-").
#'   - slat_width__m_: The slat width in meters (e.g., "-").
#'   - solar_absorptance: The solar absorptance of the material (e.g., "0.7").
#'   - solar_reflectance__dimensionless_: The solar reflectance of the material (e.g., "-").
#'   - solar_transmittance__dimensionless_: The solar transmittance of the material (e.g., "-").
#'   - solar_transmittance_at_normal_incidence: The solar transmittance at normal incidence of the material (e.g., "-").
#'   - specific_heat: The specific heat of the material (e.g., "838").
#'   - thermal_absorptance: The thermal absorptance of the material (e.g., "0.9").
#'   - thermal_hemispherical_emissivity__dimensionless_: The thermal hemispherical emissivity of the material (e.g., "-").
#'   - thickness__m_: The thickness of the material in meters (e.g., "0.079").
#'   - top_opening_multiplier__dimensionless_: The top opening multiplier of the material (e.g., "-").
#'   - visible_absorptance: The visible absorptance of the material (e.g., "0.7").
#'   - visible_reflectance__dimensionless_: The visible reflectance of the material (e.g., "-").
#'   - visible_transmittance__dimensionless_: The visible transmittance of the material (e.g., "-").
#'   - visible_transmittance_at_normal_incidence: The visible transmittance at normal incidence of the material (e.g., "-").
#'   - window_glass_spectral_data_set_name: The window glass spectral data set name of the material (e.g., "-").
#'   - minimum_slat_angle__deg_: The minimum slat angle in degrees (e.g., "-").
#'   - maximum_slat_angle__deg_: The maximum slat angle in degrees (e.g., "-").
#'   - code: A unique code for the material (e.g., 1001).
#'   - source: The source of the data (e.g., "EnergyPlus Datasets").
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' l_HLP
#' }
"l_HLP"
