#' @title Data from EHS
#'
#' @description This object contains the data retrieved from the English Housing Survey sources
#'
#' @format A list containing various datasets related to the English Housing Survey (EHS) for the year 2011:
#' - dwelling.and.household.information: Data on dwelling and household information.
#' - geometry: Data on the geometry of the dwellings.
#' - ventilation: Data on ventilation systems and parameters.
#' - other.heat.loss.elements: Data on other elements contributing to heat loss.
#' - space.heating: Data on space heating systems and usage.
#' - hot.water.system: Data on hot water systems.
#' - low.energy.lighting: Data on low energy lighting usage.
#' - complementary: Complementary data related to the survey.
#' - summarised: Summarised data from the survey.
#' - common: Common data elements used across different datasets.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_ehs_2011_ext
#' }
#' @family core data sources
"s_ehs_2011_ext"



#' @title Data from EHS
#'
#' @description This object contains the data retrieved from the English Housing Survey sources
#'
#' @format A list containing various datasets related to the English Housing Survey (EHS) for the year 2011, focusing on household data:
#' - aacode: Data on a specific aspect of household information by aacode
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_ehs_2011_hhd
#' }
#' @family core data sources
"s_ehs_2011_hhd"



#' @title Data from EHS
#'
#' @description This object contains the data retrieved from the English Housing Survey sources
#'
#' @format A list containing various datasets related to room characteristics from the English Housing Survey (EHS) for the year 2011.
#' - aacode: Unique identifier for the dwelling.
#' - dwtype7x: Type of dwelling.
#' - floor5x: Floor level of the dwelling.
#' - storeyx: Number of storeys in the dwelling.
#' - number_of_rooms: Total number of rooms in the dwelling.
#' - living.height: Height of the living room.
#' - living.width: Width of the living room.
#' - living.depth: Depth of the living room.
#' - living.is_present: Indicator if the living room is present.
#' - living.is_inspected: Indicator if the living room is inspected.
#' - living.room_function: Function of the living room.
#' - living.floor_level: Floor level of the living room.
#' - kitchen.height: Height of the kitchen.
#' - kitchen.width: Width of the kitchen.
#' - kitchen.depth: Depth of the kitchen.
#' - kitchen.is_present: Indicator if the kitchen is present.
#' - kitchen.is_inspected: Indicator if the kitchen is inspected.
#' - kitchen.room_function: Function of the kitchen.
#' - kitchen.floor_level: Floor level of the kitchen.
#' - bedroom.height: Height of the bedroom.
#' - bedroom.width: Width of the bedroom.
#' - bedroom.depth: Depth of the bedroom.
#' - bedroom.is_present: Indicator if the bedroom is present.
#' - bedroom.is_inspected: Indicator if the bedroom is inspected.
#' - bedroom.room_function: Function of the bedroom.
#' - bedroom.floor_level: Floor level of the bedroom.
#' - bathroom.height: Height of the bathroom.
#' - bathroom.is_present: Indicator if the bathroom is present.
#' - bathroom.is_inspected: Indicator if the bathroom is inspected.
#' - bathroom.floor_level: Floor level of the bathroom.
#' - circulation.height: Height of the circulation area.
#' - circulation.is_present: Indicator if the circulation area is present.
#' - circulation.is_inspected: Indicator if the circulation area is inspected.
#' - garage.is_present: Indicator if the garage is present.
#' - garage.floor_level: Floor level of the garage.
#' - balcony.is_present: Indicator if the balcony is present.
#' - balcony.floor_level: Floor level of the balcony.
#' - extra_rooms.is_present1: Indicator if the first extra room is present.
#' - extra_rooms.is_present2: Indicator if the second extra room is present.
#' - extra_rooms.is_present3: Indicator if the third extra room is present.
#' - extra_rooms.is_present4: Indicator if the fourth extra room is present.
#' - extra_rooms.is_present5: Indicator if the fifth extra room is present.
#' - extra_rooms.is_present6: Indicator if the sixth extra room is present.
#' - extra_rooms.is_present7: Indicator if the seventh extra room is present.
#' - extra_rooms_1.room_function: Function of the first extra room.
#' - extra_rooms_2.room_function: Function of the second extra room.
#' - extra_rooms_3.room_function: Function of the third extra room.
#' - extra_rooms_4.room_function: Function of the fourth extra room.
#' - extra_rooms_5.room_function: Function of the fifth extra room.
#' - extra_rooms_6.room_function: Function of the sixth extra room.
#' - extra_rooms_7.room_function: Function of the seventh extra room.
#' - extra_rooms_1.floor_level: Floor level of the first extra room.
#' - extra_rooms_2.floor_level: Floor level of the second extra room.
#' - extra_rooms_3.floor_level: Floor level of the third extra room.
#' - extra_rooms_4.floor_level: Floor level of the fourth extra room.
#' - extra_rooms_5.floor_level: Floor level of the fifth extra room.
#' - extra_rooms_6.floor_level: Floor level of the sixth extra room.
#' - extra_rooms_7.floor_level: Floor level of the seventh extra room.
#' - conservatory.window_type: Type of windows in the conservatory.
#' - conservatory.roof_type: Type of roof in the conservatory.
#' - conservatory.closable_door: Indicator if the conservatory has a closable door.
#' - conservatory.floor_area: Floor area of the conservatory.
#' - stairs.stairs_within_dwelling: Indicator if there are stairs within the dwelling.
#' - stairs.stairs_open_plan: Indicator if the stairs are open plan.
#' - radiator.fixed_radiator: Indicator if there is a fixed radiator.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_ehs_2011_rom
#' }
#' @family core data sources
"s_ehs_2011_rom"



#' @title Data from EHS
#'
#' @description This object contains the data retrieved from the **English Housing Survey** sources
#'
#' @format A list containing various datasets related to the English Housing Survey (EHS) for machine learning purposes:
#' - common: A tibble containing common data elements used across different datasets.
#' - mlearning: A tibble containing data specifically prepared for machine learning applications.
#' - merged: A tibble containing merged data from various sources for comprehensive analysis.
#'
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' s_ehs_ml
#' }
"s_ehs_ml"
