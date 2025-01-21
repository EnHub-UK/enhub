#' @title Data with room information
#'
#' @description This data frame contains information about various room layouts, including their IDs, codes, names, colours, descriptions, functions, setpoint temperatures, and category ratings.
#'
#' @format A data frame with 51 rows and 10 columns:
#' \describe{
#'   \item{id}{\code{<int>}: Unique identifier for each room layout.}
#'   \item{code}{\code{<chr>}: Code representing the room layout (e.g., "ZN000", "ZN001").}
#'   \item{name}{\code{<chr>}: Name of the room layout (e.g., "Living Room / Drawing", "Parlour").}
#'   \item{colour}{\code{<chr>}: Hex color code representing the color associated with the room layout (e.g., "#000000", "#5bb08a").}
#'   \item{description}{\code{<chr>}: Description of the room layout (e.g., "<void> used to represent complex geometries.", "A room for relaxing and socializing.").}
#'   \item{ehs.function}{\code{<chr>}: Function of the room layout in the context of energy and heating systems (e.g., "living").}
#'   \item{setpoint.temp}{\code{<int>}: Setpoint temperature for the room layout in degrees Celsius (e.g., 21, 18).}
#'   \item{cat.A}{\code{<dbl>}: Category A rating for the room layout (e.g., 1.5, 2.0).}
#'   \item{cat.B}{\code{<dbl>}: Category B rating for the room layout (e.g., 1.0, 1.5).}
#'   \item{cat.C}{\code{<dbl>}: Category C rating for the room layout (e.g., 0.5, 1.5).}
#' }
#' 
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' d_rooms
#' }
#' @family room layouts
"d_rooms"



#' @title A tupple with room/layout information
#'
#' @description This tupple contains information about various room layouts, including their locations, sizes, shapes, and proportions of different room types.
#'
#' @format A list of 50 elements, each representing a different layout. Each layout is a list with the following components:
#' \describe{
#'   \item{loc}{\code{<int>}: A vector of integers representing the locations of the rooms within the layout.}
#'   \item{size}{\code{<list>}: A list containing the dimensions of the layout:
#'     \describe{
#'       \item{cols}{\code{<int>}: Number of columns in the layout.}
#'       \item{rows}{\code{<int>}: Number of rows in the layout.}
#'     }
#'   }
#' }
#' The following example shows the structure of layout no. 069:
#' ```json
#' {
#'   "loc": [9, 61, 23, 27],
#'   "size": {
#'       "cols": [2],
#'       "rows": [2]
#'     },
#'   "shape": ["R"],
#'   "proportion": [
#'       {
#'         "name": "Stairwell",
#'         "pct": 0.25
#'       },
#'       {
#'         "name": "Bathroom",
#'         "pct": 0.25
#'       },
#'       {
#'         "name": "Bedroom Single",
#'         "pct": 0.25
#'       },
#'       {
#'         "name": "Bedroom Single (B)",
#'         "pct": 0.25
#'       }
#'     ]
#' }
#' ```
#' 
#' @examples
#' \dontrun{
#' # Displaying the whole list
#' t_layouts
#' 
#' # Displaying the layout 069
#' t_layouts$lay_069 %>% jsonlite::toJSON(pretty = TRUE)
#' }
#' @family room layouts
"t_layouts"



#' @title Linear Models for Room Layouts
#' @description This list contains multiple linear models related to room layouts, including models for the number of rooms, number of storeys, living level, and living function.
#' @format A list of 16 elements, each containing a linear model with the following components:
#' \describe{
#'   \item{n_rooms}{A linear model for the number of rooms.}
#'   \item{n_storeys}{A linear model for the number of storeys.}
#'   \item{living_level}{A linear model for the living level.}
#'   \item{living_function}{A linear model for the living function.}
#'   \item{kitchen_level}{A linear model for the kitchen level.}
#'   \item{kitchen_function}{A linear model for the kitchen function.}
#'   \item{bedroom_level}{A linear model for the bedroom level.}
#'   \item{bedroom_function}{A linear model for the bedroom function.}
#'   \item{bathroom_level}{A linear model for the bathroom level.}
#'   \item{extra_room_1 }{A linear model for the extra room 1.}
#'   \item{extra_room_2}{A linear model for the extra room 2.}
#'   \item{extra_room_3}{A linear model for the extra room 3.}
#'   \item{extra_room_4}{A linear model for the extra room 4.}
#'   \item{extra_room_5}{A linear model for the extra room 5.}
#'   \item{extra_room_6}{A linear model for the extra room 6.}
#'   \item{extra_room_7}{A linear model for the extra room 7.}
#' }
#' Each linear model contains the following components:
#' \describe{
#'   \item{coefficients}{Named numeric vector of coefficients.}
#'   \item{residuals}{Named numeric vector of residuals.}
#'   \item{effects}{Named numeric vector of effects.}
#'   \item{rank}{Integer rank of the model.}
#'   \item{fitted.values}{Named numeric vector of fitted values.}
#'   \item{assign}{Integer vector of variable assignments.}
#'   \item{qr}{List containing QR decomposition components.}
#'   \item{df.residual}{Integer degrees of freedom of residuals.}
#'   \item{contrasts}{List of contrasts used in the model.}
#'   \item{xlevels}{List of factor levels used in the model.}
#'   \item{call}{The matched call.}
#'   \item{terms}{The terms object used.}
#'   \item{model}{The model frame.}
#' }
#' @examples
#' \dontrun{
#' # Access the linear model for the number of rooms
#' lm_n_rooms <- l_room_lm$n_rooms
#' 
#' # View the coefficients of the model
#' lm_n_rooms$coefficients
#' 
#' # Access the residuals of the model
#' lm_n_rooms$residuals
#' 
#' # Access the fitted values of the model
#' lm_n_rooms$fitted.values
#' }
"l_room_lm"
