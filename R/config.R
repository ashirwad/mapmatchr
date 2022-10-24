#' Set network parameters
#'
#' @param file Network file name.
#' @param id Network id column name.
#' @param source Network source column name.
#' @param target Network target column name.
#' @param mode Network mode name. Only applicable to OpenStreetMap (OSM) network.
#'
#' @return A list.
#' @export
#'
#' @examples
set_network_params <- function(file,
                               id = "id",
                               source = "source",
                               target = "target",
                               mode = "drive") {
  # Error handling
  checkmate::assert_string(file)
  checkmate::assert_string(id)
  checkmate::assert_string(source)
  checkmate::assert_string(target)
  checkmate::assert_string(mode)
}


#' Set GPS parameters
#'
#' @param file GPS file name.
#' @param id GPS id column name.
#' @param geom GPS geometry column name. Only applicable to GPS trajectory CSV file.
#' @param gps_point If specified read input data as GPS point, otherwise read input data as trajectory.
#' @param x GPS x column name. Only applicable to GPS point CSV file.
#' @param y GPS y column name. Only applicable to GPS point CSV file.
#' @param timestamp GPS timestamp column name.
#'
#' @return A list.
#' @export
#'
#' @examples
set_gps_params <- function(file,
                           id = "id",
                           geom = "geom",
                           gps_point = FALSE,
                           x = "x",
                           y = "y",
                           timestamp = "timestamp") {
  # Error handling
  checkmate::assert_string(file)
  checkmate::assert_string(id)
  checkmate::assert_string(geom)
  checkmate::assert_flag(gps_point)
  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(timestamp)
}
