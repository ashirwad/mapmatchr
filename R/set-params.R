#' Set network parameters
#'
#' @param config A configuration object.
#' @param file Network file name.
#' @param id Network id column name.
#' @param source Network source column name.
#' @param target Network target column name.
#' @param mode Network mode name. Only applies to OpenStreetMap (OSM) network.
#' Allowed values are one of drive|walk|bike|all.
#'
#' @return A list.
#' @export
#'
#' @examples
set_network_params <- function(config,
                               file,
                               id = "id",
                               source = "source",
                               target = "target",
                               mode = "drive") {
  # defenses
  checkmate::assert_list(config)
  checkmate::assert_file_exists(file, extension = "shp")
  checkmate::assert_string(id)
  checkmate::assert_string(source)
  checkmate::assert_string(target)
  checkmate::assert_choice(mode, choices = c("drive", "bike", "walk", "all"))

  # update config object
  config <- config |>
    purrr::assign_in(
      list(1, "input", "network"),
      list(file = file, id = id, source = source, target = target, mode = mode)
    )

  # return the updated config object
  config
}


#' Set GPS parameters
#'
#' @param config A configuration object.
#' @param file GPS file name.
#' @param id GPS id column name.
#' @param geom GPS geometry column name. Only applies to GPS trajectory
#' CSV file.
#' @param gps_point If specified read input data as GPS point, otherwise read
#' input data as trajectory.
#' @param x GPS x column name. Only applies to GPS point CSV file.
#' @param y GPS y column name. Only applies to GPS point CSV file.
#' @param timestamp GPS timestamp column name.
#'
#' @return A list.
#' @export
#'
#' @examples
set_gps_params <- function(config,
                           file,
                           id = "id",
                           geom = "geom",
                           gps_point = FALSE,
                           x = "x",
                           y = "y",
                           timestamp = "timestamp") {
  # defenses
  checkmate::assert_list(config)
  checkmate::assert_file_exists(file, extension = "csv")
  checkmate::assert_string(id)
  checkmate::assert_string(geom)
  checkmate::assert_flag(gps_point)
  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(timestamp)

  # update config object
  config  <- config |>
    purrr::assign_in(
      list(1, "input", "gps"),
      list(
        file = file, id = id, geom = geom, x = x, y = y, timestamp = timestamp
      )
    )

  if (gps_point) {
    config <- config |>
      purrr::assign_in(list(1, "input", "gps", "gps_point"), NULL)
  }

  # return the updated config object
  config
}


#' Set map matching parameters
#'
#' @param candidates Number of candidates.
#' @param radius Search radius.
#' @param error GPS sensor error.
#' @param factor Scale factor to limit shortest path search. Only applicable for stmatch.
#' @param vmax Maximum vehicle speed. Only applicable for stmatch.
#'
#' @return
#' @export
#'
#' @examples
set_map_match_params <- function(candidates = 8,
                                 radius = 300,
                                 error = 50,
                                 factor = 1.5,
                                 vmax = 30) {
  # Error handling
  checkmate::assert_int(candidates)
  checkmate::assert_number(radius)
  checkmate::assert_number(error)
  checkmate::assert_number(factor)
  checkmate::assert_number(vmax)
}
