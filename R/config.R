#' Define base configuration for precomputing an UBODT table
#'
#' @param output Output file name.
#' @param delta Upper distance of routing (default: 3000, unit: map unit).
#' @param log_level Log level (default: 2 (infor)), 0-trace, 1-debug, 2-info,
#' 3-warn, 4-err, 5-critical, 6-off.
#' @param use_omp If specified, run in multiple threads, which will be faster.
#'
#' @return A configuration object.
#' @export
#'
#' @examples
ubodt_config <- function(output,
                         delta = 3000,
                         log_level = 2L,
                         use_omp = FALSE) {
  # error handling
  checkmate::assert_path_for_output(output, overwrite = TRUE, extension = "txt")
  checkmate::assert_number(delta, lower = 0)
  checkmate::assert_choice(log_level, choices = 0L:6L)
  checkmate::assert_flag(use_omp)

  # initialize a config object
  config <- list(config = NULL)

  # update the config object
  config <- empty_config |>
    purrr::assign_in(
      list(1, "output"),
      list(file = output)
    ) |>
    purrr::assign_in(
      list(1, "parameters"),
      list(delta = delta)
    ) |>
    purrr::assign_in(
      list(1, "other"),
      list(log_level = log_level, use_omp = use_omp)
    )

  # return the updated config object
  config
}


#' Define base configuration for using the FMM map matching algorithm
#'
#' @param ubodt Ubodt file name.
#' @param output Output file name.
#' @param output_fields Output fields name, one or more in (opath, cpath, tpath,
#' ogeom, mgeom, pgeom, offset, error, spdist, tp, ep, all).
#' @param log_level Log level (default: 2 (infor)), 0-trace, 1-debug, 2-info,
#' 3-warn, 4-err, 5-critical, 6-off.
#' @param use_omp If specified, run map matching in multiple threads.
#' @param step Number of trajectories to report the progress of map matching.
#'
#' @return A configuration object.
#' @export
#'
#' @examples
fmm_base_config <- function(ubodt,
                            output,
                            output_fields = "all",
                            log_level = 2L,
                            use_omp = FALSE,
                            step = 100L) {
  # defenses
  checkmate::assert_file_exists(ubodt, extension = "txt")

  # create config object
  config <- .map_match_config(output, output_fields, log_level, use_omp, step)

  # update the config object
  config <- config |>
    purrr::assign_in(
      list(1, "input", "ubodt"),
      list(file = ubodt)
    )

  # return the updated config object
  config
}


#' Define base configuration for using the FMM map matching algorithm
#'
#' @param ubodt Ubodt file name.
#' @param output Output file name.
#' @param output_fields Output fields name, one or more in (opath, cpath, tpath,
#' ogeom, mgeom, pgeom, offset, error, spdist, tp, ep, all).
#' @param log_level Log level (default: 2 (infor)), 0-trace, 1-debug, 2-info,
#' 3-warn, 4-err, 5-critical, 6-off.
#' @param use_omp If specified, run map matching in multiple threads.
#' @param step Number of trajectories to report the progress of map matching.
#'
#' @return A configuration object.
#' @export
#'
#' @examples
fmm_config <- function(ubodt,
                       output,
                       output_fields = "all",
                       log_level = 2L,
                       use_omp = FALSE,
                       step = 100L) {
  # defenses
  checkmate::assert_file_exists(ubodt, extension = "txt")
  checkmate::assert_path_for_output(output, overwrite = TRUE, extension = "txt")
  checkmate::assert(
    checkmate::assert_choice(output_fields, choices = "all"),
    checkmate::assert_subset(
      output_fields,
      choices = c(
        "opath", "cpath", "tpath", "ogeom", "mgeom", "pgeom",
        "offset", "error", "spdist", "tp", "ep"
      ),
      empty.ok = FALSE
    )
  )
  checkmate::assert_choice(log_level, choices = 0L:6L)
  checkmate::assert_flag(use_omp)
  checmate::assert_count(step, positive = TRUE)

  # initialize a config object
  config <- list(config = NULL)

  # update the config object
  config <- config |>
    purrr::assign_in(
      list(1, "input", "ubodt"),
      list(file = ubodt)
    ) |>
    purrr::assign_in(
      list(1, "output"),
      list(
        file = output,
        fields = purrr::map(purrr::set_names(output_fields), ~ NULL)
      )
    ) |>
    purrr::assign_in(
      list(1, "other"),
      list(log_level = log_level, step = step)
    )

  if (use_omp) {
    config <- config |>
      purrr::assign_in(list(1, "other"), list(use_omp = NULL))
  }

  # return the updated config object
  config
}


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


#' Set output parameters
#'
#' @param file Output file name.
#' @param fields Output fields name.
#'
#' @return A list.
#' @export
#'
#' @examples
set_output_params <- function(file, fields = "opath") {
  # Error handling
  checkmate::assert_string(file)
  checkmate::assert_subset(
    fields,
    c(
      "opath", "cpath", "tpath",
      "ogeom", "mgeom", "pgeom",
      "offset", "error", "spdist",
      "tp", "ep", "all"
    )
  )
}
