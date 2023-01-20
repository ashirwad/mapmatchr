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
  config <- .create_map_match_config(
    output, output_fields, log_level, use_omp, step
  )

  # update the config object
  config <- config |>
    purrr::assign_in(
      list(1, "input", "ubodt"),
      list(file = ubodt)
    )

  # return the updated config object
  config
}


#' Define base configuration for using the STMatch map matching algorithm
#'
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
stmatch_base_config <- function(output,
                                output_fields = "all",
                                log_level = 2L,
                                use_omp = FALSE,
                                step = 100L) {
  # create config object
  config <- .create_map_match_config(
    output, output_fields, log_level, use_omp, step
  )

  # return the config object
  config
}


#' Define helper function to create base configuration for using the
#' map matching algorithms
#'
#' @param output Output file name.
#' @param output_fields Output fields name, one or more in (opath, cpath, tpath,
#' ogeom, mgeom, pgeom, offset, error, spdist, tp, ep, all).
#' @param log_level Log level (default: 2 (infor)), 0-trace, 1-debug, 2-info,
#' 3-warn, 4-err, 5-critical, 6-off.
#' @param use_omp If specified, run map matching in multiple threads.
#' @param step Number of trajectories to report the progress of map matching.
#'
#' @return A configuration object.
#' @noRd
.create_map_match_config <- function(output,
                                     output_fields = "all",
                                     log_level = 2L,
                                     use_omp = FALSE,
                                     step = 100L) {
  # defenses
  checkmate::assert_path_for_output(output, overwrite = TRUE, extension = "txt")
  checkmate::assert(
    checkmate::check_choice(output_fields, choices = "all"),
    checkmate::check_subset(
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
  checkmate::assert_count(step, positive = TRUE)

  # initialize a config object
  config <- list(config = NULL)

  # update the config object
  config <- config |>
    purrr::assign_in(
      list(1, "output"),
      list(
        file = output,
        fields = purrr::map(purrr::set_names(output_fields), ~NULL)
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
