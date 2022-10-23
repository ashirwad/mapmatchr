set_gps_params <- function(file,
                           id = "id",
                           geom = "geom",
                           gps_point = FALSE,
                           x = "x",
                           y = "y",
                           timestamp = "timestamp") {
  # Error handling
  checkmate::check_string(file)
  checkmate::check_string(id)
  checkmate::check_string(geom)
  checkmate::check_flag(gps_point)
  checkmate::check_string(x)
  checkmate::check_string(y)
  checkmate::check_string(timestamp)
}
