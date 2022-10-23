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
