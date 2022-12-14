% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{set_gps_params}
\alias{set_gps_params}
\title{Set GPS parameters}
\usage{
set_gps_params(
  file,
  id = "id",
  geom = "geom",
  gps_point = FALSE,
  x = "x",
  y = "y",
  timestamp = "timestamp"
)
}
\arguments{
\item{file}{GPS file name.}

\item{id}{GPS id column name.}

\item{geom}{GPS geometry column name. Only applicable to GPS trajectory CSV file.}

\item{gps_point}{If specified read input data as GPS point, otherwise read input data as trajectory.}

\item{x}{GPS x column name. Only applicable to GPS point CSV file.}

\item{y}{GPS y column name. Only applicable to GPS point CSV file.}

\item{timestamp}{GPS timestamp column name.}
}
\value{
A list.
}
\description{
Set GPS parameters
}
