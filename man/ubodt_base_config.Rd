% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define-config.R
\name{ubodt_base_config}
\alias{ubodt_base_config}
\title{Define base configuration for precomputing an UBODT table}
\usage{
ubodt_base_config(output, delta = 3000, log_level = 2L, use_omp = FALSE)
}
\arguments{
\item{output}{Output file name.}

\item{delta}{Upper distance of routing (default: 3000, unit: map unit).}

\item{log_level}{Log level (default: 2 (infor)), 0-trace, 1-debug, 2-info,
3-warn, 4-err, 5-critical, 6-off.}

\item{use_omp}{If specified, run in multiple threads, which will be faster.}
}
\value{
A configuration object.
}
\description{
Define base configuration for precomputing an UBODT table
}
