% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wv_flag_sensor_depth_to_trim.R
\name{wv_flag_sensor_depth_to_trim}
\alias{wv_flag_sensor_depth_to_trim}
\title{Flag observations that should be trimmed from beginning and end of deployment}
\usage{
wv_flag_sensor_depth_to_trim(
  dat,
  depth_threshold = 1,
  return_depth_diff = FALSE
)
}
\arguments{
\item{dat}{Data frame of wave data for a single deployment in wide format.}

\item{depth_threshold}{The change in \code{sensor_depth_below_surface_m}
that will trigger a flag of 4 (in metres). Default is 1.0 m, the q95 of all
depth data.}

\item{return_depth_diff}{Logical argument indicating whether to return the
column of \code{depth_diff} = abs(lead(sensor_depth_below_surface_m) -
sensor_depth_below_surface_m).}
}
\value{
Returns \code{dat} with the flag column
  \code{trim_flag_sensor_depth_below_surface_m} (and optionally
  \code{depth_diff}.
}
\description{
A change in \code{sensor_depth_below_surface_m} greater than
\code{depth_threshold} at the beginning or end of the deployment will trigger
a flag of 4. This assumes that the sensor was recording during deployment or
retrieval, and all observations from this timestamp should be filtered out of
the analysis.
}
