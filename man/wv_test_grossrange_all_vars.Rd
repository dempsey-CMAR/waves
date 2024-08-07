% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wv_test_grossrange.R
\name{wv_test_grossrange_all_vars}
\alias{wv_test_grossrange_all_vars}
\title{Apply grossrange test to select wave variables}
\usage{
wv_test_grossrange_all_vars(dat, return_long_names = FALSE)
}
\arguments{
\item{dat}{Data frame of wave variables in wide format.}

\item{return_long_names}{Logical argument indicating whether to return the CF
variable names (long names) or the short variable names.}
}
\value{
Returns \code{dat} with an additional grossrange_flag column for each
  wave variable. If any wave height or period variables are missing, the
  function will return \code{dat} and a message.
}
\description{
Applies the grossrange test to all wave height-period column pairs in
\code{dat}. Pairs are: significant_height_m with peak_period_s;
average_height_largest_10_percent_m with period_largest_10_percent_s;
average_height_largest_33_percent_m with period_largest_33_percent_s;
maximum_height_m with period_maximum_s. If any of these variables are
missing, the function with skip that pair with a message.
}
\details{
For any other variable pairs, use \code{wv_test_grossrange()} instead.

Wave period variables with values <= 0 are flagged as "Fail". Based on the
data files, it is likely that values of 0 and -0.1 are used as \code{NULL}
values. All other values are flagged as "Pass".

For the zero-crossing parameters (average_height_largest_33_percent_m,
average_height_largest_10_percent_m, maximum_height_m and corresponding
periods), when wave period is <= 0, the corresponding wave height is almost
always 0 m. The converse is also true (when wave height is 0, the period is
<= 0). This suggests that 0 is used as a NULL value for wave height variables
in the WavesMon software.

For the variables calculated from frequency (significant_height_m,
peak_period_s), wave height of 0 m more often corresponds to a non-zero
frequency.

Wave height parameters are flagged as "Fail" if the wave height is 0 m *and*
the period is <= 0 s. All other variables are flagged as "Pass".

Applies the grossrange test to wave direction
(sea_surface_wave_to_direction_degree). Observations are flagged as "Pass" if
they are >= 0 and <= 360, and are flagged as "Fail" otherwise.
}
