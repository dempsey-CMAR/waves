#' Apply grossrange test to select wave variables
#'
#' Applies the grossrange test to all wave height-period column pairs in
#' \code{dat}. Pairs are: significant_height_m with peak_period_s;
#' average_height_largest_10_percent_m with period_largest_10_percent_s;
#' average_height_largest_33_percent_m with period_largest_33_percent_s;
#' maximum_height_m with period_maximum_s. If any of these variables are
#' missing, the function with skip that pair with a message.
#'
#' For any other variable pairs, use \code{wv_test_grossrange()} instead.
#'
#' Wave period variables with values <= 0 are flagged as "Fail". Based on the
#' data files, it is likely that values of 0 and -0.1 are used as \code{NULL}
#' values. All other values are flagged as "Pass".
#'
#' For the zero-crossing parameters (average_height_largest_33_percent_m,
#' average_height_largest_10_percent_m, maximum_height_m and corresponding
#' periods), when wave period is <= 0, the corresponding wave height is almost
#' always 0 m. The converse is also true (when wave height is 0, the period is
#' <= 0). This suggests that 0 is used as a NULL value for wave height variables
#' in the WavesMon software.
#'
#' For the variables calculated from frequency (significant_height_m,
#' peak_period_s), wave height of 0 m more often corresponds to a non-zero
#' frequency.
#'
#' Wave height parameters are flagged as "Fail" if the wave height is 0 m *and*
#' the period is <= 0 s. All other variables are flagged as "Pass".
#'
#' Applies the grossrange test to wave direction
#' (sea_surface_wave_to_direction_degree). Observations are flagged as "Pass" if
#' they are >= 0 and <= 360, and are flagged as "Fail" otherwise.
#'
#' @param dat Data frame of wave variables in wide format.
#'
#' @param return_long_names Logical argument indicating whether to return the CF
#'   variable names (long names) or the short variable names.
#'
#' @return Returns \code{dat} with an additional grossrange_flag column for each
#'   wave variable. If any wave height or period variables are missing, the
#'   function will return \code{dat} and a message.
#'
#' @export

wv_test_grossrange_all_vars <- function(dat, return_long_names = FALSE) {

  dat <- dat %>%
    wv_assign_short_variable_names() %>%
    wv_test_grossrange(
      height_var = "significant_height_m",
      period_var = "peak_period_s"
    ) %>%
    wv_test_grossrange(
      height_var = "average_height_largest_10_percent_m",
      period_var = "period_largest_10_percent_s"
    ) %>%
    wv_test_grossrange(
      height_var = "average_height_largest_33_percent_m",
      period_var = "period_largest_33_percent_s"
    ) %>%
    wv_test_grossrange(
      height_var = "maximum_height_m",
      period_var = "period_maximum_s"
    ) %>%
    mutate(
      grossrange_flag_to_direction_degree = if_else(
        (to_direction_degree >= 0 & to_direction_degree <= 360), 1, 4
      ),
      grossrange_flag_to_direction_degree = ordered(
        grossrange_flag_to_direction_degree, levels = 1:4
      )
    )

  if(isTRUE(return_long_names)) dat <- wv_append_long_variable_names(dat)

  dat
}


#' Apply grossrange test to a pair of wave height and period variables
#'
#' The grossrange test is applied to paired wave height and period variables
#' (e.g., significant_height_m with peak_period_s;
#' average_height_largest_10_percent_m with period_largest_10_percent_s;
#' average_height_largest_33_percent_m with period_largest_33_percent_s;
#' maximum_height_m with period_maximum_s).
#'
#' Wave period variables with values <= 0 are flagged as "Fail". Based on the
#' data files, it is likely that values of 0 and -0.1 are used as \code{NULL}
#' values. All other values are flagged as "Pass".
#'
#' For the zero-crossing parameters (average_height_largest_33_percent_m,
#' average_height_largest_10_percent_m, maximum_height_m and corresponding
#' periods), when wave period is <= 0, the corresponding wave height is almost
#' always 0 m. The converse is also true (when wave height is 0, the period is
#' <= 0). This suggests that 0 is used as a NULL value for wave height variables
#' in the WavesMon software.
#'
#' For the variables calculated from frequency (significant_height_m,
#' peak_period_s), wave height of 0 m more often corresponds to a non-zero
#' frequency.
#'
#' Wave height parameters are flagged as "Fail" if the wave height is 0 m *and*
#' the period is <= 0 s. All other variables are flagged as "Pass".
#'
#' @param dat Data frame of wave variables in wide format.
#'
#' @param height_var Wave height variable for which to apply the grossrange test
#'   (in quotes).
#'
#' @param period_var Period variable for which to apply the grossrange test (in
#'   quotes).
#'
#' @return Returns \code{dat} with an additional grossrange_flag column for the
#'   specified wave height and period variables.
#'
#' @importFrom dplyr case_when mutate rename
#' @importFrom data.table :=
#'
#' @export

wv_test_grossrange <- function(dat, height_var, period_var) {

  if(!(height_var %in% colnames(dat))) {
    message(height_var, " not found in dat.\ngrossrange flag not applied")
    return(dat)
  }
  if(!(period_var %in% colnames(dat))) {
    message(period_var, " not found in dat.\ngrossrange flag not applied")
    return(dat)
  }

  dat %>%
    rename(
      height_m = {{ height_var }},
      period_s = {{ period_var }}
    ) %>%
    mutate(
      grossrange_flag_height = case_when(
        height_m  == 0 & period_s <= 0 ~ 4,
        height_m < 0 ~ 4,
        TRUE ~ 1
      ),

      grossrange_flag_period = if_else(period_s <= 0, 4, 1),

      grossrange_flag_height = ordered(grossrange_flag_height, levels = 1:4),
      grossrange_flag_period = ordered(grossrange_flag_period, levels = 1:4)
    ) %>%
    rename(
      "{height_var}" := height_m,
      "{period_var}" := period_s,
      "grossrange_flag_{height_var}" := grossrange_flag_height,
      "grossrange_flag_{period_var}" := grossrange_flag_period
    )
}



