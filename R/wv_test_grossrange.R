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
#' @param wv_grossrange_table Data frame with at least 5 columns:
#'   \code{variable}: entries must match the names of the variables being tested
#'   in \code{dat}; \code{gr_min}: minimum acceptable value; \code{gr_max}:
#'   maximum accepted value ; \code{user_min}: minimum reasonable value;
#'   \code{user_max}: maximum reasonable value.
#'
#' @param return_long_names Logical argument indicating whether to return the CF
#'   variable names (long names) or the short variable names.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{wv_grossrange_table}.
#'
#' @inheritParams wv_pivot_vars_longer
#'
#' @return Returns \code{dat} with an additional grossrange_flag column for each
#'   wave variable. If any wave height or period variables are missing, the
#'   function will return \code{dat} and a message.
#'
#' @importFrom dplyr filter join_by left_join mutate select
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export
#'


wv_test_grossrange <- function(
    dat,
    wv_grossrange_table = NULL,
    county = NULL,
    first_pivot_col,
    last_pivot_col = last_col(),
    return_long_names = FALSE) {

  message("applying grossrange test")

  # check that not providing more than one county
  #county <- assert_county(dat, county, "qc_test_grossrange()")

  # import default thresholds from internal data file -----------------------
  if (is.null(wv_grossrange_table)) {

    wv_grossrange_table <- wv_thresholds %>%
      filter(qc_test == "grossrange", county == !!county | is.na(county)) %>%
      select(-c(qc_test, county)) %>%
      pivot_wider(values_from = "threshold_value", names_from = "threshold")

    # # reformat
    # user_thresh <- wv_grossrange_table %>%
    #   select(variable, contains("user")) %>%
    #   filter(!is.na(user_min) & !is.na(user_max))
    #
    # sensor_thresh <- wv_grossrange_table %>%
    #   select(sensor_type, variable, contains("sensor")) %>%
    #   filter(!is.na(sensor_type))
    #
    # wv_grossrange_table <- sensor_thresh %>%
    #   inner_join(user_thresh, by = "variable")
  }

  dat <- dat %>%
    wv_pivot_vars_longer(
      first_pivot_col = first_pivot_col,
      last_pivot_col = last_pivot_col
      ) %>%
    left_join(wv_grossrange_table, by = join_by(variable)) %>%
    mutate(
      grossrange_flag = case_when(
        value > gr_max | value < gr_min ~ 4,
        (value <= gr_max & value > user_max) |
          (value >= gr_min & value < user_min) ~ 3,
        value <= user_max | value >= user_min ~ 1,
        TRUE ~ 2
      ),
      grossrange_flag = ordered(grossrange_flag, levels = 1:4)
    ) %>%
    select(-c(gr_max, gr_min, user_max, user_min)) %>%
    pivot_wider(
      names_from = variable,
      values_from = c(value, grossrange_flag)
     # names_sort = TRUE
    )

  colnames(dat) <- str_remove_all(colnames(dat), "value_")

  dat <- dat %>%
    wv_test_grossrange_height_period(
      height_var = "significant_height_m",
      period_var = "peak_period_s",
      flag_col = "grossrange_flag_significant_height_m"
    ) %>%
    wv_test_grossrange_height_period(
      height_var = "average_height_largest_33_percent_m",
      period_var = "period_largest_33_percent_s",
      flag_col = "grossrange_flag_average_height_largest_33_percent_m"
    ) %>%
    wv_test_grossrange_height_period(
      height_var = "average_height_largest_10_percent_m",
      period_var = "period_largest_10_percent_s",
      flag_col = "grossrange_flag_average_height_largest_10_percent_m"
    ) %>%
    wv_test_grossrange_height_period(
      height_var = "maximum_height_m",
      period_var = "period_maximum_s",
      flag_col = "grossrange_flag_maximum_height_m"
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
#' @param flag_col Name of the height variable grossrange flag column (in
#'   quotes).
#'
#' @return Returns \code{dat} with updated flags for the height variable.
#'
#' @importFrom dplyr case_when mutate rename
#' @importFrom data.table :=
#'
#' @export

wv_test_grossrange_height_period <- function(
    dat, height_var, period_var, flag_col) {

  if(!(height_var %in% colnames(dat))) {
    message(height_var, " not found in dat.\ngrossrange flag not updated")
    return(dat)
  }
  if(!(period_var %in% colnames(dat))) {
    message(period_var, " not found in dat.\ngrossrange flag not updated")
    return(dat)
  }
  if(!(flag_col %in% colnames(dat))) {
    message(flag_col, " not found in dat.\ngrossrange flag not updated")
    return(dat)
  }

  dat %>%
    rename(
      height_m = {{ height_var }},
      period_s = {{ period_var }},
      grossrange_flag_height =  {{ flag_col }}
    ) %>%
    # height = 0 is not automatically suspect,
    # but it IS suspect if period is also < 0
    mutate(
      grossrange_flag_height = if_else(
        height_m  == 0 & period_s <= 0,
        ordered(4, levels = 1:4),
        grossrange_flag_height)
    ) %>%
    rename(
      "{height_var}" := height_m,
      "{period_var}" := period_s,
      "grossrange_flag_{height_var}" := grossrange_flag_height
      # "grossrange_flag_{period_var}" := grossrange_flag_period
    )
}



