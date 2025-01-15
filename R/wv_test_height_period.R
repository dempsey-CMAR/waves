#' Additional qc for height and period variables
#'
#' @param dat Data frame of wave variables in wide format.
#'
#' @return Returns \code{dat} with flags for the height and period variables.
#' @export
#'

wv_test_crossref <- function(dat) {

  message("applying height-period crossref test")

  dat %>%
    wv_test_height_period (
      height_var = "significant_height_m",
      period_var = "peak_period_s"
    ) %>%
    wv_test_height_period (
      height_var = "average_height_largest_33_percent_m",
      period_var = "period_largest_33_percent_s"
    ) %>%
    wv_test_height_period (
      height_var = "average_height_largest_10_percent_m",
      period_var = "period_largest_10_percent_s"
    ) %>%
    wv_test_height_period (
      height_var = "maximum_height_m",
      period_var = "period_maximum_s"
    )
}


#' Additional qc for height and period variables
#'
#' This cross-reference test is applied to paired wave height and period
#' variables (e.g., significant_height_m with peak_period_s;
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
#' @param height_var Wave height variable for which to apply the crossref test
#'   (in quotes).
#'
#' @param period_var Period variable for which to apply the crossref test (in
#'   quotes).
#'
#' @return Returns \code{dat} with updated flags for the height variable.
#'
#' @importFrom dplyr case_when mutate rename
#' @importFrom data.table :=
#'
#' @export

wv_test_height_period <- function(dat, height_var, period_var) {

  if(!(height_var %in% colnames(dat))) {
    message(height_var, " not found in dat.\ngrossrange flag not updated")
    return(dat)
  }
  if(!(period_var %in% colnames(dat))) {
    message(period_var, " not found in dat.\ngrossrange flag not updated")
    return(dat)
  }

  height_flag_name <- paste0("crossref_flag_", height_var)
  period_flag_name <- paste0("crossref_flag_", period_var)

  dat %>%
    rename(
      height_m = {{ height_var }},
      period_s = {{ period_var }}
      #  height_flag = all_of(height_flag_name),
      #  period_flag = all_of(period_flag_name)
    ) %>%
    # height = 0 is not automatically suspect,
    # but it IS suspect if period is also < 0
    mutate(
      height_flag = if_else(
        height_m  <= 0 & period_s <= 0,
        ordered(4, levels = 1:4),
        ordered(1, levels = 1:4)
      ),

      period_flag = if_else(
        period_s <= 0,
        ordered(4, levels = 1:4),
        ordered(1, levels = 1:4)
      )
    ) %>%
    rename(
      "{height_var}" := height_m,
      "{period_var}" := period_s,
      "crossref_flag_{height_var}" := height_flag,
      "crossref_flag_{period_var}" := period_flag
    )
}



