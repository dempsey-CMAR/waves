#' Apply quality control flags to wave height and period observations
#'
#' @param dat
#'
#' @return
#' @export

wv_apply_grossrange_test <- function(dat) {

  dat %>%
    wv_assign_short_variable_names() %>%
    apply_grossrange_test(
      height_var = "significant_height_m",
      period_var = "peak_period_s"
    ) %>%
    apply_grossrange_test(
      height_var = "average_height_largest_10_percent_m",
      period_var = "period_largest_10_percent_s"
    ) %>%
    apply_grossrange_test(
      height_var = "average_height_largest_33_percent_m",
      period_var = "period_largest_33_percent_s"
    ) %>%
    apply_grossrange_test(
      height_var = "maximum_height_m",
      period_var = "period_maximum_s"
    )

}



apply_grossrange_test <- function(dat, height_var, period_var) {

  dat %>%
    rename(
      height_m = {{ height_var }},
      period_s = {{ period_var }}
    ) %>%
    mutate(
      grossrange_flag_height = if_else(height_m  == 0 & period_s <= 0, 4, 1),
      grossrange_flag_period = if_else(period_s <= 0, 4, 1)
    ) %>%
    rename(
      "{height_var}" := height_m,
      "{period_var}" := period_s,
      "grossrange_flag_{height_var}" := grossrange_flag_height,
      "grossrange_flag_{period_var}" := grossrange_flag_period
    )
}



