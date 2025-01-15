#' Apply grossrange test to wave variables
#'
#' @param dat Data frame of wave variables in wide format.
#'
#' @param wv_grossrange_table Data frame with at least 5 columns:
#'   \code{variable}: entries must match the names of the variables being tested
#'   in \code{dat}; \code{gr_min}: minimum acceptable value; \code{gr_max}:
#'   maximum accepted value ; \code{user_min}: minimum reasonable value;
#'   \code{user_max}: maximum reasonable value.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{wv_grossrange_table}.
#'
#' @inheritParams wv_pivot_vars_longer
#'
#' @return Returns \code{dat} with an additional grossrange_flag column for each
#'   wave variable.
#'
#' @importFrom dplyr filter join_by left_join mutate select
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export


wv_test_grossrange <- function(
    dat,
    wv_grossrange_table = NULL,
    county = NULL,
    vars = NULL
    ) {

  message("applying grossrange test")

  # check that not providing more than one county
  #county <- assert_county(dat, county, "qc_test_grossrange()")

  # import default thresholds from internal data file -----------------------
  if (is.null(wv_grossrange_table)) {

    wv_grossrange_table <- wv_thresholds %>%
      filter(qc_test == "grossrange", county == !!county | is.na(county)) %>%
      select(-c(qc_test, county)) %>%
      pivot_wider(values_from = "threshold_value", names_from = "threshold")

  }

  dat <- dat %>%
    wv_pivot_vars_longer() %>%
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
      values_from = c(value, grossrange_flag),
      names_sort = TRUE
    )

  colnames(dat) <- str_remove_all(colnames(dat), "value_")

  dat
}

