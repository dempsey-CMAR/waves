#' Apply multiple quality control tests to wave data
#'
#' @param dat Data frame of wave data in a wide format.
#'
#' @param qc_tests Character vector of quality control tests to apply to
#'   \code{dat}. Defaults to all available tests: \code{qc_tests =
#'   c("grossrange", "rolling_sd", "spike")}.
#'
#' @inheritParams wv_test_grossrange
#' @inheritParams wv_test_rolling_sd
#' @inheritParams wv_test_spike
#'
#' @return Returns \code{dat} with additional quality control flag columns.
#'
#' @importFrom dplyr %>% arrange distinct left_join
#' @importFrom purrr reduce
#'
#' @export

wv_test_all <- function(
    dat,
    qc_tests = NULL,
    county = NULL,

    wv_grossrange_table = NULL,
    wv_rolling_sd_table = NULL,
    wv_spike_table = NULL,

    period_hours = 24,
    max_interval_hours = 2,
    align_window = "center",
    keep_sd_cols = FALSE,
    keep_spike_cols = FALSE
) {

  if (is.null(qc_tests)) {
    qc_tests <- c("grossrange", "rolling_sd", "spike")
  }

  qc_tests <- tolower(qc_tests)

  # # use for the join and to order columns in output
  depl_cols <- c(
    "county",
    "waterbody",
    "station",
    "lease",
    "latitude" ,
    "longitude" ,
    "deployment_id",
    "timestamp_utc",
    "depth_trim_flag",
    "trim_obs"
  )

  #  use for the join and to order columns in output
  var_cols <- dat %>%
    wv_pivot_vars_longer() %>%
    distinct(variable) %>%
    arrange()
  var_cols <- var_cols$variable

  # apply tests
  dat_out <- list()


  if ("grossrange" %in% qc_tests) {
    dat_out[[1]] <- wv_test_grossrange(
      dat,
      wv_grossrange_table = wv_grossrange_table,
      county = county
    )
  }

  if ("rolling_sd" %in% qc_tests) {
    dat_out[[2]] <- wv_test_rolling_sd(
      dat,
      wv_rolling_sd_table = wv_rolling_sd_table,
      county = county,

      period_hours = period_hours,
      max_interval_hours = max_interval_hours,
      align_window = align_window,
      keep_sd_cols = keep_sd_cols
    )
  }

  if("spike" %in% qc_tests) {
    dat_out[[3]] <- wv_test_spike(
      dat,
      county = county,
      wv_spike_table = wv_spike_table,
      keep_spike_cols = keep_spike_cols
    )
  }

  # remove empty list elements
  #dat_out <- Filter(Negate(is.null), dat_out)

  # join results from each test
  join_cols <- c(
    depl_cols[which(depl_cols %in% colnames(dat))], var_cols)

  # join by all common columns
  dat_out <- dat_out %>%
    purrr::reduce(dplyr::left_join, by = join_cols)

  dat_out
}

