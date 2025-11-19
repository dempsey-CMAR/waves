#' Pivot flagged wave data longer by variable
#'
#' @param dat Data frame of flagged wave data in wide format.
#'
#' @param vars Vector of character strings indicating which columns to pivot.
#'   Default is all variables (with column names in the "short" format). Only
#'   required if variables in \code{dat} are in a wide format.
#'
#' @param qc_tests Quality control tests included in \code{dat_wide}. If
#'   \code{dat_wide} only includes the max flag, use \code{qc_tests = "qc"}.
#'
#' @return Returns \code{dat_wide}, with variables and flags pivoted to a long
#'   format.
#'
#' @importFrom dplyr %>% any_of arrange filter relocate select
#' @importFrom tidyr pivot_longer
#'
#' @export

wv_pivot_flags_longer <- function(dat, qc_tests = NULL, vars = NULL) {

  if (is.null(qc_tests)) {
    qc_tests <- c(
      "grossrange",
     # "crossref",
      "rolling_sd",
      "spike",
      "qc"
    )
  }

  qc_tests <- tolower(qc_tests)

  if(!("variable" %in% colnames(dat))) {
    dat <- wv_pivot_vars_longer(dat, vars = vars)
  }

  # pivot the flags indicated
  if ("grossrange" %in% qc_tests) {
    dat <- wv_pivot_single_test_longer(dat, qc_test = "grossrange")
  }

  # pivot the flags indicated
  if ("crossref" %in% qc_tests) {
    dat <- wv_pivot_single_test_longer(dat, qc_test = "crossref")
  }

  if ("rolling_sd" %in% qc_tests) {
    dat <- wv_pivot_single_test_longer(dat, qc_test = "rolling_sd")
  }

  if ("spike" %in% qc_tests) {
    dat <- wv_pivot_single_test_longer(dat, qc_test = "spike")
  }

  if ("qc" %in% qc_tests) {
    dat <- wv_pivot_single_test_longer(dat, qc_test = "qc")
  }

  # #don't need to pivot this test rn
  # if ("human_in_loop" %in% qc_tests) {
  #   dat <- pivot_flags_longer(dat, qc_test = "human_in_loop")
  # }

  # don't arrange by deployment_range (because it will be alphabetical not chronological)
  dat #%>%
   # arrange(county, station, deployment_id, variable, timestamp_utc)
}


#' Complete pivot_longer of flagged wave data
#'
#' @param dat_wide Data frame of flagged wave data with the variables
#'   in a long format and flags in wide format.
#'
#' @param qc_test Flag columns to pivot.
#'
#' @return Returns \code{dat_wide} with the qc_test flag columns pivoted to a
#'   long format.
#'
#' @importFrom dplyr %>% contains filter select
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
#'
#' @export


wv_pivot_single_test_longer <- function(dat_wide, qc_test) {

  col_name <- paste0(qc_test, "_flag_variable")

  dat_wide %>%
    pivot_longer(
      cols = contains(qc_test),
      names_to = paste0(qc_test, "_flag_variable"),
      names_prefix = paste0(qc_test, "_flag_"),
      values_to = paste0(qc_test, "_flag_value"),
      values_drop_na = TRUE
    ) %>%
    filter(`variable` == !!sym(col_name)) %>%
    select(-!!sym(col_name))
}
