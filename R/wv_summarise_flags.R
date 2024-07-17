#' Generate summary table of flags
#'
#' @param dat Data frame of wave data with variables and grossrange ranges in a
#'   long format. Depth flag (\code{depth_trim_flag}) should be in a separate
#'   column.
#'
#' @param ... Optional argument. Column names (not quoted) from \code{dat} to
#'   use as grouping variables.
#'
#' @return Returns a data frame with the number and percent of observations
#'   assigned each flag value for each variable, qc_test, and groups in
#'   \code{...}.
#'
#' @importFrom dplyr arrange bind_rows group_by mutate n ungroup rename select
#'   summarise
#' @importFrom tidyr pivot_longer
#' @importFrom qaqcmar qc_assign_flag_labels
#'
#' @export

wv_summarise_flags <- function(dat, ...) {

  qc_summary <- dat %>%
    group_by(variable) %>%
    mutate(n_obs = n()) %>%
    ungroup() %>%
    group_by(variable, n_obs, grossrange_flag_value) %>%
    summarise(n_fl = n()) %>%
    ungroup() %>%
    mutate(
      qc_test = "grossrange",
      flag_value = grossrange_flag_value
    ) %>%
    select(qc_test, n_obs, variable, flag_value, n_fl)

  if("depth_trim_flag" %in% colnames(dat)) {

    depth_qc_summary <- dat %>%
      distinct(timestamp_utc, sensor_depth_below_surface_m, depth_trim_flag) %>%
      mutate(n_obs = n()) %>%
      group_by(n_obs, depth_trim_flag) %>%
      summarise(n_fl = n()) %>%
      mutate(
        qc_test = "depth_trim",
        variable = "sensor_depth_below_surface_m",
        flag_value = depth_trim_flag
      ) %>%
      ungroup() %>%
      select(qc_test, n_obs, variable, flag_value, n_fl)

    qc_summary <- qc_summary %>%
      bind_rows(depth_qc_summary)
  }

  qc_summary %>%
    mutate(n_percent = round(100 * n_fl / n_obs, digits = 2)) %>%
    qc_assign_flag_labels()  %>%
    select(-n_obs) %>%
    rename(n_flag = n_fl) %>%
    arrange(qc_test, variable, flag_value)
}
