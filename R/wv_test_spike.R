#' Apply the spike test to wave parameters
#'
#' @param dat Data frame of wave data in wide format.
#'
#' @param wv_spike_table Data frame with at least 3 columns: \code{variable}:
#'   should match the names of the variables being tested in \code{dat}.
#'   \code{spike_low}: maximum acceptable spike value to "Pass", and
#'   \code{spike_high}: maximum acceptable value to be flagged "Suspect/Of
#'   Interest".
#'
#'   Default values are used if \code{wv_spike_table = NULL}. To see the
#'   default \code{wv_spike_table}, type \code{subset(wv_thresholds,
#'   qc_test == "spike")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{wv_spike_table}.
#'
#' @param keep_spike_cols  Logical value. If \code{TRUE}, the columns used to
#'   produce the spike value are returned in \code{dat}. Default is
#'   \code{FALSE}.
#'
#' @return Returns \code{dat} in a wide format, with spike flag columns for each
#'   variable in the form "spike_flag_variable".
#'
#' @family tests
#'
#' @importFrom dplyr %>% arrange case_when contains group_by if_else lag lead
#'   left_join mutate select
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export

wv_test_spike <- function(
    dat,
    county = NULL,
    wv_spike_table = NULL,
    keep_spike_cols = FALSE
) {

  message("applying spike test")

  # import default thresholds from internal data file
  if (is.null(wv_spike_table)) {

    wv_spike_table <- wv_thresholds %>%
      filter(qc_test == "spike", county == !!county | is.na(county)) %>%
      select(-c(qc_test, county)) %>%
      pivot_wider(values_from = "threshold_value", names_from = "threshold")
  }

  # add thresholds to dat and assign flags ---------------------------------------------------
  dat <- dat %>%
    wv_pivot_vars_longer() %>%
    left_join(wv_spike_table, by = "variable") %>%
    group_by(station, deployment_id, variable) %>%
    dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
    mutate(
      lag_value = lag(value),
      lead_value = lead(value),
      spike_ref = (lag_value + lead_value) / 2,
      spike_value = abs(value - spike_ref),
      spike_flag = case_when(
        spike_value > spike_high ~ 4,
        (spike_value <= spike_high & spike_value > spike_low) ~ 3,
        spike_value <= spike_low ~ 1,
        TRUE ~ 2
      ),
      spike_flag = ordered(spike_flag, levels = 1:4)
    ) %>%
    ungroup()

  if(isFALSE(keep_spike_cols)) {
    dat <- dat %>%
      select(-c(lag_value, lead_value,
                spike_ref, spike_value,
                spike_high, spike_low))
  }

  dat <- dat %>%
    pivot_wider(
      names_from = variable,
      values_from = c(value, spike_flag),
      names_sort = TRUE
    )

  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  dat
}
