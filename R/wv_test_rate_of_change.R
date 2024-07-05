#' Flag observations with rapidly changing sensor_depth_below_surface_m
#'
#' A change in \code{sensor_depth_below_surface_m} greater than
#' \code{roc_depth_threshold} will trigger a flag of 3 (Suspect/Of Interest).
#' The flag will be upgraded to 4 (Fail), if it is assigned to the first or last
#' row. This assumes that the sensor was recording during deployment or
#' retrieval, and all observations from this timestamp should be filtered out of
#' the analysis.
#'
#' @param dat Dataframe of wave data for a single deployment in wide format.
#'
#' @param roc_depth_threshold The change in \code{sensor_depth_below_surface_m}
#'   that will trigger a flag of 3 (in metres). If the first or last row are
#'   assigned a flag of 3, this will be upgraded to a flag of 4. This assumes
#'   that the sensor was recording during deployment or retrieval.
#'
#' @param return_depth_diff Logical argument indicating whether to return the
#'   column of \code{depth_diff} = lead(sensor_depth_below_surface_m) -
#'   sensor_depth_below_surface_m.
#'
#' @return Returns \code{dat} with the flag column
#'   \code{rate_of_change_flag_sensor_depth_below_surface_m} (and optionally
#'   \code{depth_diff}.
#'
#' @importFrom dplyr case_when lag lead mutate row_number
#'
#' @export

wv_test_rate_of_change_depth <- function(
    dat, roc_depth_threshold = 1, return_depth_diff = FALSE
) {

  # can change this to warning OR just pivot when consequences are understood
  if("variable" %in% colnames(dat)) {
    stop("dat must be in wide format")
  }

  dat <- dat %>%
   # select(timestamp_utc, sensor_depth_below_surface_m) %>%
    # distinct() %>%
    mutate(
      depth_diff = lead(sensor_depth_below_surface_m) - sensor_depth_below_surface_m,
      depth_diff = abs(depth_diff),
      rate_of_change_flag = case_when(
        depth_diff > roc_depth_threshold ~ 3, # when sensor is being lowered
        lag(depth_diff) > roc_depth_threshold ~ 3, # when sensor is being retrieved
        TRUE ~ 1
      ),
      rate_of_change_flag = if_else(
        (row_number() == 1 | row_number() == nrow(dat)) &
          rate_of_change_flag == 3, 4, rate_of_change_flag
      )
    ) %>%
    rename(rate_of_change_flag_sensor_depth_below_surface_m = rate_of_change_flag)

  if(isFALSE(return_depth_diff)) dat <- dat %>% select(-depth_diff)

  dat


}
