#' Flag observations that should be trimmed from beginning and end of deployment
#'
#' A change in \code{sensor_depth_below_surface_m} greater than
#' \code{depth_threshold} at the beginning or end of the deployment will trigger
#' a flag of 4. This assumes that the sensor was recording during deployment or
#' retrieval, and all observations from this timestamp should be filtered out of
#' the analysis.
#'
#' @param dat Data frame of wave data for a single deployment in wide format.
#'
#' @param roc_depth_threshold The change in \code{sensor_depth_below_surface_m}
#'   that will trigger a flag of 4 (in metres). Default is 1.0 m, the q95 of all
#'   depth data.
#'
#' @param return_depth_diff Logical argument indicating whether to return the
#'   column of \code{depth_diff} = abs(lead(sensor_depth_below_surface_m) -
#'   sensor_depth_below_surface_m).
#'
#' @return Returns \code{dat} with the flag column
#'   \code{trim_flag_sensor_depth_below_surface_m} (and optionally
#'   \code{depth_diff}.
#'
#' @importFrom dplyr case_when lag lead mutate row_number
#'
#' @export

wv_flag_sensor_depth_to_trim <- function(
    dat, depth_threshold = 1, return_depth_diff = FALSE
) {

  # can change this to warning OR just pivot when consequences are understood
  if("variable" %in% colnames(dat)) stop("dat must be in wide format")

  dat <- dat %>%
    mutate(
      depth_diff = abs(
        lead(sensor_depth_below_surface_m) - sensor_depth_below_surface_m),

      depth_trim_flag = case_when(
        depth_diff > depth_threshold ~ 3, # when sensor is being lowered
        lag(depth_diff) > depth_threshold ~ 3, # when sensor is being retrieved
        TRUE ~ 1
      ),
      depth_trim_flag = if_else(
        (row_number() == 1 |
           row_number() == 2 |
           row_number() == nrow(dat) - 1 |
           row_number() == nrow(dat)) &
          depth_trim_flag == 3, 4, depth_trim_flag),

      depth_trim_flag = ordered(depth_trim_flag, levels = 1:4)
    )

  if(isFALSE(return_depth_diff)) dat <- dat %>% select(-depth_diff)

  dat
}
