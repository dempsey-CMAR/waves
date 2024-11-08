#' Flag observations that should be trimmed from beginning and end of deployment
#'
#' A change in \code{sensor_depth_below_surface_m} greater than
#' \code{depth_threshold} at the beginning or end of the deployment will trigger
#' a flag of 4. This assumes that the sensor was recording before or after the
#' ADCP was deployed. Observations of all variables at this timestamp should be
#' filtered out of the dataset.
#'
#' @param dat Data frame of wave data for a single deployment in wide format.
#'
#' @param depth_threshold The change in \code{sensor_depth_below_surface_m} that
#'   will trigger a flag of 4 (in metres). Default is 1.0 m, the q95 of all
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
#' @importFrom dplyr lag lead mutate n row_number slice_head
#'   slice_tail
#'
#' @export

wv_start_end_obs_to_trim <- function(
    dat, depth_threshold = 1, return_depth_diff = FALSE
) {

  # can change this to warning OR just pivot when consequences are understood
  if("variable" %in% colnames(dat)) stop("dat must be in wide format")

  dat_og <- dat

  join_cols <- colnames(dat_og)

  dat_trim <- dat %>%
    slice_head(n = 3) %>%
    mutate(group = "start") %>%
    rbind(
      dat %>%
        slice_tail(n = 3) %>%
        mutate(group = "end")
    ) %>%
    group_by(group) %>%
    mutate(
      depth_diff = abs(
        lead(sensor_depth_below_surface_m) - sensor_depth_below_surface_m),

      depth_diff = if_else(
        row_number() == n() & group == "end",
        abs(sensor_depth_below_surface_m - lag(sensor_depth_below_surface_m)),
        depth_diff
      ),

      trim_obs = if_else(depth_diff > depth_threshold, 4,  1)
    ) %>%
    ungroup() %>%
    select(-group)


  dat_out <- dat_og %>%
    left_join(dat_trim, by = join_cols) %>%
    mutate(
      trim_obs = if_else(is.na(trim_obs), 1, trim_obs),
      trim_obs = ordered(trim_obs, levels = 1:4) # for plotting
    )

  if(isFALSE(return_depth_diff)) dat_out <- dat_out %>% select(-depth_diff)

  dat_out

#   dat <- dat %>%
#     mutate(
#       depth_diff = abs(
#         lead(sensor_depth_below_surface_m) - sensor_depth_below_surface_m),
#
#       depth_diff = if_else(
#         row_number() == nrow(dat),
#         abs(sensor_depth_below_surface_m - lag(sensor_depth_below_surface_m)),
#         depth_diff
#       ),
#
#       trim_obs = case_when(
#         depth_diff > depth_threshold ~ 3,
#         TRUE ~ 1
#       ),
#
#       trim_obs = if_else(
#         ((row_number() == 1 |
#            row_number() == 2 |
#            row_number() == nrow(dat) - 1 |
#            row_number() == nrow(dat)) &
#           depth_trim_flag == 3), 4, depth_trim_flag),
#
#       depth_trim_flag = ordered(depth_trim_flag, levels = 1:4)
#     )
#
#   if(isFALSE(return_depth_diff)) dat <- dat %>% select(-depth_diff)
#
#   dat
}
