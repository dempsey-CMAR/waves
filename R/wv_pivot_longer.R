#' Pivot wave data
#'
#' @param dat_wide Data frame of ADCP wave data, as returned from
#'   \code{wv_read_txt()}.

#' @param rm_na Logical argument. If \code{rm_NA = TRUE}, rows where
#'   \code{sea_water_speed_m_s} is \code{NA} OR
#'   \code{sea_water_to_direction_degree} is \code{NA} will be removed.
#'
#' @return Returns data in a long format.
#'
#'   If \code{dat_wide} is from \code{adcp_read_txt()} (i.e., bin altitude has
#'   not been assigned), then the following columns are returned: timestamp_utc,
#'   sensor_depth_below_surface_m (SensorDepth), bin_id (the default column
#'   names when imported using read_adcp_txt; starts at V8), sea_water_speed_m_s
#'   (WaterSpeed), and sea_water_to_direction_degree (WaterDirection).
#'
#'   If \code{dat_wide} is from \code{adcp_assign_altitude()}, then the
#'   following columns are returned: timestamp_utc, sensor_depth_below_surface_m
#'   (SensorDepth), bin_depth_below_surface_m, bin_height_above_sea_floor_m,
#'   sea_water_speed_m_s (WaterSpeed), and sea_water_to_direction_degree
#'   (WaterDirection).
#'
#' @importFrom dplyr last_col
#' @importFrom tidyr pivot_longer
#'
#' @export


wv_pivot_longer <- function(dat, vars = NULL) {

  if(is.null(vars)) {
    dat %>%
      pivot_longer(
        cols = 2:last_col(),
        names_to = "variable", values_to = "value"
      )
  } else {
    dat %>%
      pivot_longer(
        cols = vars,
        names_to = "variable", values_to = "value"
      )

  }

}

