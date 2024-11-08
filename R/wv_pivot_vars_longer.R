#' Pivot wave data
#'
#' @param dat_wide Data frame of ADCP wave data, as returned from
#'   \code{wv_read_txt()}.
#'
#' @param vars Vector of character strings indicating which columns to pivot.
#'   Default is all variables (with column names in the "short" format).
#'
#' @return Returns data in a long format.
#'
#' @importFrom dplyr any_of
#' @importFrom tidyr pivot_longer
#'
#' @export

wv_pivot_vars_longer <- function(dat_wide, vars = NULL) {

  if(is.null(vars)) {
    vars <- c(
      "significant_height_m",
      "average_height_largest_10_percent_m",
      "average_height_largest_33_percent_m",
      "maximum_height_m",
      "peak_period_s",
      "period_largest_10_percent_s",
      "period_largest_33_percent_s",
      "period_maximum_s",
      "to_direction_degree",
      "sensor_depth_below_surface_m",
      "sea_water_speed_m_s",
      "sea_water_to_direction_degree"

      # "sea_surface_wave_significant_height_m",
      # "sea_surface_wave_average_height_largest_10_percent_m",
      # "sea_surface_wave_average_height_largest_33_percent_m",
      # "sea_surface_wave_maximum_height_m",
      # "sea_surface_wave_peak_period_s",
      # "sea_surface_wave_period_largest_10_percent_s",
      # "sea_surface_wave_period_largest_33_percent_s",
      # "sea_surface_wave_period_maximum_s",
      # "sea_surface_wave_to_direction_degree"
    )
  }

  dat_wide %>%
    pivot_longer(
      cols = any_of(vars),
      names_to = "variable",
      values_to = "value",
      values_drop_na = TRUE
    )
}
