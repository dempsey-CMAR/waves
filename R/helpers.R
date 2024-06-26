#' Extract deployment date and station name from file path
#'
#' @param file_path Path to the file, include file name and extension (.csv or
#'   .txt). File name must include the deployment date and the station name,
#'   separated by " - ", e.g., "2008.09.25 - Coffin Island SW - Waves.txt"
#'
#' @return Returns a tibble with two columns: \code{depl_date}, and
#'   \code{station}.
#'
#' @importFrom dplyr %>%  mutate
#' @importFrom lubridate as_date
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#'
#' @export

wv_extract_deployment_info <- function(file_path) {
  sub(".*/", "", file_path, perl = TRUE) %>%
    data.frame() %>%
    separate(col = ".", into = c("depl_date", "station", NA), sep = " - ") %>%
    mutate(
      depl_date = str_replace_all(depl_date, "\\.", "-"),
      depl_date= as_date(depl_date)
    )
}


#' Assign climate forecast-like variable names to columns
#'
#' Guidelines for Construction of CF Standard Names:
#' https://cfconventions.org/Data/cf-standard-names/docs/guidelines.html
#'
#' @param dat Data frame as returned from \code{wv_read_txt()}.
#'
#' @importFrom dplyr rename
#'
#' @export

wv_assign_cf_variable_names <- function(dat) {

  dat %>%
    rename(
      sea_surface_wave_significant_height_m = Hs,
      sea_surface_wave_average_height_largest_10_percent_m = `H1/10`,
      sea_surface_wave_average_height_largest_33_percent_m = `H1/3`,
      sea_surface_wave_maximum_height_m = Hmax,
      sea_surface_wave_peak_period_s = Tp,
      sea_surface_wave_period_largest_10_percent_s = `T1/10`,
      sea_surface_wave_period_largest_33_percent_s = `T1/3`,
      sea_surface_wave_period_maximum_s = Tmax,
      sea_surface_wave_to_direction_degree = Dp,
      sensor_depth_below_surface_m = Depth,
      sea_water_speed_m_s = CM,
      sea_water_to_direction_degree = CD
    )
}


#' Assign short variable names to columns
#'
#' Removes "sea_surface_wave_" from wave variable column names.
#'
#' @param dat Data frame as returned from \code{wv_assign_cf_variable_names()}.
#'
#' @importFrom stringr str_remove
#'
#' @export

wv_assign_short_variable_names <- function(dat) {

  colnames(dat) <- str_remove(colnames(dat), "sea_surface_wave_")

  dat
}


