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
      sea_surface_wave_from_direction_degree = Dp,
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

#' Append "sea_surface_wave_" to height, period, and direction column names
#'
#' @param dat Data frame as returned from
#'   \code{wv_assign_short_variable_names()}.
#'
#' @importFrom dplyr if_else mutate
#' @importFrom stringr str_detect str_remove str_replace
#'
#' @export

wv_append_long_variable_names <- function(dat) {

  new_colnames <- colnames(dat) %>%
    data.frame(column_names = .) %>%
    mutate(
      # add sea_surface_wave_ to all cols with height, period, or direction
      column_names = if_else(
        str_detect(column_names, "height|period|from_direction"),
        paste0("sea_surface_wave_", column_names), column_names
      ),

      # remove sea_surface_wave_ from beginning of flag cols
      column_names = if_else(
        str_detect(column_names, "grossrange_flag|rolling_sd_flag|spike|qc"),
        str_remove(column_names, "sea_surface_wave_"), column_names
      ),

      # add sea_surface_wave_ to correct spot for qc columns
      column_names = case_when(
        str_detect(column_names, "grossrange_flag") &
          str_detect(column_names, "height|period|from_direction") ~
          str_replace(
            column_names,
            "grossrange_flag_",
            "grossrange_flag_sea_surface_wave_"
          ),

        str_detect(column_names, "rolling_sd_flag") &
          str_detect(column_names, "height|period|from_direction") ~
          str_replace(
            column_names,
            "rolling_sd_flag_",
            "rolling_sd_flag_sea_surface_wave_"
          ),

        str_detect(column_names, "spike_flag") &
          str_detect(column_names, "height|period|fom_direction") ~
          str_replace(
            column_names,
            "spike_flag_",
            "spike_flag_sea_surface_wave_"
          ),

        str_detect(column_names, "qc_flag") &
          str_detect(column_names, "height|period|from_direction")~
          str_replace(
            column_names,
            "qc_flag_",
            "qc_flag_sea_surface_wave_"
          ),

        TRUE ~ column_names
      ),

      # remove sea_surface_wave_ from of current direction col
      # column_names = if_else(
      #   str_detect(column_names, "sea_surface_wave_sea_water_to_direction_degree"),
      #   str_remove(column_names, "sea_surface_wave_"), column_names
      # ),
    )

  colnames(dat) <- new_colnames$column_names

  dat

}



