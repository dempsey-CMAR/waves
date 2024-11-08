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

#' Set histogram bin width for different variables
#'
#' @param var_to_bin Character string of the variable to bin.
#'
#' @return Returns a number that will generate useful histogram for
#'   \code{var_to_bin}.
#' @export
#'
wv_get_bin_width <- function(var_to_bin) {

  if(str_detect(var_to_bin, "height")) bin_width <- 0.1
  if(str_detect(var_to_bin, "period")) bin_width <- 2
  if(str_detect(var_to_bin, "direction")) bin_width <- 20
  if(str_detect(var_to_bin, "depth")) bin_width <- 1
  if(str_detect(var_to_bin, "speed")) bin_width <- 0.05

  bin_width
}

#' Convert wave variables to ordered factor to control order of ggplot facets
#'
#' @param dat Data frame of wave data in long format. Variable names
#'   must be in short format. Flag columns will be dropped.
#'
#' @return Returns \code{dat} with \code{variable} column as an ordered factor.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#'
#' @export

wv_convert_vars_to_ordered_factor <- function(dat) {

  # can't automate this unless wv_pivot_longer() get a default for first_pivot_col
  # if(!("variable" %in% colnames(dat))) {
  #   dat <- dat %>%
  #     wv_assign_short_variable_names() %>%
  #     wv_pivot_vars_longer() %>%
  #     filter(!str_detect(variable, "flag"))
  # }

  dat %>%
    mutate(
      variable = ordered(
        variable,
        levels = c(
          "significant_height_m",
          "peak_period_s",

          "average_height_largest_33_percent_m",
          "period_largest_33_percent_s",

          "average_height_largest_10_percent_m",
          "period_largest_10_percent_s",

          "maximum_height_m",
          "period_maximum_s",

          "to_direction_degree",
          "sensor_depth_below_surface_m",

          "sea_water_speed_m_s",
          "sea_water_to_direction_degree",

          TRUE ~ NA_character_
        )
      )
    )
}



#' Add a column of axis labels for wave variables
#'
#' @param dat Data frame of wave data in long format. Variable names
#'   should be in short format. Flag columns will be dropped.
#'
#' @param convert_to_ordered_factor Logical variable indicating whether the new
#'   \code{variable_label} column should be converted to an ordered factor.
#'   Default is \code{TRUE}.
#'
#' @return Returns \code{dat} with an additional \code{variable_label} column
#'   for use in faceted figures.
#'
#' @importFrom dplyr case_when mutate
#' @importFrom stringr str_detect
#'
#' @export
#'

wv_create_variable_labels <- function(dat, convert_to_ordered_factor = TRUE) {

  if (!("variable" %in% colnames(dat))) {
    dat <- wv_pivot_vars_longer(dat)
  }

  dat <- dat %>%
    mutate(
      variable_label = case_when(
        variable == "significant_height_m" ~ "Wave Height (m)",
        variable == "average_height_largest_33_percent_m" ~ "H1/3 (m)",
        variable == "average_height_largest_10_percent_m" ~ "H1/10 (m)",
        variable == "maximum_height_m" ~ "Hmax (m)",

        variable == "peak_period_s" ~ "Peak Period (s)",
        variable == "period_largest_33_percent_s" ~ "T1/3 (s)",
        variable == "period_largest_10_percent_s" ~ "T1/10 (s)",
        variable == "period_maximum_s" ~ "Tmax (s)",

        variable == "to_direction_degree" ~ "Wave Direction (degree)",
        variable == "sensor_depth_below_surface_m" ~ "Sensor Depth Below Surface (m)",
        variable == "sea_water_speed_m_s" ~ "Sea Water Speed (m/s)",
        variable == "sea_water_to_direction_degree" ~ "Sea Water Direction (degree)",

        TRUE ~ NA_character_
      )
    )

  if(isTRUE(convert_to_ordered_factor)) {
    dat <- dat %>%
      mutate(
        variable_label = ordered(
          variable_label,
          levels = c(
            "Wave Height (m)", "Peak Period (s)", "Wave Direction (degree)",
            "H1/3 (m)", "T1/3 (s)",
            "H1/10 (m)", "T1/10 (s)",
            "Hmax (m)", "Tmax (s)",
            "Sensor Depth Below Surface (m)",
            "Sea Water Speed (m/s)",
            "Sea Water Direction (degree)"
          )
        )
      )
  }

  dat
}


#' Add a column of wave variables in title case
#'
#' @param dat Data frame of wave data in long format. Variable names
#'   should be in short format. Flag columns will be dropped.
#'
#' @param convert_to_ordered_factor Logical variable indicating whether the new
#'   \code{variable_title} column should be converted to an ordered factor.
#'   Default is \code{TRUE}.
#'
#' @return Returns \code{dat} with an additional \code{variable_title} column
#'   for use in faceted figures.
#'
#' @importFrom dplyr case_when mutate
#' @importFrom stringr str_detect
#'
#' @export
#'

wv_convert_vars_to_title <- function(dat, convert_to_ordered_factor = TRUE) {

  dat <- dat %>%
    mutate(
      variable_title = case_when(
        variable == "significant_height_m" ~ "Significant Wave Height",
        variable == "average_height_largest_33_percent_m" ~
          "Average Height of Largest 1/3 Waves",
        variable == "average_height_largest_10_percent_m" ~
          "Average Height of Largest 1/10 Waves",
        variable == "maximum_height_m" ~ "Largest Wave Height",

        variable == "peak_period_s" ~ "Peak Period",
        variable == "period_largest_33_percent_s" ~
          "Period of Largest 1/3 Waves",
        variable == "period_largest_10_percent_s" ~
          "Period of Largest 1/10 Waves",
        variable == "period_maximum_s" ~ "Period of Largest Waves",

        variable == "to_direction_degree" ~ "Direction Wave is Travelling To",
        variable == "sensor_depth_below_surface_m" ~ "Sensor Depth Below Surface",
        variable == "sea_water_speed_m_s" ~ "Sea Water Speed",
        variable == "sea_water_to_direction_degree" ~
          "Direction Sea Water is Travelling To",

        TRUE ~ NA_character_
      )
    )

  if(isTRUE(convert_to_ordered_factor)) {
    dat <- dat %>%
      mutate(
        variable_title = ordered(
          variable_title,
          levels = c(
            "Significant Wave Height",
            "Peak Period",

            "Average Height of Largest 1/3 Waves",
            "Period of Largest 1/3 Waves",

            "Average Height of Largest 1/10 Waves",
            "Period of Largest 1/10 Waves",

            "Largest Wave Height",
            "Period of Largest Waves",

            "Direction Wave is Travelling To",
            "Sensor Depth Below Surface",

            "Sea Water Speed",
            "Direction Sea Water is Travelling To"
          )
        )
      )
  }

  dat
}



