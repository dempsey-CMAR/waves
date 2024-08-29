#' Plot sensor_depth_below_surface_m coloured by roc flag.
#'
#' @param dat Data frame of flagged wave data with variables in long format and
#'   grossrange flags in long or wide format.
#'
#' @param n_col Number of columns for the faceted figure.
#'
#' @return ggplot object. Figure shows variables over time, coloured by the
#'   \code{grossrange_flag_value} column.
#'
#' @importFrom ggplot2 aes facet_wrap geom_point ggplot labs scale_colour_manual
#'   scale_x_datetime theme theme_light
#' @importFrom dplyr distinct mutate select
#'
#' @export

wv_plot_grossrange_flags <- function(dat, n_col) {

  flag_colours <- c("chartreuse4",  "#DB4325")

  if(!("grossrange_flag_value" %in% colnames(dat))) {
    dat <- dat %>% wv_pivot_flags_longer()
  }

  p <- dat %>%
    qaqcmar::qc_assign_flag_labels() %>%
    wv_convert_vars_to_title() %>%
    ggplot(aes(timestamp_utc, value, col = grossrange_flag_value)) +
    geom_point(show.legend = TRUE) +
    scale_x_datetime("Date", date_labels = "%Y-%m-%d") +
    scale_colour_manual("Flag Value", values = flag_colours, drop = FALSE) +
    theme_light() +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(colour = "grey80", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10)
    ) +
    facet_wrap(~variable_title, ncol = n_col, scales = "free_y")

  p
}

