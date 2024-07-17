#' Plot sensor_depth_below_surface_m coloured by roc flag.
#'
#' @param dat Data frame of wave data in long format, including
#'   \code{depth_flag} column, as exported from \code{adcp_flag_data()}.
#'
#' @param plotly_friendly Logical argument. If \code{TRUE}, the legend will be
#'   plotted when \code{plotly::ggplotly} is called on \code{p}. Default is
#'   \code{FALSE}, which makes the legend look better in a static figure.
#'
#' @return ggplot object. Figure shows sensor_depth_below_surface_m over time,
#'   coloured by the \code{depth_flag} column.
#'
#' @importFrom ggplot2 aes geom_point ggplot labs scale_colour_manual
#'   scale_x_datetime theme theme_light
#' @importFrom dplyr distinct mutate select
#'
#' @export

wv_plot_depth_flags <- function(
    dat,
    plotly_friendly = FALSE
) {

  flag_colours <- c("chartreuse4", "grey24", "#EDA247", "#DB4325")

  p <- dat %>%
    select(
      timestamp_utc,
      sensor_depth_below_surface_m,
      depth_trim_flag
    ) %>%
    qaqcmar::qc_assign_flag_labels() %>%
    ggplot(
      aes(timestamp_utc, sensor_depth_below_surface_m, colour = depth_trim_flag)) +
    geom_point(show.legend = TRUE) +
    scale_x_datetime("Date") +
    scale_colour_manual("Flag Value", values = flag_colours, drop = FALSE) +
    theme_light() +
    theme(
      strip.text = element_text(colour = "black", size = 10),
      strip.background = element_rect(fill = "white", colour = "darkgrey")
    )

  if(isFALSE(plotly_friendly)) {
    p <- p + guides(color = guide_legend(override.aes = list(size = 4)))
  }


  #if (isTRUE(flag_title)) p + ggtitle(paste0(qc_test, " test: ", var))

  p
}

