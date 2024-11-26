#' Plot histogram of wave parameters
#'
#' @inheritParams wv_plot_ts
#'
#' @param binwidth Width of the bins. Passed to \code{geom_histogram()}.
#'
#' @param bar_outline Single colour for the outline of the histogram bars.
#'   Default is "grey10". May leave artifacts (lines at the top of some groups)
#'   in the \code{ggplotly()} version of the figure. Use \code{bar_outline = NA}
#'   to avoid.
#'
#' @param x_axis_label Label for the x-axis. Default is "Wave Height (m)" if
#'   only plotting wave variables and "Wave Period (s)" if only plotting period
#'   variables.
#'
#' @return Returns a ggplot object.
#'
#' @importFrom dplyr %>% all_of case_when count mutate
#' @importFrom ggplot2 aes after_stat element_blank element_rect expansion
#'   element_text geom_histogram ggplot guides guide_legend scale_fill_manual
#'   scale_x_continuous scale_y_continuous theme theme_light
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tidyr pivot_longer
#'
#' @export

wv_plot_histogram <- function(
    dat,
    pal = NULL,
    x_axis_label = NULL,
    n_col = 1,
    scales = "fixed",
    binwidth = NULL,
    bar_outline = "grey10",

    show_legend = FALSE) {

  # this will pivot longer if required
  dat <- dat %>%
    wv_create_variable_labels()

  vars <- distinct(dat, variable)$variable

  if(is.null(pal)) {

    get_pal <- colorRampPalette(c("#063E4D", "#62AECA", "#FFD118"))

    n <- length(vars)

    pal <- get_pal(n)
  }

  p <- ggplot(dat, aes(value, fill = variable_label)) +
    geom_histogram(binwidth = binwidth, col = bar_outline) +
    scale_y_continuous(
      "Number of Observations", expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(x_axis_label) +
    scale_fill_manual("", values = pal) +
    theme_light() +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(colour = "grey80", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10)
    ) +
    facet_wrap(~variable_label, ncol = n_col, scales = scales)

  if(isFALSE(show_legend)) {
    p <- p +
      theme(legend.position = "none") +
      guides(fill = guide_legend(keyheight = 2))
  }

  p

}

