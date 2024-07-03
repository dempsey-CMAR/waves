#' Plot wave parameters over time
#'
#' @param dat Data frame with wave parameter data in wide or long format.
#'
#' @param pal Optional colour palette. Default is the "Dark2" palette from
#'   \code{Rcolourbrewer}.
#'
#' @param y_axis_label Label for the y-axis. Default is "Wave Height (m)" if
#'   only plotting wave variables and "Wave Period (s)" if only plotting period
#'   variables.
#'
#' @param show_legend Logical argument indicating whether to show figure legend.
#'   Default is \code{FALSE}.
#'
#' @param n_col Number of columns for faceted figure. Default is 1.
#'
#' @param scales Character string indicating how to treat scales of the faceted
#'   plot. Passed to \code{facet_wrap()}. Default is "fixed". Other options are
#'   "free_x" and "free_y".
#'
#' @return Returns a ggplot object of the plotted variables.
#'
#' @importFrom dplyr %>% all_of case_when distinct mutate
#' @importFrom ggplot2 aes element_text element_rect facet_wrap geom_line ggplot
#'   guides guide_legend scale_colour_manual scale_x_datetime scale_y_continuous
#'   theme theme_light waiver
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer

#' @export
#'

wv_plot_ts <- function(
    dat,
    pal = NULL,
    y_axis_label = NULL,
    n_col = 1,
    scales = "fixed",
    show_legend = FALSE
) {

  # if(!("variable" %in% colnames(dat))) {
  #   dat <- dat %>%
  #     wv_assign_short_variable_names() %>%
  #     wv_pivot_vars_longer() %>%
  #     filter(!str_detect(variable, "flag"))
  # }

  # this will pivot longer if required
  dat <- dat %>%
    wv_convert_vars_to_title()

  vars <- distinct(dat, variable)$variable

  if(is.null(pal)) {
    n <- length(vars)
    if(n < 3) n <- 3

    if(n <= 8)  pal <- brewer.pal(n, "Dark2")

    # maybe if n > 8, make everything grey?
    if(n > 8) {
      # set up colour palette - need to interpolate with colorRampPalette
      get_pal = colorRampPalette(brewer.pal(8, "Dark2"))
      pal <- get_pal(n)
    }
  }

  if(is.null(y_axis_label)) {
    if (all(str_detect(vars, "height"))) {
      y_axis_label <- "Wave Height (m)"
    } else if (all(str_detect(vars, "period"))) {
      y_axis_label <- "Wave Period (s)"
    } else y_axis_label <- waiver()
  }

  p <- ggplot(dat, aes(timestamp_utc, value, col = variable)) +
    geom_line(linewidth = 1) +
    scale_x_datetime("Date") +
    scale_y_continuous(y_axis_label) +
    scale_colour_manual("", values = pal) +
    theme_light() +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(colour = "grey80", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10)
    ) +
    facet_wrap(~variable_title, ncol = n_col, scales = scales)

  if(isFALSE(show_legend)) p <- p + theme(legend.position = "none")

  p
}


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
    dat, vars, binwidth = NULL, bar_outline = "grey10", pal = NULL,
    show_legend = TRUE) {

  # if(is.null(pal)) {
  #   n <- length(vars)
  #
  #   if(n < 3) n <- 3
  #   pal <- brewer.pal(n, "Dark2")
  # }
  #
  # if (all(grepl("H", x = vars))) x_axis <- "Wave Height (m)"
  # if (all(grepl("T", x = vars))) x_axis <- "Wave Period (s)"
  #
  # if(!("variable" %in% colnames(dat))) {
  #   dat <- dat %>%
  #     pivot_longer(
  #       cols = all_of(vars), values_to = "value", names_to = "variable")
  # }
  #
  # p <- dat %>%
  #   filter(variable %in% vars) %>%
  #   # mutate(variable = case_when(
  #   #   variable == "Hs" ~ "Significant\nWave Height",
  #   #   variable == "Hmax" ~ "Maximum\nWave Height",
  #   #   variable == "H1/3" ~ "Average of Largest 1/3 Waves",
  #   #   variable == "H1/10" ~ "Average of Largest 1/10 Waves",
  #   #   variable == "Tp" ~ "Peak Wave Period",
  #   #   variable == "Tmax" ~ "Period of Largest Waves",
  #   #   variable == "T1/3" ~ "Period of Average\nof Largest 1/3 Waves",
  #   #   variable == "T1/10" ~ "Period of Average\nof Largest 1/10 Waves"
  #   # )) %>%
  #   mutate(
  #     variable = ordered(
  #       variable,
  #       levels = c("Hs", "H1/3", "H1/10", "Hmax",
  #                  "Tp", "T1/3", "T1/10", "Tmax", "Depth"))
  #   ) %>%
  #   ggplot(aes(value, fill = variable)) +
  #   geom_histogram(
  #     aes(
  #       y = 100 * after_stat(count / sum(count)),
  #       text = after_stat(
  #         paste(
  #           "percent:", round(100 * after_stat(count / sum(count)), digits = 2),
  #           "\nbin:", x
  #         ))
  #     ),
  #     binwidth = binwidth, col = bar_outline
  #   ) +
  #   scale_y_continuous(
  #     "Percent of Observations", expand = expansion(mult = c(0, 0.1))
  #   ) +
  #   scale_x_continuous(x_axis) +
  #   scale_fill_manual("", values = pal) +
  #   guides(fill = guide_legend(keyheight = 2)) +
  #   theme(
  #     panel.grid = element_blank(),
  #     panel.border = element_rect(colour = "grey10", fill = NA),
  #     legend.text = element_text(size = 10)
  #   ) +
  #   theme_light()
  #
  # if(isFALSE(show_legend)) p <- p + theme(legend.position = "none")
  #
  # p

}

