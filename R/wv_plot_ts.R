#' Plot wave parameters over time
#'
#' @param dat Data frame with wave parameter data in wide or long format.
#'
#' @param vars Character vector of variables to pivot if \code{dat} is in a wide
#'   format. Default is all variables.
#'
#' @param pal Optional colour palette. Default is c("#104862", "#62AECA",
#'   "#FFD118") (Indigo dye, Moonstone, Jonquil). For plots with more than 3
#'   variables, these colours are interpolated using
#'   \code{grDevices::colorRampPalette}.
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

wv_plot_ts <- function(
    dat,
    vars = NULL,
    pal = NULL,
    n_col = 1,
    scales = "fixed",
    show_legend = FALSE
) {

  if(!("variable" %in% colnames(dat))) {
    dat <- wv_pivot_vars_longer(dat, vars = vars)
  }


  dat <- dat %>%
    wv_create_variable_labels()

  vars <- distinct(dat, variable)$variable

  if(is.null(pal)) {

    get_pal <- colorRampPalette(c("#063E4D", "#62AECA", "#FFD118"))

    n <- length(vars)

    pal <- get_pal(n)
  }


  p <- ggplot(dat, aes(timestamp_utc, value, col = variable_label)) +
    geom_line(linewidth = 1) +
    scale_x_datetime("Date", date_labels = "%Y-%m-%d") +
    scale_colour_manual("", values = pal) +
    theme_light() +
    theme(
      axis.title.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(colour = NA, fill = NA),
      strip.text = element_text(colour = "black", size = 10)
    ) +
    facet_wrap(
      ~variable_label,
      ncol = n_col,
      scales = scales,
      strip.position = "left"
    )

  if(isFALSE(show_legend)) {
    p <- p +
      theme(legend.position = "none")
  }

  p
}

