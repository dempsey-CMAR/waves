#' Plot wave data coloured by flag value
#'
#' @param dat Data frame of flagged wave data in long or wide format. Must
#'   include at least one column name with the string "_flag_variable".
#'
#' @param vars Character vector of variables to plot. Default is \code{vars =
#'  "all"}, which will make a plot for each recognized variable in \code{dat}.
#'
#' @param qc_tests Character string of QC tests to plot. Default is
#'   \code{qc_tests = c("grossrange", "rolling_sd")}.
#'
#' @param labels Logical argument indicating whether to convert numeric flag
#'   values to text labels for the legend.
#'
#' @param n_col Number of columns for faceted plots.
#'
#' @param flag_title Logical argument indicating whether to include a ggtitle of
#'   the qc test and variable plotted.
#'
#' @param plotly_friendly Logical argument. If \code{TRUE}, the legend will be
#'   plotted when \code{plotly::ggplotly} is called on \code{p}. Default is
#'   \code{FALSE}, which makes the legend look better in a static figure.
#'
#' @return Returns a list of ggplot objects; one figure for each test in
#'   \code{qc_tests} and variable in \code{vars}. Points are coloured by the
#'   flag value and panels are faceted by depth and sensor. faceted by depth and
#'   sensor.
#'
#' @importFrom lubridate as_datetime
#'
#' @export
#'

wv_plot_flags <- function(
    dat,
    qc_tests = c("grossrange", "rolling_sd", "spike"),
    vars = "all",
    labels = TRUE,
    n_col = NULL,
    flag_title = TRUE,
    plotly_friendly = FALSE
) {

  p_out <- list()

  if (!("variable" %in% colnames(dat))) {
    dat <- wv_pivot_vars_longer(dat) %>%
      wv_pivot_flags_longer(qc_tests = qc_tests)
  }

  if (vars == "all") vars <- unique(dat$variable)

  if (isTRUE(labels)) dat <- dat %>% qaqcmar::qc_assign_flag_labels()

  if (is.null(n_col)) n_col <- 2

  # plot for each test
  for (j in seq_along(qc_tests)) {
    qc_test_j <- qc_tests[j]

    p_out[[qc_test_j]] <- wv_ggplot_flags(
      dat,
      qc_test = qc_test_j,
      n_col = n_col,
      plotly_friendly = plotly_friendly
    )
  }

  p_out
}


#' Create ggplot for one wave qc_test and variable
#'
#' @param dat Data frame of flagged wave data in long format. Must
#'   include a column named with the string "_flag_value".
#'
#' @param qc_test qc test to plot.
#'
#param var variable to plot.
#'
#' @inheritParams wv_plot_flags
#'
#' @return Returns a ggplot object; a figure for \code{qc_test}.
#'   Points are coloured by the flag value and panels are faceted by depth and
#'   sensor.
#'
#' @importFrom ggplot2 aes element_rect element_text facet_wrap geom_point
#'   ggplot ggtitle guides guide_legend  scale_colour_manual scale_x_datetime
#'   scale_y_continuous theme_light theme
#'
#' @importFrom gtools mixedsort


wv_ggplot_flags <- function(
    dat,
    qc_test,
    #var,
    n_col = NULL,
    flag_title = TRUE,
    plotly_friendly = FALSE
) {
  # https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/
  flag_colours <- c("chartreuse4", "grey24", "#EDA247", "#DB4325")

  flag_column <- paste0(qc_test, "_flag_value")

  p <- dat %>%
    wv_convert_vars_to_title() %>%
    ggplot(aes(timestamp_utc, value, col = !!sym(flag_column))) +
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

  if(isFALSE(plotly_friendly)) {
    p <- p + guides(color = guide_legend(override.aes = list(size = 4)))
  }

  if (isTRUE(flag_title)) p + ggtitle(paste0(qc_test, " test"))

  p
}

