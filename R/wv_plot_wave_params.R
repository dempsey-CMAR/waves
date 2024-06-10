
#' Plot wave height over time
#'
#' @param dat Data frame with at
#' @param vars
#' @param pal
#'
#' @return
#' @export
#'

wv_plot_ts <- function(dat, vars = c("Hs", "Hmax"), pal = NULL) {

  if(is.null(pal)) {
    n <- length(vars)

    if(n < 3) n <- 3
    pal <- brewer.pal(n, "Dark2")
  }

  if (all(grepl("H", x = vars))) y_axis <- "Wave Height (m)"
  if (all(grepl("T", x = vars))) y_axis <- "Wave Period (s)"

  dat %>%
    pivot_longer(cols = all_of(vars), values_to = "value", names_to = "variable") %>%
    mutate(variable = case_when(
      variable == "Hs" ~ "Significant\nWave Height",
      variable == "Hmax" ~ "Maximum\nWave Height",
      variable == "H1/3" ~ "Average of Largest 1/3 Waves",
      variable == "H1/10" ~ "Average of Largest 1/10 Waves",
      variable == "Tp" ~ "Peak Wave Period",
      variable == "Tmax" ~ "Period of Largest Waves",
      variable == "T1/3" ~ "Period of Average\nof Largest 1/3 Waves",
      variable == "T1/10" ~ "Period of Average\nof Largest 1/10 Waves"

    )) %>%
    ggplot(aes(timestamp_utc, value, col = variable)) +
    geom_line(size = 1) +
    scale_x_datetime("Date") +
    scale_y_continuous(y_axis) +
    scale_colour_manual("", values = pal) +
    guides(colour = guide_legend(keyheight = 2))

}
