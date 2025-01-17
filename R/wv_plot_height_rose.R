#' Generate current rose
#'
#' @details Generates a wave height rose using the \code{windRose()} function
#'   from the \code{openair} package. See help files for
#'   \code{openair::windRose} for more detail.
#'
#' @param dat Data frame with one column name that includes the string
#'   "significant_height_m" and one column name that includes the string
#'   "direction". Other columns will be ignored.
#'
#' @param breaks Number of break points for current speed OR a vector of breaks.
#'   Lower-inclusive. Default is 0 to the maximum height by 0.5 m or 0.25 m
#'   increments.
#'
#' @param height_cols Vector of colours. Must be the same length as
#'   \code{breaks}. Default is the magma palette from \code{viridis}.
#'
#' @param height_label Title of the current speed legend. Default is "Wave
#'   Height (m)".
#'
#' @return Returns an "openair" object, a rose plot of current speed and
#'   direction.
#'
#' @importFrom dplyr contains
#' @importFrom openair windRose
#' @importFrom tidyr pivot_wider
#' @importFrom viridis viridis
#'
#' @export

wv_plot_height_rose <- function(
    dat, breaks = NULL,
    height_cols = NULL,
    height_label = "Wave Height (m)") {


  if("variable" %in% colnames(dat)) {
    dat <- dat %>%
      pivot_wider(names_from = "variable", values_from = "value")
  }

  dat <- dat %>%
    select(-contains("sea_water"), -contains("grossrange_flag")) %>%
    select(
      HEIGHT = contains("significant_height_m"),
      DIRECTION = contains("from_direction_degree")
    )

  if(ncol(dat) > 2) {
    stop("More than one height and/or direction column found in dat")
  }

  if(is.null(breaks)) {
    if(ceiling(max(dat$HEIGHT, na.rm = TRUE)) > 2) {
      breaks <- seq(0, ceiling(max(dat$HEIGHT, na.rm = TRUE)), 0.5)
    } else {
      breaks <- seq(0, ceiling(max(dat$HEIGHT, na.rm = TRUE)), 0.25)
    }
  }

  if (is.null(height_cols)) {
    if(length(breaks) == 1) {
      n_cols <- breaks
    } else n_cols <- length(breaks)

    height_cols <- viridis(n_cols, option = "F", direction = -1)
  }

  openair::windRose(
    dat,
    ws = "HEIGHT", wd = "DIRECTION",
    breaks = breaks,
    cols = height_cols,
    paddle = FALSE,
    auto.text = FALSE,
    annotate = FALSE,
    key.header = height_label,
    key.footer = "",
    key.position = "right"
  )
}
