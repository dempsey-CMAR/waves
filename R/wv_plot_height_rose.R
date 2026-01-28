#' Generate wave height rose
#'
#' The direction of the petal indicates the direction the wave is travelling
#' from. The colour indicates the wave height. The length of the petal shows the
#' number of observations for each wave height and direction bin.
#'
#' @param dat Data frame with at least 2 columns: an ordered factor of direction
#'   groups, and a factor of height groups. The proportion of observations in
#'   each group is counted in the function and automatically converted to
#'   percent for the figure.
#'
#' @param direction_col The column in \code{dat} that holds the direction groups
#'   (NOT QUOTED).
#'
#' @param height_col The column in \code{dat} that holds the height groups (NOT
#'   QUOTED).
#'
#' @param pal Vector of colours. Must be the same length as the number of speed
#'   factor levels.
#'
#' @param height_label Title of the current speed legend. Default is "Wave
#'   Height (m)".
#'
#' @param ncol_legend Number of columns for the figure legend. Default is 2.
#'
#' @return Returns a ggplot object, a rose plot of current speed and direction.
#'
#' @importFrom dplyr reframe
#' @importFrom ggplot2 aes coord_radial element_blank element_line element_rect
#'   geom_col ggplot position_stack scale_fill_manual scale_x_discrete theme
#' @importFrom scales percent
#' @importFrom viridis viridis
#'
#'
#' @export


wv_plot_height_rose <- function(
    dat,
    pal = NULL,
    height_col = significant_height_m_labels,
    direction_col = from_direction_degree_labels,
    height_label = "Wave Height (m)",
    ncol_legend = 2
) {

  if (is.null(pal)) {
    #n_levels <-  nrow(reframe(dat, levels({{ height_col }})))

    n_levels <- nrow(reframe(dat, levels(significant_height_m_labels)))

    get_wv_rose_cols <- colorRampPalette(c("#c7dfe7", "#063E4D"))

    pal <- get_wv_rose_cols(n_levels)
  }

  n_dir_levels <- length(levels(dat$from_direction_degree_labels))

  if(n_dir_levels == 8) theta <- -22.5
  if(n_dir_levels == 16) theta <- -11.25

  dat %>%
    group_by({{ height_col }}, {{ direction_col}}, drop = FALSE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(n_prop = n / sum(n)) %>%
    ggplot(
      aes({{ direction_col }}, n_prop, fill = {{ height_col }})
    ) +
    geom_col(show.legend = TRUE, position = position_stack(reverse = TRUE)) +
    scale_fill_manual(height_label, values = pal, drop = FALSE) +
    scale_x_discrete(
      expand = expansion(add = c(0.5, 0.5)),
      drop = FALSE
    ) +
    scale_y_continuous(labels = scales::percent) +
    coord_radial(start = theta * pi / 180, r.axis.inside = TRUE) +
    guides(fill = guide_legend(ncol =  ncol_legend)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),

      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),

      axis.text.x = element_text(color = 1),

      panel.border =  element_rect(colour = "gray50", fill = NA, linewidth = 0.25),
      panel.background = element_rect(fill = NA, color = NA),

      panel.grid = element_line(color = "gray70", linewidth = 0.25),
      panel.grid.minor.y = element_blank()
    )
}






# Generate wave height rose
#
# @details Generates a wave height rose using the \code{windRose()} function
#   from the \code{openair} package. See help files for
#  \code{openair::windRose} for more detail.
#
#@param dat Data frame with one column name that includes the string
#   "significant_height_m" and one column name that includes the string
#   "direction". Other columns will be ignored.
#
# @param breaks Number of break points for current speed OR a vector of breaks.
#   Lower-inclusive. Default is 0 to the maximum height by 0.5 m or 0.25 m
#   increments.
#
# @param height_cols Vector of colours. Must be the same length as
#   \code{breaks}. Default is the magma palette from \code{viridis}.
#
# @param height_label Title of the current speed legend. Default is "Wave
#   Height (m)".
#
# @return Returns an "openair" object, a rose plot of current speed and
#   direction.
#
# @importFrom dplyr contains
# @importFrom openair windRose
# @importFrom tidyr pivot_wider
# @importFrom viridis viridis
#
# @export

# wv_plot_height_rose <- function(
#     dat, breaks = NULL,
#     height_cols = NULL,
#     height_label = "Wave Height (m)") {
#
#
#   if("variable" %in% colnames(dat)) {
#     dat <- dat %>%
#       pivot_wider(names_from = "variable", values_from = "value")
#   }
#
#   dat <- dat %>%
#     select(-contains("sea_water"), -contains("grossrange_flag")) %>%
#     select(
#       HEIGHT = contains("significant_height_m"),
#       DIRECTION = contains("from_direction_degree")
#     )
#
#   if(ncol(dat) > 2) {
#     stop("More than one height and/or direction column found in dat")
#   }
#
#   if(is.null(breaks)) {
#     if(ceiling(max(dat$HEIGHT, na.rm = TRUE)) > 2) {
#       breaks <- seq(0, ceiling(max(dat$HEIGHT, na.rm = TRUE)), 0.5)
#     } else {
#       breaks <- seq(0, ceiling(max(dat$HEIGHT, na.rm = TRUE)), 0.25)
#     }
#   }
#
#   if (is.null(height_cols)) {
#     if(length(breaks) == 1) {
#       n_cols <- breaks
#     } else n_cols <- length(breaks)
#
#     height_cols <- viridis(n_cols, option = "F", direction = -1)
#   }
#
#   openair::windRose(
#     dat,
#     ws = "HEIGHT", wd = "DIRECTION",
#     breaks = breaks,
#     cols = height_cols,
#     paddle = FALSE,
#     auto.text = FALSE,
#     annotate = FALSE,
#     key.header = height_label,
#     key.footer = "",
#     key.position = "right"
#   )
# }
