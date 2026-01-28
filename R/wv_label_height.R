#' Assign intervals to wave height data
#'
#' Assigns intervals to \code{column} using the \code{cut()} function.
#' \code{column} will typically be a \code{wave_height} column. To label
#' direction data, use \code{wv_label_direction()}.
#'
#' Intervals start and 0 and end at the maximum value in increments of 0.25 or
#' 0.5
#'
#' @param dat Data frame including the column with observations that will be
#'   assigned to intervals.
#'
#' @param column Column in \code{dat} that will be assigned to intervals (NOT
#'   QUOTED).
#'
#' @param breaks Vector of breaks.
#'
#' @param label_sep Separator for the interval labels ("lower to upper").
#'   Default is " ".
#'
#' @return Returns \code{dat} with an additional column \code{column_labels},
#'   the interval labels as an ordered factor.
#'
#' @importFrom dplyr arrange mutate pull select tibble
#' @importFrom DescTools RoundTo
#' @importFrom rlang :=
#'
#' @export


wv_label_height <- function(
    dat,
    column = significant_height_m,
    breaks = NULL,
    label_sep = " "
    ) {

  dat <- dat %>%
    mutate(col_to_cut = {{ column }})

  # dat <- dat %>%
  #   mutate(col_to_cut = significant_height_m)

  if( is.null(breaks)) {
    max_height <- max(dat$col_to_cut, na.rm = TRUE)

    if(max_height <= 0.5) {
      #max_height <- round(max_height, digits = 1)
      max_height <- 0.5
    } else max_height <- ceiling(max_height)

    if(max_height <= 1) {
      inc_wv <- 0.10
    } else if(max_height <= 4) {
      inc_wv <- 0.25
    } else inc_wv <- 0.5

    breaks <- seq(0, max_height, by = inc_wv)
  }

  # if(is.null(breaks)) {
  #   if(ceiling(max(dat$col_to_cut, na.rm = TRUE)) > 2) {
  #     breaks <- seq(0, ceiling(max(dat$col_to_cut, na.rm = TRUE)), 0.5)
  #   } else {
  #     breaks <- seq(0, ceiling(max(dat$col_to_cut, na.rm = TRUE)), 0.25)
  #   }
  # }

  # if (is.null(height_cols)) {
  #   if(length(breaks) == 1) {
  #     n_cols <- breaks
  #   } else n_cols <- length(breaks)
  #
  #   height_cols <- viridis(n_cols, option = "F", direction = -1)
  # }

  dat$ints <- cut(
    dat$col_to_cut,
    breaks =  breaks,
    include.lowest = TRUE # smallest interval will be left AND right inclusive
  )

  dat <- dat %>%
    separate(
      col = "ints", into = c("lower", "upper"), sep = ",", remove = FALSE
    ) %>%
    mutate(
      lower = as.numeric(str_remove(lower, pattern = "\\(|\\[|\\)|\\]")),
      upper = as.numeric(str_remove(upper, pattern = "\\(|\\[|\\)|\\]")),
      ints_label = paste(lower, "to", upper, sep = label_sep),
    )

  # int labels in order from lowest to highest heights
  ints_levels <- data.frame(int_levels = levels(dat$ints)) %>%
    separate(
      col = "int_levels", into = c("lower", "upper"), sep = ",", remove = FALSE
    ) %>%
    mutate(
      lower = as.numeric(str_remove(lower, pattern = "\\(|\\[|\\)|\\]")),
      upper = as.numeric(str_remove(upper, pattern = "\\(|\\[|\\)|\\]")),
      ints_levels = paste(lower, "to", upper, sep = label_sep),
    ) %>%
    pull(ints_levels)

  # convert int_labels to factor so they are in the correct order for the barplot
  dat  %>%
    mutate("{{column}}_labels" := factor(ints_label, levels = ints_levels)) %>%
    select(-c(col_to_cut, ints, lower, upper, ints_label))

}
