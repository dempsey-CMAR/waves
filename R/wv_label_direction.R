#' Cut wind direction into 8 or 16 bins
#'
#' Adapted from \code{cutVecWinddir()} from \code{OpenAir}.
#'
#' Likely more efficient in the original vectorized form. Modified to match
#' \code{wv_label_speed()}. Could re-visit both.
#'
#' If \code{n_petals} is 8: Assigns direction data in bins of 45 degrees. -22.5
#' to 22.5 degree is North, 22.5 to 67.5 is NE, etc.
#'
#' If \code{n_petals} is 16 (the default): Assigns direction data in bins of
#' 22.5 degrees. -11.25 to 11.25 degree is North, 11.25 to 33.75 is NNE, etc.
#'
#' @param dat Data frame with column from_direction_degree.
#'
#' @param n_petals Number of bins to divide direction data into. Must be either
#'   8 or 16.
#'
#' @returns Returns \code{dat} with an additional column
#'   \code{from_direction_degree_labels}, the direction labels as an
#'   ordered factor.
#'
#' @export


wv_label_direction <- function(dat, n_petals = 16) {

  if(n_petals != 8 & n_petals != 16) {

    warning("n_petals must be 8 or 16, not << ", n_petals, ">>. n_petal will be changed to 16.")

    n_petals <- 16
  }

  if(n_petals == 8) {
    min_break <- 22.5

    dat$from_direction_degree_labels <- cut(
      dat$from_direction_degree,
      breaks = seq(22.5, 382.5, 45),
      labels = c("NE", "E", "SE", "S", "SW", "W", "NW", "N")
    )

    levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  }

  if(n_petals == 16) {
    min_break <- 11.25

    dat$from_direction_degree_labels <- cut(
      dat$from_direction_degree,
      breaks = seq(11.25, 371.25, 22.5),
      labels = c(
        "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
        "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
    )

    levels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
                "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  }

  dat %>%
    mutate(
      # for direction <= 22.5
      from_direction_degree_labels = if_else(
        is.na(from_direction_degree_labels) &
          from_direction_degree >= 0 &
          from_direction_degree <= min_break, "N",
        from_direction_degree_labels
      ),
      from_direction_degree_labels = ordered(
        from_direction_degree_labels, levels = levels)
    )
}
