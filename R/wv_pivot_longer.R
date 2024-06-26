#' Pivot wave data
#'
#' @param dat Data frame of ADCP wave data, as returned from
#'   \code{wv_read_txt()}.
#'
#' @param vars Vector of character strings indicating which columns to pivot.
#'   Default is the second column to the last column.
#'
#' @return Returns data in a long format.
#'
#' @importFrom dplyr last_col
#' @importFrom tidyr pivot_longer
#'
#' @export


wv_pivot_longer <- function(dat, vars = NULL) {

  if(is.null(vars)) {
    dat %>%
      pivot_longer(
        cols = 2:last_col(),
        names_to = "variable", values_to = "value"
      )
  } else {
    dat %>%
      pivot_longer(
        cols = vars,
        names_to = "variable", values_to = "value"
      )

  }

}

