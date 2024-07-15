#' Pivot wave data
#'
#' @param dat Data frame of ADCP wave data, as returned from
#'   \code{wv_read_txt()}.
#'
#' @param first_pivot_col Numeric value or character string indicating the first
#'   column to pivot. Will be ignored if \code{vars} is not \code{NULL}.
#'
#' @param last_pivot_col Numeric value or character string indicating the lasst
#'   column to pivot. Default is the last column in \code{dat}. Will be ignored
#'   if \code{vars} is not \code{NULL}.
#'
#' @param vars Vector of character strings indicating which columns to pivot.
#'   Default is first_pivot_col column to the last column.
#'
#' @return Returns data in a long format.
#'
#' @importFrom dplyr last_col
#' @importFrom tidyr pivot_longer
#'
#' @export

wv_pivot_vars_longer <- function(
    dat,
    first_pivot_col = 3,
    last_pivot_col = last_col(),
    vars = NULL) {

  if(is.null(vars)) {
    dat %>%
      pivot_longer(
        cols = first_pivot_col:last_pivot_col,
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

#' Pivot flag columns
#'
#' @param dat Data frame of flagged wave data with the variables in a long
#'   format and flags in wide format.
#'
#' @param qc_test Flag columns to pivot.
#'
#' @return Returns \code{dat} with the qc_test flag columns pivoted to a long
#'   format.
#'
#' @importFrom dplyr %>% contains filter select
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
#'
#' @export

wv_pivot_flags_longer <- function(dat, qc_test = "grossrange") {

  col_name <- paste0(qc_test, "_flag_variable")

  dat %>%
    pivot_longer(
      cols = contains(qc_test),
      names_to = paste0(qc_test, "_flag_variable"),
      names_prefix = paste0(qc_test, "_flag_"),
      values_to = paste0(qc_test, "_flag_value"),
      values_drop_na = TRUE
    ) %>%
    filter(!!sym(col_name) == variable) %>%
    select(-!!sym(col_name))
}
