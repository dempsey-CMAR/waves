#' Formats tables for summary report
#'
#' @param report_table Table to include in the summary report. Either the
#'   deployment metadata from the NSDFA tracking sheet (as returned from
#'   \code{adcp_write_report_table()}, or the document history) .
#'
#' @param transpose Logical argument indicating whether to transpose
#'   \code{report_table} before applying the format. Use the default \code{TRUE}
#'   for the deployment table. Set to \code{FALSE} for the document history
#'   table.
#'
#' @return Returns a flextable object that will render nicely in the Word
#'   report. A table with two columns: the first column (bold) is the column
#'   names of \code{report_table}; the second column is the corresponding
#'   entries in \code{report_table}.
#'
#' @importFrom dplyr mutate select
#' @importFrom officer fp_border
#' @importFrom  flextable bg delete_part vline border_outer border_inner_h
#'   border_inner_v bold font fontsize autofit fit_to_width
#'
#' @export


wv_format_report_table <- function(report_table, transpose = TRUE) {
  if (isTRUE(transpose)) {
    # transpose table and make the column names the first column
    report_table <- report_table %>%
      t() %>%
      data.frame() %>%
      mutate(Record = colnames(report_table)) %>%
      select(Record, "Deployment Info" = 1)
    rownames(report_table) <- NULL

    report_table <- report_table %>%
      flextable::flextable() %>%
      flextable::delete_part(part = "header") %>%
      flextable::bold(i = NULL, j = 1, bold = TRUE, part = "body")
  } else {
    report_table <- report_table %>%
      flextable::flextable() %>%
      flextable::bold(bold = TRUE, part = "header")
  }

  # set border style
  small_border <- officer::fp_border(color = "gray", width = 1)
  #  grey_rows <- seq(2, length(report_table$body$content$content$keys), 2)
  #  grey_rows <- seq(2, report_table$body$content$content$ncol, 2)

  # format table
  report_table %>%
    # borders
    flextable::vline(border = small_border, part = "all") %>%
    flextable::border_outer(part = "all", border = small_border) %>%
    flextable::border_inner_h(part = "all", border = small_border) %>%
    flextable::border_inner_v(part = "all", border = small_border) %>%
    # fill
    flextable::bg(part = "all", i = c(2, 4, 6, 8, 10), bg = "grey90") %>%
    # flextable::bg(part = "all", i = grey_rows, bg = "grey90") %>%
    # font
    flextable::font(part = "all", fontname = "ebrima") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    # fit
    flextable::autofit() %>%
    flextable::fit_to_width(7.5)
}
