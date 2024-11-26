#' @title Import wave data from rds files.

#' @param path_input Path to the rds files to be imported. Default is the
#'   wave/processed_data/assembled_data folder on the CMAR R drive (user must be
#'   connected to the Perennia VPN).
#'
#' @param county Vector of character string(s) indicating the county or counties
#'   for which to import data. The filter is applied to the file path, so the
#'   county name MUST be part of the file path (e.g., the name of the file).
#'   Defaults to all counties.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_subset
#' @importFrom purrr map_dfr
#'
#' @export


wv_import_data <- function(path_input = NULL, county = "all") {

  if (is.null(path_input)) {
    # path to Open Data folder
    path_input <- file.path(
      "R:/data_branches/wave/processed_data/assembled/"
    )
  } else {
    path_input <- path_input
  }

  # list rds files on the path and import -----------------------------------
  dat <- list.files(path_input, full.names = TRUE, pattern = ".rds")

  # filter for specified county(ies)
  if (!("all" %in% county)) dat <- str_subset(dat, paste(county, collapse = "|"))

  # read and bind the rds files
  dat %>%
    purrr::map_dfr(readRDS)

}
