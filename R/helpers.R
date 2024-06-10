#' Extract deployment date and station name from file path
#'
#' @param file_path Path to the file, include file name and extension (.csv or
#'   .txt). File name must include the deployment date and the station name,
#'   separated by " - ", e.g., "2008.09.25 - Coffin Island SW - Waves.txt"
#'
#' @return Returns a tibble with two columns: \code{depl_date}, and
#'   \code{station}.
#'
#' @importFrom dplyr %>%  mutate
#' @importFrom lubridate as_date
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#'
#' @export

wv_extract_deployment_info <- function(file_path) {
  sub(".*/", "", file_path, perl = TRUE) %>%
    data.frame() %>%
    separate(col = ".", into = c("depl_date", "station", NA), sep = " - ") %>%
    mutate(
      depl_date = str_replace_all(depl_date, "\\.", "-"),
      depl_date= as_date(depl_date)
    )
}
