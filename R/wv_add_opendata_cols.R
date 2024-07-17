#' Add deployment_id, county, waterbody, and station columns
#'
#' @param dat Data frame of wave data.
#'
#' @param metadata Data frame with metadata information for the deployment in
#'   \code{dat} (e.g., a row from the ADCP tracking sheet). Must include columns
#'   \code{depl_id}, \code{county}, \code{waterbody}, and \code{station}. Option
#'   to use default value \code{metadata = NULL} and provide the required values
#'   in the remaining arguments.
#'
#' @param deployment_id Unique ID assigned to each deployment. Not used if
#'   \code{metadata} argument is specified.
#'
#' @param county County in which ADCP was deployed. Not used if \code{metadata}
#'   argument is specified.
#'
#' @param waterbody Waterbody in which ADCP was deployed. Not used if
#'   \code{metadata} argument is specified.
#'
#' @param station Specific area in which ADCP was deployed. Not used if
#'   \code{metadata} argument is specified.
#'
#' @return Returns \code{dat} with columns deployment_id, county, waterbody, and
#'   station.
#'
#' @importFrom dplyr everything mutate select
#'
#' @export

wv_add_opendata_cols <- function(
    dat,
    metadata = NULL,
    deployment_id = NULL,
    county = NULL,
    waterbody = NULL,
    station = NULL) {

  if (!is.null(metadata)) {
    deployment_id <- metadata$depl_id
    county <- metadata$county
    waterbody <- metadata$waterbody
    station <- metadata$station
  }

  dat <- dat %>%
    mutate(
      deployment_id = deployment_id,
      county = county,
      waterbody = waterbody,
      station = station,
    ) %>%
    select(deployment_id:station, everything())
}
