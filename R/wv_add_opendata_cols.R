#' Add deployment_id, waterbody, and station columns
#'
#' @param dat Data frame of ACDP data in long format, as returned by
#'   \code{adcp_pivot_longer()}.
#'
#' @param metadata Data frame with metadata information for the deployment in
#'   \code{dat} (e.g., a row from the NSDFA tracking sheet). Must include
#'   columns \code{Depl_ID}, \code{Waterbody}, and \code{Station}. Option to use
#'   default value \code{metadata = NULL} and provide the required values in the
#'   remaining arguments.
#'
#' @param deployment_id Unique ID assigned to each deployment. Not used if
#'   \code{metadata} argument is specified.
#'
#' @param waterbody Waterbody in which ADCP was deployed. Not used if
#'   \code{metadata} argument is specified.
#'
#' @param station Specific area in which ADCP was deployed. Not used if
#'   \code{metadata} argument is specified.
#'
#' @return Returns \code{dat} with columns deployment_id, waterbody, and
#'   station.
#'
#' @export

adcp_add_opendata_cols <- function(dat,
                                   metadata = NULL,
                                   deployment_id = NULL,
                                   waterbody = NULL,
                                   station = NULL) {
  # if (!is.null(metadata)) {
  #   deployment_id <- metadata$Depl_ID
  #   waterbody <- metadata$Waterbody
  #   station <- metadata$Station_Name
  # }
  #
  # dat <- dat %>%
  #   mutate(
  #     deployment_id = deployment_id,
  #     waterbody = waterbody,
  #     station = station,
  #   ) %>%
  #   select(deployment_id:station, everything())
}
