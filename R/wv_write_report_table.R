#' Writes deployment table for summary report
#'
#' @param metadata Deployment metadata from the NSDFA tracking sheet.
#'
#' @return Returns a tibble with three columns: \code{DEPLOYMENT},
#'   \code{Depl_Date}, and \code{Station_Name}.
#'
#' @importFrom dplyr if_else mutate select tibble
#'
#' @export

wv_write_report_table <- function(metadata) {
  metadata %>%
    tibble() %>%
    select(
      Station = Station_Name,
      `Instrument Model` = Inst_Model,
      Latitude = Depl_Lat, Longitude = Depl_Lon,
      `Deployment Date` = Depl_Date, `Recovery Date` = Recv_Date,
      `Duration (d)` = Depl_Duration,
      `Depth Sounding (m)` = Depl_Sounding,
      `Ensemble Intervals (s)` = Waves_Ensemble_Interval_s,
      `Averaging Intervals (s)` = Waves_Averaging_Interval_s,
      `Pings per Ensemble` = Waves_PingsPerEnsemble
    ) %>%
    mutate(
      `Depth Sounding (m)` = as.character(`Depth Sounding (m)`),
      `Depth Sounding (m)` = if_else(
        is.na(`Depth Sounding (m)`), "Not recorded", `Depth Sounding (m)`
      ),

      `Ensemble Intervals (s)` = as.character(`Ensemble Intervals (s)`),
      `Ensemble Intervals (s)` = if_else(
        is.na(`Ensemble Intervals (s)`), "Not recorded", `Ensemble Intervals (s)`
      ),

      `Averaging Intervals (s)` = as.character(`Averaging Intervals (s)`),
      `Averaging Intervals (s)` = if_else(
        is.na(`Averaging Intervals (s)`), "Not recorded", `Averaging Intervals (s)`
      ),

      `Pings per Ensemble` = as.character(`Pings per Ensemble`),
      `Pings per Ensemble` = if_else(
        is.na(`Pings per Ensemble`), "Not recorded", `Pings per Ensemble`
      )
    )
}
