#' Writes deployment table for summary report
#'
#' @param metadata Deployment metadata from the ADCP TRACKING "Deployment
#'   Details" sheet.
#'
#' @return Returns a tibble with columns for the report table.
#'
#' @importFrom dplyr if_else mutate select tibble
#'
#' @export

wv_write_report_table <- function(metadata) {
  metadata %>%
    mutate(
      depl_duration = as.numeric(
        difftime(retrieval_date, deployment_date, units = "days")
      )
    ) %>%
    select(
      Station = station,
      `Instrument Model` = sensor_model,
      Latitude = latitude, Longitude = longitude,
      `Deployment Date` = deployment_date, `Recovery Date` = retrieval_date,
      `Duration (d)` = depl_duration,
      `Depth Sounding (m)` = deployment_sounding_m,
      `Ensemble Intervals (s)` = wave_ensemble_interval_s,
      `Averaging Intervals (s)` = wave_averaging_interval_s,
      `Pings per Ensemble` = wave_pings_per_ensemble
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

#' Writes deployment table for summary report from NSDFA tracking
#'
#' @param metadata Deployment metadata from the NSDFA tracking sheet.
#'
#' @return Returns a tibble with columns for the report table.
#'
#' @importFrom dplyr if_else mutate select tibble
#'
#' @export

wv_write_report_table_nsdfa <- function(metadata) {
  metadata %>%
    #tibble() %>%
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
