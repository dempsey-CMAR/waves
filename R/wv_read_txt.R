#' Read ADCP wave data txt file
#'
#' @description Read raw ADCP wave data txt file into R and format. Label each
#'   row with the appropriate variable name (i.e., "SensorDepth", "WaterSpeed",
#'   or "WaterDirection").
#'
#' @details The \code{timestamp_ns} column is in the timezone of the deployment
#'   date (e.g., "AST" if deployed in November to March and "DST" if deployed in
#'   March to November). The \code{timestamp_ns} does NOT account for changes in
#'   daylight savings time. Here, the \code{timestamp_ns} is assigned a timezone
#'   of "UTC" to avoid \code{NA} values during the beginning of daylight savings
#'   time (e.g., 2019-03-10 02:30:00 is NOT a valid time for the
#'   "America/Halifax" timezone). This \code{timestamp_ns} can be converted to
#'   true UTC using \code{adcp_correct_timestamp()}.
#'
#'   A warning will be printed if duplicate \code{timestamp_ns} are detected.
#'
#' @param path Path to the txt file (including ".txt" extension) or to the
#'   folder where the txt file is saved.
#'
#' @param file_name Required if \code{path} does not include the file name.
#'   Include the ".txt" file extension. Default is \code{file_name = NULL}.
#'
#' @param rm_dups Logical argument indicating whether to remove duplicate rows.
#'   Default is \code{TRUE}. (Note: the \code{Num} column is removed before
#'   checking for duplicate rows.)
#'
#' @param rm_ns_timestamp Logical argument indicating whether to remove the
#'   original timestamp column. If \code{TRUE}, only the timestamp in UTC will
#'   be returned. Timezone conversion is done using
#'   \code{adcp::adcp_correct_timestamp}.
#'
#' @return Returns a data frame of the data with a single header row and each
#'   row labelled as "SensorDepth", "WaterSpeed", or "WaterDirection".
#'
#' @importFrom adcp adcp_correct_timestamp
#' @importFrom data.table fread
#' @importFrom dplyr %>% across case_when everything filter if_else last_col
#'   mutate n select
#' @importFrom lubridate make_datetime force_tz
#' @importFrom stringr str_detect
#'
#' @export


# if(FALSE) {
#
#   library(here)
#   path <- here("data-raw")
#
#   file_name <- "2022.09.29 - Angus Shoal - Waves.txt"
# }


wv_read_txt <- function(
    path,
    file_name = NULL,
    rm_dups = TRUE,
    rm_ns_timestamp = TRUE
    ) {
  if (!is.null(file_name)) path <- paste0(path, "/", file_name)

  path <- file.path(path)

  if (!str_detect(path, "txt")) {
    stop("File extension not found. \nHINT: Include .txt in path or file_name.")
  }

  # read in data
  dat_raw <- fread(
    path,
    #sep2 = ",", # make separate columns for each cell
    fill = TRUE, # add NA for cells that do not have data
    header = TRUE, # keep header names
    skip = 2, # skip first two rows (duplicate header vals)
    data.table = FALSE # return a dataframe (instead of data.table)
  )

  # rows alternate between SensorDepth, WaterSpeed, and WaterDirection
  dat <- dat_raw %>%
    mutate(
      year = case_when(
        yr >= 90 & nchar(yr) == 2 ~ yr + 1900,
        yr < 90 & nchar(yr) == 2 ~ yr + 2000, # this will have a hard time with single digit years (e.g., with leading 0)
        nchar(yr) == 4 ~ yr
      ),
      # do not assign tz= "America/Halifax". This will introduce NA values for the skipped hour of DST
      timestamp_ns = make_datetime(year, mo, da, hr, mn, sc, tz = "UTC")
    )

  # change NaN values to NA
  # mutate(across(V8:last_col(), ~ if_else(is.nan(.), NA_real_, .)))


# check for duplicate timestamps ------------------------------------------

  dups <- dat[duplicated(dat$timestamp_ns), ]
  dups <- unique(dups$timestamp_ns)

  if( length(dups) > 0 & isTRUE(rm_dups)) {

    message(
      paste(
        length(dups), "duplicate timestamps found and removed:",
        paste(dups, collapse = ", ")
      )
    )
  }


  # Return dat --------------------------------------------------------------

  dat %>%
    select(-c(yr, mo, da, hr, mn, sc, year)) %>%
    adcp_correct_timestamp(rm = rm_ns_timestamp)
}
