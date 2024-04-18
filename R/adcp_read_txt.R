#' Read ADCP txt file
#'
#' @description Read raw ADCP txt file into R and format. Label each row with
#'   the appropriate variable name (i.e., "SensorDepth", "WaterSpeed", or
#'   "WaterDirection").
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
#' @return Returns a data frame of the data with a single header row and each
#'   row labelled as "SensorDepth", "WaterSpeed", or "WaterDirection".
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% across case_when everything filter if_else last_col
#'   mutate n select
#' @importFrom glue glue
#' @importFrom lubridate make_datetime force_tz
#' @importFrom stringr str_detect
#'
#' @export


adcp_read_txt <- function(path, file_name = NULL, rm_dups = TRUE) {
  if (!is.null(file_name)) path <- paste0(path, "/", file_name)

  path <- file.path(path)

  if (!str_detect(path, "txt")) {
    stop("File extension not found. \nHINT: Include .txt in path or file_name.")
  }

  # names and order of parameters in txt file (SensorDepth, WaterSpeed, WaterDirection)
  params <- data.table::fread(path, nrows = 3, header = FALSE, select = 8)
  params <- params$V8

  # read in data
  dat_raw <- fread(
    path,
    sep2 = ",", # make separate columns for each cell
    fill = TRUE, # add NA for cells that do not have data
    header = TRUE, # keep header names
    skip = 2, # skip first two rows (duplicate header vals)
    data.table = FALSE # return a dataframe (instead of data.table)
  )

  # one column will be named SensorDepth, WaterSpeed, or WaterDirection. Change to V8
  colnames(dat_raw)[which(colnames(dat_raw) %in% params)] <- paste0(
    "V", which(colnames(dat_raw) %in% params)
  )

  # rows alternate between SensorDepth, WaterSpeed, and WaterDirection
  dat <- dat_raw %>%
    mutate(
      index = 1:n(),
      variable = case_when(
        index %in% seq(1, n(), 3) ~ params[1],
        index %in% seq(2, n(), 3) ~ params[2],
        index %in% seq(3, n(), 3) ~ params[3],
        TRUE ~ NA_character_
      ),
      # do not assign tz= "America/Halifax". This will introduce NA values for the skipped hour of DST
      timestamp_ns = make_datetime(Year, Month, Day, Hour, Min, Sec, tz = "UTC"),
    ) %>%
    select(-index) %>%
    select(timestamp_ns, Num, variable, V8:last_col()) %>%
    # change NaN values to NA
    mutate(across(V8:last_col(), ~ if_else(is.nan(.), NA_real_, .)))

  if (adcp_check_duplicate_timestamp(dat) & rm_dups) {
    dups <- dat %>%
      filter(variable == "SensorDepth")
    dups <- dups[duplicated(dups$timestamp_ns), ]
    dups <- unique(dups$timestamp_ns)

    message(paste(
      length(dups), "duplicate timestamps found and removed:",
      paste(dups, collapse = ", ")
    ))

    dat <- dat %>%
      select(-Num) %>%
      distinct() %>%
      mutate(Num = sort(rep(1:(n() / 3), 3))) %>%
      select(timestamp_ns, Num, variable, everything())
  }

  # QA check ---------------------------------------------------------------

  # SensorDepth rows should only have data in col V8
  check <- dat %>%
    filter(variable == "SensorDepth") %>%
    select(V9:last_col())

  if (!all(is.na(check))) {
    warning("More than one SensorDepth found in row. \nHINT: Check labelling code")
  }

  # Return dat --------------------------------------------------------------

  dat
}
