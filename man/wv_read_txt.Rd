% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wv_read_txt.R
\name{wv_read_txt}
\alias{wv_read_txt}
\title{Read ADCP wave data txt file}
\usage{
wv_read_txt(path, file_name = NULL, rm_dups = TRUE, rm_ns_timestamp = TRUE)
}
\arguments{
\item{path}{Path to the txt file (including ".txt" extension) or to the
folder where the txt file is saved.}

\item{file_name}{Required if \code{path} does not include the file name.
Include the ".txt" file extension. Default is \code{file_name = NULL}.}

\item{rm_dups}{Logical argument indicating whether to remove duplicate rows.
Default is \code{TRUE}.}

\item{rm_ns_timestamp}{Logical argument indicating whether to remove the
original timestamp column. If \code{TRUE}, only the timestamp in UTC will
be returned. Timezone conversion is done using
\code{adcp::adcp_correct_timestamp}.}
}
\value{
Returns a data frame of the data with a single header row and each
  row labelled as "SensorDepth", "WaterSpeed", or "WaterDirection".
}
\description{
Read raw ADCP wave data txt file into R and format. Label each
  row with the appropriate variable name (i.e., "SensorDepth", "WaterSpeed",
  or "WaterDirection").
}
\details{
The \code{timestamp_ns} column is in the timezone of the deployment
  date (e.g., "AST" if deployed in November to March and "DST" if deployed in
  March to November). The \code{timestamp_ns} does NOT account for changes in
  daylight savings time. Here, the \code{timestamp_ns} is assigned a timezone
  of "UTC" to avoid \code{NA} values during the beginning of daylight savings
  time (e.g., 2019-03-10 02:30:00 is NOT a valid time for the
  "America/Halifax" timezone). This \code{timestamp_ns} can be converted to
  true UTC using \code{adcp_correct_timestamp()}.

  A warning will be printed if duplicate \code{timestamp_ns} are detected.

  Rows where yr, mo, da, hr, mn, and sc are all 0 will be filtered out.
}
