% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wv_write_report_table.R
\name{wv_write_report_table}
\alias{wv_write_report_table}
\title{Writes deployment table for summary report}
\usage{
wv_write_report_table(metadata)
}
\arguments{
\item{metadata}{Deployment metadata from the NSDFA tracking sheet.}
}
\value{
Returns a tibble with three columns: \code{DEPLOYMENT},
  \code{Depl_Date}, and \code{Station_Name}.
}
\description{
Writes deployment table for summary report
}
